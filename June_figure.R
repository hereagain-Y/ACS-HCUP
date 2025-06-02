# updated with adjusted table 
library(survey)

library(knitr)
library(dplyr)
library(broom)

library(tableone)
library(writexl)
library(flextable)
library(ggplot2)
library(scales)
load("/Users/hereagain/Desktop/ACS/hcup_apr21/Apr21_cancer_data.RData")

# table1 with adjusted survey weight:

kids_blood_cancer
vars<-c('age_group','gender','factor_race','PAY1_factor','income_group','treatment','dispostion_group','LOS','DIED','TOTCHG','total_cost','prs_type','HOSP_BEDSIZE',
        'HOSP_LOCTEACH','HOSP_REGION_factor','H_CONTRL')

table1 <- CreateTableOne(vars = vars, data = kids_blood_cancer,test=TRUE)

# Print the table to view it in the console
print(table1, showAllLevels = TRUE,nonnormal = c('LOS','TOTCHG','total_cost'),
      exact=c('dispostion_group'),
      test=FALSE)

table_df <-as.data.frame(print(table1, showAllLevels = TRUE,
                               test=TRUE))
table_df<-cbind(Variable=rownames(table_df),table_df)
#write_xlsx(table_df, path = "/Users/danwei.yao/Library/CloudStorage/OneDrive-AmericanCancerSociety/Desktop/Mar_20_table1.xlsx")



options(survey.lonely.psu = "adjust")
kid_design <- svydesign(id=~HOSP_KID,
                        
                        weights = ~DISCWT,
                        data = kids_blood_cancer,
                        nest=TRUE)


table2 <- svyCreateTableOne(vars = vars, data = kid_design, test = TRUE)
sum(kids_blood_clean$DISCWT) 
mean(kids_blood_clean$DISCWT)
sum(weights(kid_design))
# Print the table
print(table2, showAllLevels = TRUE)


table_df2 <-as.data.frame(print(table2, showAllLevels = TRUE,
                                test=TRUE))

table_df2<-cbind(Variable=rownames(table_df2),table_df2)

write_xlsx(table_df2, path = "/Users/hereagain/Desktop/hcup_apr21/Apr_22_survey_table1.xlsx")

# LOS and treatment
svyby(~LOS, ~treatment, kid_design, svymean, na.rm = TRUE)

# Weighted mean cost by treatment group
svyby(~total_cost, ~treatment, kid_design, svymean, na.rm = TRUE)


# Weighted median
svyby(~total_cost, ~treatment, kid_design, svyquantile, quantiles = 0.5, na.rm = TRUE)
cost_summary <- svyby(~total_cost, ~treatment, kid_design, svymean, na.rm = TRUE, keep.names = FALSE) %>%
  as.data.frame()


anova_model <- svyglm(total_cost ~ treatment, design = kid_design)
anova_p<-summary(anova_model)$coefficients[2,4]
p_label <- ifelse(anova_p < 0.001, "p < 0.001", paste0("p = ", signif(anova_p, 3)))
los_summary <- svyby(~LOS, ~treatment, kid_design, svymean, na.rm = TRUE) %>% as.data.frame()
cost_summary<- svyby(~total_cost, ~treatment, kid_design, svymean, na.rm = TRUE) %>% as.data.frame()
install.packages('srvyr')
library(srvyr)



# boxplot
ggplot(kids_blood_cancer, aes(x = treatment, y = total_cost, weight = DISCWT)) +
  geom_boxplot(
    aes(fill = treatment),
    outlier.color = "black", 
    outlier.shape = 21, 
    outlier.size = 2,
    alpha = 0.9
  ) +
  scale_y_log10(labels = scales::dollar_format(scale = 1)) +
  scale_fill_brewer(palette = "Set2") +
  geom_point(data = cost_summary, aes(x = treatment, y = total_cost), 
             size = 3, shape = 21, fill = "white", stroke = 1.2, inherit.aes = FALSE) +
  labs(
    title = "Survey-Weighted Total Hospital Cost by Treatment Group",
    x = "Treatment Group",
    y = "Total Hospital Cost (USD)"
  ) +
  annotate("text", x = 1.5, y = max(kids_blood_cancer$total_cost, na.rm = TRUE), 
           label = p_label, size = 5, fontface = "italic") +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 13),
    axis.text.y = element_text(size = 12),
    panel.grid = element_blank(),         # No grid lines
    axis.line = element_line(color = "black"),
    legend.position = "none"
  )

anova_model2 <- svyglm(LOS ~ treatment, design = kid_design)
anova_p2<-summary(anova_model2)$coefficients[2,4]
p_label2 <- ifelse(anova_p2 < 0.001, "p < 0.001", paste0("p = ", signif(anova_p2, 3)))
ggplot(kids_blood_cancer, aes(x = treatment, y = LOS, weight = DISCWT)) +
  geom_boxplot(
    aes(fill = treatment),
    outlier.color = "black", 
    outlier.shape = 21, 
    outlier.size = 2,
    alpha = 0.9
  ) +
  geom_point(data = los_summary, aes(x = treatment, y = LOS), 
             size = 3, shape = 21, fill = "white", stroke = 1.2, inherit.aes = FALSE) +
  
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Survey-Weighted Total Length of Stay by Treatment Group",
    x = "Treatment Group",
    y = "Days"
  ) +
  annotate("text", x = 1.5, y = max(100, na.rm = TRUE), 
           label = p_label2, size = 5, fontface = "italic") +
  theme_classic(base_size = 16) +
  ylim(0,100)+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 13),
    axis.text.y = element_text(size = 12),
    panel.grid = element_blank(),         # No grid lines
    axis.line = element_line(color = "black"),
    legend.position = "none"
  )


library(srvyr)
kid_svy <- kids_blood_cancer %>% 
  as_survey_design(weights = DISCWT)

summary_stats <- kid_svy %>% 
  group_by(treatment) %>% 
  summarise(
    mean   = survey_mean(total_cost,  na.rm = TRUE)[["total_cost"]],
    q1     = survey_quantile(total_cost, quantiles = 0.25, na.rm = TRUE)[[1]],
    median = survey_quantile(total_cost, quantiles = 0.50, na.rm = TRUE)[[1]],
    q3     = survey_quantile(total_cost, quantiles = 0.75, na.rm = TRUE)[[1]]
  ) %>% 
  mutate(IQR = q3 - q1)
summary_stats

summary_stats2 <- kid_svy %>% 
  group_by(treatment) %>% 
  summarise(
    mean   = survey_mean(LOS,  na.rm = TRUE)[["total_cost"]],
    q1     = survey_quantile(LOS, quantiles = 0.25, na.rm = TRUE)[[1]],
    median = survey_quantile(LOS, quantiles = 0.50, na.rm = TRUE)[[1]],
    q3     = survey_quantile(LOS, quantiles = 0.75, na.rm = TRUE)[[1]]
  ) %>% 
  mutate(IQR = q3 - q1)
summary_stats2

library(writexl)
write_xlsx(summary_stats, "/Users/hereagain/Desktop/ACS/hcup_apr21/boxplotsummary_stats.xlsx")
write_xlsx(summary_stats2, "/Users/hereagain/Desktop/ACS/hcup_apr21/boxplotsummary_stats2.xlsx")

