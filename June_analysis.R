# final code for the analysis of the cancer data
library(survey)

library(knitr)
library(dplyr)
library(broom)

load("/Users/hereagain/Desktop/hcup_apr21/Kids_finalized_mar20.Rdata")
dim(kids_blood_clean)
kids_blood_clean$treatment
kids_blood_clean$new_prs_type

levels(kids_blood_clean$treatment) <- c(levels(kids_blood_clean$treatment), "Transplant w/o Chemo")

# Then perform the conditional replacement
kids_blood_clean$treatment <- ifelse(
  (kids_blood_clean$treatment == "Others" & kids_blood_clean$new_prs_type == "stem_cell_trans"),
  "Transplant w/o Chemo",
  as.character(kids_blood_clean$treatment) 
)

kids_blood_clean$treatment <- factor(kids_blood_clean$treatment)
table(kids_blood_clean$treatment)

library(ggplot2)
library(forcats)
kids_blood_cancer <- kids_blood_clean %>%
  dplyr::mutate(treatment = dplyr::recode(treatment,
                            "Chemo + Transplant" = "Transplant w/ Chemo"),
         treatment = factor(treatment, levels = c("Chemo only", "Transplant w/ Chemo", "Transplant w/o Chemo", "Others")))
kids_blood_cancer$age_group = factor(kids_blood_cancer$age_group, levels = c("15-17.9", "0-4","5-9","10-14"))

ggplot(kids_blood_cancer, aes(x = treatment, y = total_cost)) +
  geom_boxplot(fill = "#4F81BD", outlier.color = "red", outlier.shape = 16) +
  scale_y_log10(labels = scales::dollar_format(scale = 1)) +
  labs(
    title = "Cost Distribution by Treatment Group",
    x = "Treatment Group",
    y = "Cost (log scale, USD)"
  ) +
  theme_classic(base_size = 14) +  # Classic theme removes background and grid
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

# plot the LOS 
kids_blood_cancer$LOS
ggplot(kids_blood_cancer, aes(x = treatment, y = LOS)) +
  geom_boxplot(fill = "#4F81BD", outlier.color = "red", outlier.shape = 16) +
  labs(
    title = "Total Length of Stay by Treatment Group",
    x = "Treatment Group",
    y = "Days"
  ) +
  theme_classic(base_size = 14) +  # Classic theme removes background and grid
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )+
  ylim(0,150)


# adjusted analysis 

kids_blood_cancer=kids_blood_cancer %>%
  dplyr::filter(PAY1_factor != "Medicare") # Recode "No charge" to "Others"%>%
kids_blood_cancer$new_insurance<-ifelse(kids_blood_cancer$PAY1_factor == "No charge", "Others",kids_blood_cancer$PAY1_factor)



options(survey.lonely.psu = "adjust")
kid_design <- svydesign(id=~HOSP_KID,
                        strata =~KID_STRATUM,
                        weights = ~DISCWT,
                        data = kids_blood_cancer,
                        nest=TRUE)

library(MASS)
glm_nb <- glm.nb(LOS~age_group+gender+PAY1_factor+treatment+HOSP_BEDSIZE+HOSP_REGION_factor,
                 data=kids_blood_cancer)

theta_estimate<-glm_nb$theta
print(theta_estimate)
glm_nb_weight <- svyglm(LOS~age_group+gender+PAY1_factor+treatment+HOSP_BEDSIZE+HOSP_REGION_factor,
                        design = kid_design,
                        family = negative.binomial(theta=theta_estimate))
summary(glm_nb_weight)
glm_results <- summary(glm_nb_weight)

glm_table <- broom::tidy(glm_nb_weight, conf.int = TRUE, exponentiate = TRUE)

glm_table %>%
  dplyr::select(term, estimate, conf.low, conf.high, p.value) %>%
  dplyr::rename(
    Variable = term,
    `exp(Beta)` = estimate,
    `95% CI Lower` = conf.low,
    `95% CI Upper` = conf.high,
    `P-value` = p.value
  )
kable(glm_table, digits = 3, caption = "Exponentiated Coefficients with 95% CI and P-values")
# -==== Model 2 Total Cost============

log_model2 <- svyglm(log_cost ~ age_group+gender+PAY1_factor+treatment+HOSP_BEDSIZE+HOSP_REGION_factor,
                     design = kid_design,
                     family = gaussian())  
lognormal_table <- broom::tidy(log_model2, conf.int = TRUE) %>%
  dplyr::mutate(
    exp_beta = exp(estimate),     
    conf.low = exp(conf.low),     
    conf.high = exp(conf.high)    
  ) %>%
  dplyr::select(term, exp_beta, conf.low, conf.high, p.value) %>%
  dplyr::rename(
    Variable = term,
    `exp(Beta)` = exp_beta,
    `95% CI Lower` = conf.low,
    `95% CI Upper` = conf.high,
    `P-value` = p.value
  )

save(kids_blood_cancer, file = "/Users/hereagain/Desktop/hcup_apr21/Apr21_cancer_data.RData")

