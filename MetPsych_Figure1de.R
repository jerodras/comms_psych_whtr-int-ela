working_dir="/Users/rasmussj/Documents/PaperSubmission/MET_PSYCH/Analysis_5_1/"
setwd(working_dir)
rm(list = ls())

library(dplyr)
library(tidyr)
library(lme4)
library(performance)
library('gamm4')
library(ggplot2)
library(pushoverr)

load(file = "dat_cleaned_04262024.RData")

######IN MAIN (FIGURE 1E)

m_s_null <- gamm4(cbcl_scr_syn_internal_r ~ interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                    demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                  random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

m_s_bmi_full <- gamm4(cbcl_scr_syn_internal_r ~ s(bmi) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                        demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                      random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

m_s_whtr_full <- gamm4(cbcl_scr_syn_internal_r ~ s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                         demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                       random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

m_s_whtr_bmi_full <- gamm4(cbcl_scr_syn_internal_r ~ s(bmi) + s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                         demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                       random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

residuals_full <- residuals(m_s_whtr_full$gam)

whtr_effect <- predict(m_s_whtr_full$gam, newdata = dat_cleaned, type = "terms")[,"s(whtr)"]

coefficients <- coef(m_s_whtr_full$gam)
intercept <- coefficients["(Intercept)"]  
adjusted_values <- residuals_full + whtr_effect + intercept

dat_cleaned$adjusted_cbcl_internal = adjusted_values

simdata <- data.frame(
  whtr = seq(min(dat_cleaned$whtr, na.rm = TRUE), max(dat_cleaned$whtr, na.rm = TRUE), length.out = 100),
  interview_age = rep(mean(dat_cleaned$interview_age, na.rm = TRUE), 100),
  reshist_addr1_adi_wsum = rep(mean(dat_cleaned$reshist_addr1_adi_wsum, na.rm = TRUE), 100),
  demo_sex_v2 = rep(dat_cleaned$demo_sex_v2[1], 100),
  demo_ethn_v2 = rep(dat_cleaned$demo_ethn_v2[1], 100),
  demo_max_ed_v2 = rep(dat_cleaned$demo_max_ed_v2[1], 100),
  demo_max_income_v2 = rep(dat_cleaned$demo_max_income_v2[1], 100),
  pds_p_ss_category_2 = rep(mean(dat_cleaned$pds_p_ss_category_2, na.rm = TRUE), 100),
  race_cleaned = rep(dat_cleaned$race_cleaned[1], 100)
)

#De-mean to match residual effect for plotting
simdata$predicted <- predict(m_s_whtr_full$gam, simdata, type = "response")

# Create the plot
plot <- ggplot() +
  # Adding the scatter plot of observed values
  geom_point(data = dat_cleaned, aes(x = whtr, y = adjusted_cbcl_internal), 
             colour = "gray", alpha = 0.4) +
  # Adding the line for predicted values
  geom_line(data = simdata, aes(x = whtr, y = predicted), 
            colour = "red", linewidth = 2.0) +
  labs(x = "Waist-to-Height Ratio (WHtR)", y = "Adjusted Internalizing Symptoms",
       title = "Adjusted Effects of WHtR on Internalizing Symptoms") +
  theme_minimal()

# Print the plot
print(plot)

####Baseline#############################
dat_bas <- dat_cleaned %>%
  filter(eventname == "baseline_year_1_arm_1")

m_s_bmi_null_bas <- gamm4(cbcl_scr_syn_internal_r ~ interview_age + s(reshist_addr1_adi_wsum) + demo_max_ed_v2 + demo_max_income_v2 +
                           demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                         random = ~(1|site_id_l) + (1|rel_family_id), data = dat_bas, REML = TRUE)
  
m_s_bmi_full_bas <- gamm4(cbcl_scr_syn_internal_r ~ s(bmi) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                        demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                      random = ~(1|site_id_l) + (1|rel_family_id), data = dat_bas, REML = TRUE)

m_s_whtr_full_bas <- gamm4(cbcl_scr_syn_internal_r ~ s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                         demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                       random = ~(1|site_id_l) + (1|rel_family_id), data = dat_bas, REML = TRUE)

m_s_whtr_bmi_full_bas <- gamm4(cbcl_scr_syn_internal_r ~ s(bmi) + s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                             demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                           random = ~(1|site_id_l) + (1|rel_family_id), data = dat_bas, REML = TRUE)
#############################
####1Y#############################
dat_1y <- dat_cleaned %>%
  filter(eventname == "1_year_follow_up_y_arm_1")

m_s_bmi_null_1y <- gamm4(cbcl_scr_syn_internal_r ~ interview_age + s(reshist_addr1_adi_wsum) + demo_max_ed_v2 + demo_max_income_v2 +
                           demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                         random = ~(1|site_id_l) + (1|rel_family_id), data = dat_1y, REML = TRUE)

m_s_bmi_full_1y <- gamm4(cbcl_scr_syn_internal_r ~ s(bmi) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                            demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                          random = ~(1|site_id_l) + (1|rel_family_id), data = dat_1y, REML = TRUE)

m_s_whtr_full_1y <- gamm4(cbcl_scr_syn_internal_r ~ s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                             demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                           random = ~(1|site_id_l) + (1|rel_family_id), data = dat_1y, REML = TRUE)

m_s_whtr_bmi_full_1y <- gamm4(cbcl_scr_syn_internal_r ~ s(bmi) + s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                                 demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                               random = ~(1|site_id_l) + (1|rel_family_id), data = dat_1y, REML = TRUE)
#############################
####2Y#############################
dat_2y <- dat_cleaned %>%
  filter(eventname == "2_year_follow_up_y_arm_1")

m_s_bmi_null_2y <- gamm4(cbcl_scr_syn_internal_r ~ interview_age + s(reshist_addr1_adi_wsum) + demo_max_ed_v2 + demo_max_income_v2 +
                           demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                         random = ~(1|site_id_l) + (1|rel_family_id), data = dat_2y, REML = TRUE)

m_s_bmi_full_2y <- gamm4(cbcl_scr_syn_internal_r ~ s(bmi) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                           demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                         random = ~(1|site_id_l) + (1|rel_family_id), data = dat_2y, REML = TRUE)

m_s_whtr_full_2y <- gamm4(cbcl_scr_syn_internal_r ~ s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                            demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                          random = ~(1|site_id_l) + (1|rel_family_id), data = dat_2y, REML = TRUE)

m_s_whtr_bmi_full_2y <- gamm4(cbcl_scr_syn_internal_r ~ s(bmi) + s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                                demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                              random = ~(1|site_id_l) + (1|rel_family_id), data = dat_2y, REML = TRUE)
#############################
####3Y#############################
dat_3y <- dat_cleaned %>%
  filter(eventname == "3_year_follow_up_y_arm_1")

m_s_bmi_null_3y <- gamm4(cbcl_scr_syn_internal_r ~ interview_age + s(reshist_addr1_adi_wsum) + demo_max_ed_v2 + demo_max_income_v2 +
                           demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                         random = ~(1|site_id_l) + (1|rel_family_id), data = dat_3y, REML = TRUE)

m_s_bmi_full_3y <- gamm4(cbcl_scr_syn_internal_r ~ s(bmi) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                           demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                         random = ~(1|site_id_l) + (1|rel_family_id), data = dat_3y, REML = TRUE)

m_s_whtr_full_3y <- gamm4(cbcl_scr_syn_internal_r ~ s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                            demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                          random = ~(1|site_id_l) + (1|rel_family_id), data = dat_3y, REML = TRUE)

m_s_whtr_bmi_full_3y <- gamm4(cbcl_scr_syn_internal_r ~ s(bmi) + s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                                demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                              random = ~(1|site_id_l) + (1|rel_family_id), data = dat_3y, REML = TRUE)
#############################
###4Y#############################
dat_4y <- dat_cleaned %>%
  filter(eventname == "4_year_follow_up_y_arm_1")

m_s_bmi_null_4y <- gamm4(cbcl_scr_syn_internal_r ~ interview_age + s(reshist_addr1_adi_wsum) + demo_max_ed_v2 + demo_max_income_v2 +
                           demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                         random = ~(1|site_id_l) + (1|rel_family_id), data = dat_4y, REML = TRUE)

m_s_bmi_full_4y <- gamm4(cbcl_scr_syn_internal_r ~ s(bmi) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                           demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                         random = ~(1|site_id_l) + (1|rel_family_id), data = dat_4y, REML = TRUE)

m_s_whtr_full_4y <- gamm4(cbcl_scr_syn_internal_r ~ s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                            demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                          random = ~(1|site_id_l) + (1|rel_family_id), data = dat_4y, REML = TRUE)

m_s_whtr_bmi_full_4y <- gamm4(cbcl_scr_syn_internal_r ~ s(bmi) + s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                                demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                              random = ~(1|site_id_l) + (1|rel_family_id), data = dat_4y, REML = TRUE)
#############################
F_bmi_bas=summary(m_s_bmi_full_bas$gam)$s.table["s(bmi)", "F"]
F_bmi_1y=summary(m_s_bmi_full_1y$gam)$s.table["s(bmi)", "F"]
F_bmi_2y=summary(m_s_bmi_full_2y$gam)$s.table["s(bmi)", "F"]
F_bmi_3y=summary(m_s_bmi_full_3y$gam)$s.table["s(bmi)", "F"]
F_bmi_4y=summary(m_s_bmi_full_4y$gam)$s.table["s(bmi)", "F"]

F_whtr_bas=summary(m_s_whtr_full_bas$gam)$s.table["s(whtr)", "F"]
F_whtr_1y=summary(m_s_whtr_full_1y$gam)$s.table["s(whtr)", "F"]
F_whtr_2y=summary(m_s_whtr_full_2y$gam)$s.table["s(whtr)", "F"]
F_whtr_3y=summary(m_s_whtr_full_3y$gam)$s.table["s(whtr)", "F"]
F_whtr_4y=summary(m_s_whtr_full_4y$gam)$s.table["s(whtr)", "F"]

F_bmi_cwhtr_bas=summary(m_s_whtr_bmi_full_bas$gam)$s.table["s(bmi)", "F"]
F_bmi_cwhtr_1y=summary(m_s_whtr_bmi_full_1y$gam)$s.table["s(bmi)", "F"]
F_bmi_cwhtr_2y=summary(m_s_whtr_bmi_full_2y$gam)$s.table["s(bmi)", "F"]
F_bmi_cwhtr_3y=summary(m_s_whtr_bmi_full_3y$gam)$s.table["s(bmi)", "F"]
F_bmi_cwhtr_4y=summary(m_s_whtr_bmi_full_4y$gam)$s.table["s(bmi)", "F"]
F_whtr_cbmi_bas=summary(m_s_whtr_bmi_full_bas$gam)$s.table["s(whtr)", "F"]
F_whtr_cbmi_1y=summary(m_s_whtr_bmi_full_1y$gam)$s.table["s(whtr)", "F"]
F_whtr_cbmi_2y=summary(m_s_whtr_bmi_full_2y$gam)$s.table["s(whtr)", "F"]
F_whtr_cbmi_3y=summary(m_s_whtr_bmi_full_3y$gam)$s.table["s(whtr)", "F"]
F_whtr_cbmi_4y=summary(m_s_whtr_bmi_full_4y$gam)$s.table["s(whtr)", "F"]

# Create the data frame
Fscore_data_full <- data.frame(
  Visit = c("Baseline", "1y", "2y", "3y", "4y",
            "Baseline", "1y", "2y", "3y", "4y",
            "Baseline", "1y", "2y", "3y", "4y",
            "Baseline", "1y", "2y", "3y", "4y"),
  Model = c(rep("BMI", 5), rep("WHtR", 5), rep("BMI | WHtR", 5), rep("WHtR | BMI", 5)),
  Fscore = c(F_bmi_bas, F_bmi_1y, F_bmi_2y, F_bmi_3y, F_bmi_4y,
             F_whtr_bas, F_whtr_1y, F_whtr_2y, F_whtr_3y, F_whtr_4y,
             F_bmi_cwhtr_bas, F_bmi_cwhtr_1y, F_bmi_cwhtr_2y, F_bmi_cwhtr_3y, F_bmi_cwhtr_4y,
             F_whtr_cbmi_bas, F_whtr_cbmi_1y, F_whtr_cbmi_2y, F_whtr_cbmi_3y, F_whtr_cbmi_4y)
)

# Create the line plot with gray shades and different line patterns
ggplot(Fscore_data_full, aes(x = Visit, y = Fscore, group = Model, color = Model, linetype = Model)) +
  geom_line(linewidth = 1.2) +  # Use a slightly thicker line for visibility
  geom_point(size = 3, shape = 21, fill = "white") +  # Add points with white fill
  scale_x_discrete(limits = c("Baseline", "1y", "2y", "3y", "4y")) +
  labs(x = "Visit", y = "F-Score",
       title = "Comparison of R-squared Values Across Different Models and Visits",
       subtitle = "Models: BMI-Null, and WHtR-Null") +
  theme_minimal() +
  scale_color_manual(values = c("BMI" = "gray50", "WHtR" = "gray10", "BMI | WHtR" = "gray50", "WHtR | BMI" = "gray10")) +  # Set shades of gray
  scale_linetype_manual(values = c("BMI" = "solid", "WHtR" = "solid", "BMI | WHtR" = "dashed", "WHtR | BMI" = "dashed"))  # Set line types

#############################
Rsqnull_bas=summary(m_s_bmi_null_bas$gam)$r.sq
Rsqnull_1y=summary(m_s_bmi_null_1y$gam)$r.sq
Rsqnull_2y=summary(m_s_bmi_null_2y$gam)$r.sq
Rsqnull_3y=summary(m_s_bmi_null_3y$gam)$r.sq
Rsqnull_4y=summary(m_s_bmi_null_4y$gam)$r.sq

Rsqbmi_bas=summary(m_s_bmi_full_bas$gam)$r.sq
Rsqbmi_1y=summary(m_s_bmi_full_1y$gam)$r.sq
Rsqbmi_2y=summary(m_s_bmi_full_2y$gam)$r.sq
Rsqbmi_3y=summary(m_s_bmi_full_3y$gam)$r.sq
Rsqbmi_4y=summary(m_s_bmi_full_4y$gam)$r.sq

Rsqwhtr_bas=summary(m_s_whtr_full_bas$gam)$r.sq
Rsqwhtr_1y=summary(m_s_whtr_full_1y$gam)$r.sq
Rsqwhtr_2y=summary(m_s_whtr_full_2y$gam)$r.sq
Rsqwhtr_3y=summary(m_s_whtr_full_3y$gam)$r.sq
Rsqwhtr_4y=summary(m_s_whtr_full_4y$gam)$r.sq

Rsqbmi_cwhtr_bas=summary(m_s_whtr_bmi_full_bas$gam)$r.sq
Rsqbmi_cwhtr_1y=summary(m_s_whtr_bmi_full_1y$gam)$r.sq
Rsqbmi_cwhtr_2y=summary(m_s_whtr_bmi_full_2y$gam)$r.sq
Rsqbmi_cwhtr_3y=summary(m_s_whtr_bmi_full_3y$gam)$r.sq
Rsqbmi_cwhtr_4y=summary(m_s_whtr_bmi_full_4y$gam)$r.sq
Rsqwhtr_cbmi_bas=summary(m_s_whtr_bmi_full_bas$gam)$r.sq
Rsqwhtr_cbmi_1y=summary(m_s_whtr_bmi_full_1y$gam)$r.sq
Rsqwhtr_cbmi_2y=summary(m_s_whtr_bmi_full_2y$gam)$r.sq
Rsqwhtr_cbmi_3y=summary(m_s_whtr_bmi_full_3y$gam)$r.sq
Rsqwhtr_cbmi_4y=summary(m_s_whtr_bmi_full_4y$gam)$r.sq

# Create the data frame
Rsqscore_data_full <- data.frame(
  Visit = c("Baseline", "1y", "2y", "3y", "4y",
            "Baseline", "1y", "2y", "3y", "4y",
            "Baseline", "1y", "2y", "3y", "4y",
            "Baseline", "1y", "2y", "3y", "4y"),
  Model = c(rep("BMI", 5), rep("WHtR", 5), rep("Null", 5), rep("WHtR & BMI", 5)),
  Rsqscore = c(Rsqbmi_bas, Rsqbmi_1y, Rsqbmi_2y, Rsqbmi_3y, Rsqbmi_4y,
             Rsqwhtr_bas, Rsqwhtr_1y, Rsqwhtr_2y, Rsqwhtr_3y, Rsqwhtr_4y,
             Rsqnull_bas, Rsqnull_1y, Rsqnull_2y, Rsqnull_3y, Rsqnull_4y,
             Rsqwhtr_cbmi_bas, Rsqwhtr_cbmi_1y, Rsqwhtr_cbmi_2y, Rsqwhtr_cbmi_3y, Rsqwhtr_cbmi_4y)
)

# Create the line plot with gray shades and different line patterns
ggplot(Rsqscore_data_full, aes(x = Visit, y = Rsqscore, group = Model, color = Model, linetype = Model)) +
  geom_line(linewidth = 1.2) +  # Use a slightly thicker line for visibility
  geom_point(size = 3, shape = 21, fill = "white") +  # Add points with white fill
  scale_x_discrete(limits = c("Baseline", "1y", "2y", "3y", "4y")) +
  labs(x = "Visit", y = "Rs=q",
       title = "Comparison of R-squared Values Across Different Models and Visits",
       subtitle = "Models: BMI-Null, and WHtR-Null") +
  theme_minimal() +
  scale_color_manual(values = c("BMI" = "gray50", "WHtR" = "gray10", "Null" = "gray50", "WHtR & BMI" = "gray10")) +  # Set shades of gray
  scale_linetype_manual(values = c("BMI" = "solid", "WHtR" = "solid", "Null" = "dashed", "WHtR & BMI" = "dashed"))  # Set line types


#############################
BICbmi_bas=BIC(m_s_bmi_full_bas$mer)
BICbmi_1y=BIC(m_s_bmi_full_1y$mer)
BICbmi_2y=BIC(m_s_bmi_full_2y$mer)
BICbmi_3y=BIC(m_s_bmi_full_3y$mer)
BICbmi_4y=BIC(m_s_bmi_full_4y$mer)

BICwhtr_bas=BIC(m_s_whtr_full_bas$mer)
BICwhtr_1y=BIC(m_s_whtr_full_1y$mer)
BICwhtr_2y=BIC(m_s_whtr_full_2y$mer)
BICwhtr_3y=BIC(m_s_whtr_full_3y$mer)
BICwhtr_4y=BIC(m_s_whtr_full_4y$mer)

BIC_cwhtr_bas=BIC(m_s_whtr_bmi_full_bas$mer)
BIC_cwhtr_1y=BIC(m_s_whtr_bmi_full_1y$mer)
BIC_cwhtr_2y=BIC(m_s_whtr_bmi_full_2y$mer)
BIC_cwhtr_3y=BIC(m_s_whtr_bmi_full_3y$mer)
BIC_cwhtr_4y=BIC(m_s_whtr_bmi_full_4y$mer)

# Create the data frame
BICscore_data_full <- data.frame(
  Visit = c("Baseline", "1y", "2y", "3y", "4y",
            "Baseline", "1y", "2y", "3y", "4y",
            "Baseline", "1y", "2y", "3y", "4y"),
  Model = c(rep("BMI", 5), rep("WHtR", 5), rep("WHtR & BMI", 5)),
  BICscore = c(BICbmi_bas, BICbmi_1y, BICbmi_2y, BICbmi_3y, BICbmi_4y,
               BICwhtr_bas, BICwhtr_1y, BICwhtr_2y, BICwhtr_3y, BICwhtr_4y,
               BIC_cwhtr_bas, BIC_cwhtr_1y, BIC_cwhtr_2y, BIC_cwhtr_3y, BIC_cwhtr_4y)
)

# Create the line plot with gray shades and different line patterns
ggplot(BICscore_data_full, aes(x = Visit, y = BICscore, group = Model, color = Model, linetype = Model)) +
  geom_line(linewidth = 1.2) +  # Use a slightly thicker line for visibility
  geom_point(size = 3, shape = 21, fill = "white") +  # Add points with white fill
  scale_x_discrete(limits = c("Baseline", "1y", "2y", "3y", "4y")) +
  labs(x = "Visit", y = "F-Score",
       title = "Comparison of R-squared Values Across Different Models and Visits",
       subtitle = "Models: BMI-Null, and WHtR-Null") +
  theme_minimal() +
  scale_color_manual(values = c("BMI" = "gray50", "WHtR" = "gray10",  "WHtR | BMI" = "gray10")) +  # Set shades of gray
  scale_linetype_manual(values = c("BMI" = "solid", "WHtR" = "solid",  "WHtR & BMI" = "dashed"))  # Set line types

#############################
BICbmi_bas_diff=BIC(m_s_bmi_full_bas$mer)-BIC(m_s_whtr_full_bas$mer)
BICbmi_1y_diff=BIC(m_s_bmi_full_1y$mer)-BIC(m_s_whtr_full_1y$mer)
BICbmi_2y_diff=BIC(m_s_bmi_full_2y$mer)-BIC(m_s_whtr_full_2y$mer)
BICbmi_3y_diff=BIC(m_s_bmi_full_3y$mer)-BIC(m_s_whtr_full_3y$mer)
BICbmi_4y_diff=BIC(m_s_bmi_full_4y$mer)-BIC(m_s_whtr_full_4y$mer)

BIC_cwhtr_bas_diff=BIC(m_s_whtr_bmi_full_bas$mer)-BIC(m_s_whtr_full_bas$mer)
BIC_cwhtr_1y_diff=BIC(m_s_whtr_bmi_full_1y$mer)-BIC(m_s_whtr_full_1y$mer)
BIC_cwhtr_2y_diff=BIC(m_s_whtr_bmi_full_2y$mer)-BIC(m_s_whtr_full_2y$mer)
BIC_cwhtr_3y_diff=BIC(m_s_whtr_bmi_full_3y$mer)-BIC(m_s_whtr_full_3y$mer)
BIC_cwhtr_4y_diff=BIC(m_s_whtr_bmi_full_4y$mer)-BIC(m_s_whtr_full_4y$mer)

# Create the data frame
BICscore_diff_data_full <- data.frame(
  Visit = c("Baseline", "1y", "2y", "3y", "4y",
            "Baseline", "1y", "2y", "3y", "4y"),
  Model = c(rep("BMI", 5), rep("WHtR & BMI", 5)),
  BICscore_diff = c(BICbmi_bas_diff, BICbmi_1y_diff, BICbmi_2y_diff, BICbmi_3y_diff, BICbmi_4y_diff,
               BIC_cwhtr_bas_diff, BIC_cwhtr_1y_diff, BIC_cwhtr_2y_diff, BIC_cwhtr_3y_diff, BIC_cwhtr_4y_diff)
)

# Create the line plot with gray shades and different line patterns
ggplot(BICscore_diff_data_full, aes(x = Visit, y = BICscore_diff, group = Model, color = Model, linetype = Model)) +
  geom_line(linewidth = 1.2) +  # Use a slightly thicker line for visibility
  geom_point(size = 3, shape = 21, fill = "white") +  # Add points with white fill
  scale_x_discrete(limits = c("Baseline", "1y", "2y", "3y", "4y")) +
  labs(x = "Visit", y = "BIC Diff.",
       title = "Comparison of R-squared Values Across Different Models and Visits",
       subtitle = "Models: BMI-Null, and WHtR-Null") +
  ylim(-25, 25) +  # Set y-axis limits
  theme_minimal() +
  scale_color_manual(values = c("BMI" = "gray50", "WHtR | BMI" = "gray10")) +  # Set shades of gray
  scale_linetype_manual(values = c("BMI" = "solid", "WHtR & BMI" = "dashed"))  # Set line types

