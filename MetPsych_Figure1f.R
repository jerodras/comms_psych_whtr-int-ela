working_dir="/Users/rasmussj/Documents/PaperSubmission/MET_PSYCH/Analysis_5_1/"
setwd(working_dir)
rm(list = ls())

library(dplyr)
library(performance)
library(ggplot2)

load(file = "dat_cleaned_04262024.RData")

dat_filtered <- dat_cleaned %>%
  group_by(rel_family_id, eventname) %>%
  filter(n() == 2) %>%
  ungroup()

# Assuming dat_filtered is already grouped and filtered as needed
# First, ensure the data is ordered as needed within each group
dat_filtered <- dat_filtered %>%
  arrange(rel_family_id, eventname, src_subject_id)

# Now, split the data into two data frames
dat_filtered_1 <- dat_filtered %>%
  group_by(rel_family_id, eventname) %>%
  slice(1) %>%
  ungroup()

dat_filtered_2 <- dat_filtered %>%
  group_by(rel_family_id, eventname) %>%
  slice(2) %>%
  ungroup()

dat_diff <- inner_join(dat_filtered_1, dat_filtered_2, by = c("rel_family_id", "eventname"), suffix = c(".1", ".2"))

# Calculate the differences for the relevant variables
dat_diff <- dat_diff %>%
  mutate(
    diff_whtr = whtr.1 - whtr.2,
    diff_bmi = bmi.1 - bmi.2,
    diff_cbcl_internal = cbcl_scr_syn_internal_r.1 - cbcl_scr_syn_internal_r.2,
    diff_age = interview_age.1 - interview_age.2,
    # diff_sex = demo_sex_v2.1 - demo_sex_v2.2,
    diff_sex = factor(paste0(demo_sex_v2.1, demo_sex_v2.2)),
    diff_puberty = pds_p_ss_category_2.1 - pds_p_ss_category_2.2,
    eventname = eventname
  ) %>%
  select(rel_family_id, eventname, diff_bmi, diff_whtr, diff_cbcl_internal, diff_age, diff_sex, diff_puberty)

dat_diff_baseline <- dat_diff %>%
  filter(eventname == "baseline_year_1_arm_1")
fitlm <- lm(diff_cbcl_internal ~ diff_age + diff_sex + diff_puberty, data = dat_diff_baseline)
fitlm_whtr <- lm(diff_cbcl_internal ~ diff_whtr + diff_age + diff_sex + diff_puberty, data = dat_diff_baseline)
r2_null <- r2(fitlm)
r2_whtr <- r2(fitlm_whtr)

summary_base <- summary(fitlm_whtr)
ci_base <- confint(fitlm_whtr, 'diff_whtr', level = 0.95)

r2_diff_base_whtr <- r2_whtr$R2-r2_null$R2

# First, ensure the data used in the model does not have any missing values in the relevant columns
dat_diff_baseline_clean <- dat_diff_baseline %>%
  filter(!is.na(diff_whtr), !is.na(diff_cbcl_internal), !is.na(diff_age), !is.na(diff_sex), !is.na(diff_puberty))

# Refit the linear model on the cleaned data
fitlm_whtr_clean <- lm(diff_cbcl_internal ~ diff_whtr + diff_age + diff_sex + diff_puberty, data = dat_diff_baseline_clean)

# Calculate the coefficient for diff_whtr
beta_whtr <- coef(fitlm_whtr_clean)["diff_whtr"]

# Calculate partial residuals
dat_diff_baseline_clean$partial_residuals <- residuals(fitlm_whtr_clean) + beta_whtr * dat_diff_baseline_clean$diff_whtr

# Create the plot of partial residuals
ggplot(dat_diff_baseline_clean, aes(x = diff_whtr, y = partial_residuals)) +
  geom_point(alpha = 0.9, color = rgb(246/255, 195/255, 153/255), size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  xlim(-0.3, 0.3) +
  ylim(-30, 30) +
  coord_fixed(ratio = .02) +
  labs(
    x = "Difference in Waist-to-Height Ratio",
    y = "Adjusted Residuals of Internalizing Symptoms",
    title = "Partial Residual Plot",
    subtitle = "Adjusting for Age, Sex, and Puberty Scores"
  ) +
  theme_minimal()


dat_diff_1y <- dat_diff %>%
  filter(eventname == "1_year_follow_up_y_arm_1")
fitlm <- lm(diff_cbcl_internal ~ diff_age + diff_sex + diff_puberty, data = dat_diff_1y)
fitlm_whtr <- lm(diff_cbcl_internal ~ diff_whtr + diff_age + diff_sex + diff_puberty, data = dat_diff_1y)
r2_null <- r2(fitlm)
r2_whtr <- r2(fitlm_whtr)

summary_1y <- summary(fitlm_whtr)
ci_1y <- confint(fitlm_whtr, 'diff_whtr', level = 0.95)

r2_diff_1y_whtr <- r2_whtr$R2-r2_null$R2

# First, ensure the data used in the model does not have any missing values in the relevant columns
dat_diff_1y_clean <- dat_diff_1y %>%
  filter(!is.na(diff_whtr), !is.na(diff_cbcl_internal), !is.na(diff_age), !is.na(diff_sex), !is.na(diff_puberty))

# Refit the linear model on the cleaned data
fitlm_whtr_clean <- lm(diff_cbcl_internal ~ diff_whtr + diff_age + diff_sex + diff_puberty, data = dat_diff_1y_clean)

# Calculate the coefficient for diff_whtr
beta_whtr <- coef(fitlm_whtr_clean)["diff_whtr"]

# Calculate partial residuals
dat_diff_1y_clean$partial_residuals <- residuals(fitlm_whtr_clean) + beta_whtr * dat_diff_1y_clean$diff_whtr

# Create the plot of partial residuals
ggplot(dat_diff_1y_clean, aes(x = diff_whtr, y = partial_residuals)) +
  geom_point(alpha = 0.9, color = rgb(227/255, 150/255, 145/255), size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  xlim(-0.3, 0.3) +
  ylim(-30, 30) +
  coord_fixed(ratio = .02) +
  labs(
    x = "Difference in Waist-to-Height Ratio",
    y = "Adjusted Residuals of Internalizing Symptoms",
    title = "Partial Residual Plot",
    subtitle = "Adjusting for Age, Sex, and Puberty Scores"
  ) +
  theme_minimal()





dat_diff_2y <- dat_diff %>%
  filter(eventname == "2_year_follow_up_y_arm_1")
fitlm <- lm(diff_cbcl_internal ~ diff_age + diff_sex + diff_puberty, data = dat_diff_2y)
fitlm_whtr <- lm(diff_cbcl_internal ~ diff_whtr + diff_age + diff_sex + diff_puberty, data = dat_diff_2y)
r2_null <- r2(fitlm)
r2_whtr <- r2(fitlm_whtr)

summary_2y <- summary(fitlm_whtr)
ci_2y <- confint(fitlm_whtr, 'diff_whtr', level = 0.95)

r2_diff_2y_whtr <- r2_whtr$R2-r2_null$R2

# First, ensure the data used in the model does not have any missing values in the relevant columns
dat_diff_2y_clean <- dat_diff_2y %>%
  filter(!is.na(diff_whtr), !is.na(diff_cbcl_internal), !is.na(diff_age), !is.na(diff_sex), !is.na(diff_puberty))

# Refit the linear model on the cleaned data
fitlm_whtr_clean <- lm(diff_cbcl_internal ~ diff_whtr + diff_age + diff_sex + diff_puberty, data = dat_diff_2y_clean)

# Calculate the coefficient for diff_whtr
beta_whtr <- coef(fitlm_whtr_clean)["diff_whtr"]

# Calculate partial residuals
dat_diff_2y_clean$partial_residuals <- residuals(fitlm_whtr_clean) + beta_whtr * dat_diff_2y_clean$diff_whtr

# Create the plot of partial residuals
ggplot(dat_diff_2y_clean, aes(x = diff_whtr, y = partial_residuals)) +
  geom_point(alpha = 0.9, color = rgb(164/255, 189/255, 216/255), size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  xlim(-0.3, 0.3) +
  ylim(-30, 30) +
  coord_fixed(ratio = .02) +
  labs(
    x = "Difference in Waist-to-Height Ratio",
    y = "Adjusted Residuals of Internalizing Symptoms",
    title = "Partial Residual Plot",
    subtitle = "Adjusting for Age, Sex, and Puberty Scores"
  ) +
  theme_minimal()



dat_diff_3y <- dat_diff %>%
  filter(eventname == "3_year_follow_up_y_arm_1")
fitlm <- lm(diff_cbcl_internal ~ diff_age + diff_sex + diff_puberty, data = dat_diff_3y)
fitlm_whtr <- lm(diff_cbcl_internal ~ diff_whtr + diff_age + diff_sex + diff_puberty, data = dat_diff_3y)
r2_null <- r2(fitlm)
r2_whtr <- r2(fitlm_whtr)

summary_3y <- summary(fitlm_whtr)
ci_3y <- confint(fitlm_whtr, 'diff_whtr', level = 0.95)

r2_diff_3y_whtr <- r2_whtr$R2-r2_null$R2

# First, ensure the data used in the model does not have any missing values in the relevant columns
dat_diff_3y_clean <- dat_diff_3y %>%
  filter(!is.na(diff_whtr), !is.na(diff_cbcl_internal), !is.na(diff_age), !is.na(diff_sex), !is.na(diff_puberty))

# Refit the linear model on the cleaned data
fitlm_whtr_clean <- lm(diff_cbcl_internal ~ diff_whtr + diff_age + diff_sex + diff_puberty, data = dat_diff_3y_clean)

# Calculate the coefficient for diff_whtr
beta_whtr <- coef(fitlm_whtr_clean)["diff_whtr"]

# Calculate partial residuals
dat_diff_3y_clean$partial_residuals <- residuals(fitlm_whtr_clean) + beta_whtr * dat_diff_3y_clean$diff_whtr

# Create the plot of partial residuals
ggplot(dat_diff_3y_clean, aes(x = diff_whtr, y = partial_residuals)) +
  geom_point(alpha = 0.9, color = rgb(179/255, 214/255, 171/255), size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  xlim(-0.3, 0.3) +
  ylim(-30, 30) +
  coord_fixed(ratio = .02) +
  labs(
    x = "Difference in Waist-to-Height Ratio",
    y = "Adjusted Residuals of Internalizing Symptoms",
    title = "Partial Residual Plot",
    subtitle = "Adjusting for Age, Sex, and Puberty Scores"
  ) +
  theme_minimal()



dat_diff_4y <- dat_diff %>%
  filter(eventname == "4_year_follow_up_y_arm_1")
fitlm <- lm(diff_cbcl_internal ~ diff_age + diff_sex + diff_puberty, data = dat_diff_4y)
fitlm_whtr <- lm(diff_cbcl_internal ~ diff_whtr + diff_age + diff_sex + diff_puberty, data = dat_diff_4y)
r2_null <- r2(fitlm)
r2_whtr <- r2(fitlm_whtr)

summary_4y <- summary(fitlm_whtr)
ci_4y <- confint(fitlm_whtr, 'diff_whtr', level = 0.95)

r2_diff_4y_whtr <- r2_whtr$R2-r2_null$R2

# First, ensure the data used in the model does not have any missing values in the relevant columns
dat_diff_4y_clean <- dat_diff_4y %>%
  filter(!is.na(diff_whtr), !is.na(diff_cbcl_internal), !is.na(diff_age), !is.na(diff_sex), !is.na(diff_puberty))

# Refit the linear model on the cleaned data
fitlm_whtr_clean <- lm(diff_cbcl_internal ~ diff_whtr + diff_age + diff_sex + diff_puberty, data = dat_diff_4y_clean)

# Calculate the coefficient for diff_whtr
beta_whtr <- coef(fitlm_whtr_clean)["diff_whtr"]

# Calculate partial residuals
dat_diff_4y_clean$partial_residuals <- residuals(fitlm_whtr_clean) + beta_whtr * dat_diff_4y_clean$diff_whtr

# Create the plot of partial residuals
ggplot(dat_diff_4y_clean, aes(x = diff_whtr, y = partial_residuals)) +
  geom_point(alpha = 0.9, color = rgb(198/255, 168/255, 207/255), size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  xlim(-0.3, 0.3) +
  ylim(-30, 30) +
  coord_fixed(ratio = .02) +
  labs(
    x = "Difference in Waist-to-Height Ratio",
    y = "Adjusted Residuals of Internalizing Symptoms",
    title = "Partial Residual Plot",
    subtitle = "Adjusting for Age, Sex, and Puberty Scores"
  ) +
  theme_minimal()


#####################################BMI###########################
dat_diff_baseline <- dat_diff %>%
  filter(eventname == "baseline_year_1_arm_1")
fitlm <- lm(diff_cbcl_internal ~ diff_age + diff_sex + diff_puberty, data = dat_diff_baseline)
fitlm_bmi <- lm(diff_cbcl_internal ~ diff_bmi + diff_age + diff_sex + diff_puberty, data = dat_diff_baseline)
r2_null <- r2(fitlm)
r2_bmi <- r2(fitlm_bmi)

r2_diff_base_bmi <- r2_bmi$R2-r2_null$R2


dat_diff_1y <- dat_diff %>%
  filter(eventname == "1_year_follow_up_y_arm_1")
fitlm <- lm(diff_cbcl_internal ~ diff_age + diff_sex + diff_puberty, data = dat_diff_1y)
fitlm_bmi <- lm(diff_cbcl_internal ~ diff_bmi + diff_age + diff_sex + diff_puberty, data = dat_diff_1y)
r2_null <- r2(fitlm)
r2_bmi <- r2(fitlm_bmi)

r2_diff_1y_bmi <- r2_bmi$R2-r2_null$R2


dat_diff_2y <- dat_diff %>%
  filter(eventname == "2_year_follow_up_y_arm_1")
fitlm <- lm(diff_cbcl_internal ~ diff_age + diff_sex + diff_puberty, data = dat_diff_2y)
fitlm_bmi <- lm(diff_cbcl_internal ~ diff_bmi + diff_age + diff_sex + diff_puberty, data = dat_diff_2y)
r2_null <- r2(fitlm)
r2_bmi <- r2(fitlm_bmi)

r2_diff_2y_bmi <- r2_bmi$R2-r2_null$R2


dat_diff_3y <- dat_diff %>%
  filter(eventname == "3_year_follow_up_y_arm_1")
fitlm <- lm(diff_cbcl_internal ~ diff_age + diff_sex + diff_puberty, data = dat_diff_3y)
fitlm_bmi <- lm(diff_cbcl_internal ~ diff_bmi + diff_age + diff_sex + diff_puberty, data = dat_diff_3y)
r2_null <- r2(fitlm)
r2_bmi <- r2(fitlm_bmi)

r2_diff_3y_bmi <- r2_bmi$R2-r2_null$R2


dat_diff_4y <- dat_diff %>%
  filter(eventname == "4_year_follow_up_y_arm_1")
fitlm <- lm(diff_cbcl_internal ~ diff_age + diff_sex + diff_puberty, data = dat_diff_4y)
fitlm_bmi <- lm(diff_cbcl_internal ~ diff_bmi + diff_age + diff_sex + diff_puberty, data = dat_diff_4y)
r2_null <- r2(fitlm)
r2_bmi <- r2(fitlm_bmi)

r2_diff_4y_bmi <- r2_bmi$R2-r2_null$R2

dof_base <- summary_base$df[2] # Residual degrees of freedom
t_base <- summary_base$coefficients[2,3]
cat("Sibling Discordance - Baseline:\n")
cat("  t-statistic:", t_base, "\n")
cat("  t-statistic DoF:", dof_base, "\n")
cat("  95% CI for diff_whtr:", ci_base, "\n\n")

dof_1y <- summary_1y$df[2] # Residual degrees of freedom
t_1y <- summary_1y$coefficients[2,3]
cat("Sibling Discordance - 1yline:\n")
cat("  t-statistic:", t_1y, "\n")
cat("  t-statistic DoF:", dof_1y, "\n")
cat("  95% CI for diff_whtr:", round(ci_1y, digits = 1), "\n\n")

dof_2y <- summary_2y$df[2] # Residual degrees of freedom
t_2y <- summary_2y$coefficients[2,3]
cat("Sibling Discordance - 2yline:\n")
cat("  t-statistic:", t_2y, "\n")
cat("  t-statistic DoF:", dof_2y, "\n")
cat("  95% CI for diff_whtr:", round(ci_2y, digits = 1), "\n\n")

dof_3y <- summary_3y$df[2] # Residual degrees of freedom
t_3y <- summary_3y$coefficients[2,3]
cat("Sibling Discordance - 3yline:\n")
cat("  t-statistic:", t_3y, "\n")
cat("  t-statistic DoF:", dof_3y, "\n")
cat("  95% CI for diff_whtr:", round(ci_3y, digits = 1), "\n\n")

dof_4y <- summary_4y$df[2] # Residual degrees of freedom
t_4y <- summary_4y$coefficients[2,3]
cat("Sibling Discordance - 4yline:\n")
cat("  t-statistic:", t_4y, "\n")
cat("  t-statistic DoF:", dof_4y, "\n")
cat("  95% CI for diff_whtr:", round(ci_4y, digits = 1), "\n\n")
