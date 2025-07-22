working_dir="/Users/rasmussj/Documents/PaperSubmission/MET_PSYCH/Analysis_5_1/"
setwd(working_dir)
rm(list = ls())

library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)

load(file = "dat_cleaned_04262024.RData")

dat_aces <- read.csv("aces_final_5_1.csv") 
dat_cleaned <- left_join(dat_cleaned, dat_aces, by = c("src_subject_id"))

###Get slopes
lme_full <- lmer(cbcl_scr_syn_internal_r ~ whtr + interview_age + reshist_addr1_adi_wsum +
                   demo_max_ed_v2 + demo_max_income_v2 +
                   demo_sex_v2 + demo_ethn_v2 + pds_p_ss_category_2 + race_cleaned +
                   (whtr|src_subject_id) + (1|site_id_l) + (1|rel_family_id),
                 data = dat_cleaned, REML = TRUE)

random_effects <- ranef(lme_full)$src_subject_id
random_slopes <- random_effects[, "whtr", drop = FALSE]

slope_data <- data.frame(src_subject_id = rownames(random_effects), Random_Slope_WHtR = random_slopes$whtr)
dat_cleaned$src_subject_id <- as.character(dat_cleaned$src_subject_id)
slope_data$src_subject_id <- as.character(slope_data$src_subject_id)

dat_cleaned <- merge(dat_cleaned, slope_data, by = "src_subject_id", all.x = TRUE)
dat_cleaned$Random_Slope_WHtR <- ifelse(dat_cleaned$eventname == "baseline_year_1_arm_1", dat_cleaned$Random_Slope_WHtR, NA)

###Import positive ecologies, use time point 2 measures, get level, FA and PCA measures
dat_fe <- read.csv("ce_p_fes.csv") 
dat_pr <- read.csv("ce_y_pbp.csv") 
dat_sch <- read.csv("ce_y_srpf.csv") 
dat_com <- read.csv("ce_p_comc.csv") 

# Assuming the data frames have the columns 'src_subject_id' and 'eventname'
dat_fe_y2 <- dat_fe[dat_fe$eventname == "2_year_follow_up_y_arm_1", c("src_subject_id", "eventname", "fes_p_ss_cohesion_sum_pr")]
dat_pr_y2 <- dat_pr[dat_pr$eventname == "2_year_follow_up_y_arm_1", c("src_subject_id", "eventname", "pbp_ss_prosocial_peers")]
dat_sch_y2 <- dat_sch[dat_sch$eventname == "2_year_follow_up_y_arm_1", c("src_subject_id", "eventname", "srpf_y_ss_ses")]
dat_com_y2 <- dat_com[dat_com$eventname == "2_year_follow_up_y_arm_1", c("src_subject_id", "eventname", "comc_ss_cohesion_p")]

# Ensure that 'src_subject_id' and 'eventname' are correctly typed across all dataframes
dat_cleaned <- left_join(dat_cleaned, dat_fe_y2, by = c("src_subject_id", "eventname"))
dat_cleaned <- left_join(dat_cleaned, dat_pr_y2, by = c("src_subject_id", "eventname"))
dat_cleaned <- left_join(dat_cleaned, dat_sch_y2, by = c("src_subject_id", "eventname"))
dat_cleaned <- left_join(dat_cleaned, dat_com_y2, by = c("src_subject_id", "eventname"))

dat_cleaned <- dat_cleaned %>%
  group_by(src_subject_id) %>%
  mutate(Random_Slope_WHtR = if_else(eventname == "baseline_year_1_arm_1", Random_Slope_WHtR, NA_real_)) %>%
  fill(Random_Slope_WHtR, .direction = "downup") %>%
  filter(eventname == "2_year_follow_up_y_arm_1" | eventname == "baseline_year_1_arm_1")

# Now we'll create a version of the dataframe filtered for the year 2 follow-up
dat_year2 <- dat_cleaned %>%
  filter(eventname == "2_year_follow_up_y_arm_1")

# Model with fes_p_ss_cohesion_sum_pr
model_cohesion <- lm(Random_Slope_WHtR ~ fes_p_ss_cohesion_sum_pr, data = dat_year2)
summary(model_cohesion)

# Model with pbp_ss_prosocial_peers
model_prosocial <- lm(Random_Slope_WHtR ~ pbp_ss_prosocial_peers, data = dat_year2)
summary(model_prosocial)

# Model with srpf_y_ss_ses
model_ses <- lm(Random_Slope_WHtR ~ srpf_y_ss_ses, data = dat_year2)
summary(model_ses)

# Model with comc_ss_cohesion_p
model_comc_cohesion <- lm(Random_Slope_WHtR ~ comc_ss_cohesion_p, data = dat_year2)
summary(model_comc_cohesion)

# Model with comc_ss_cohesion_p
model_pos_all <- lm(Random_Slope_WHtR ~ comc_ss_cohesion_p + srpf_y_ss_ses + pbp_ss_prosocial_peers + fes_p_ss_cohesion_sum_pr, data = dat_year2)
summary(model_pos_all)

# Model with aces_final - need to filter for baseline data
dat_baseline <- dat_cleaned %>%
  filter(eventname == "baseline_year_1_arm_1")

model_aces <- lm(Random_Slope_WHtR ~ aces_final, data = dat_baseline)
summary(model_aces)

# Filter data to ensure all variables are non-NA for the analysis
dat_for_model <- dat_cleaned %>%
  filter(!is.na(comc_ss_cohesion_p) & !is.na(srpf_y_ss_ses) &  !is.na(pbp_ss_prosocial_peers) &  !is.na(fes_p_ss_cohesion_sum_pr) & !is.na(aces_final) & !is.na(Random_Slope_WHtR) &
           eventname == "2_year_follow_up_y_arm_1")
dat_for_factor_analysis <- ungroup(dat_for_model) %>%
  select(fes_p_ss_cohesion_sum_pr, pbp_ss_prosocial_peers, srpf_y_ss_ses, comc_ss_cohesion_p) %>%
  na.omit()  # Ensuring no NAs
fa_result <- factanal(dat_for_factor_analysis, factors = 1, scores = "regression")
chi_square_value <- fa_result$STATISTIC
degrees_of_freedom <- fa_result$dof
dat_for_model$latent_factor_score <- fa_result$scores

library(lavaan)
model <- "
  general =~ fes_p_ss_cohesion_sum_pr 
           + pbp_ss_prosocial_peers 
           + srpf_y_ss_ses 
           + comc_ss_cohesion_p
"
cfa_result <- cfa(model, data = dat_for_factor_analysis)
summary(cfa_result, fit.measures=TRUE, standardized=TRUE)


interaction_model_community <- lm(Random_Slope_WHtR ~ comc_ss_cohesion_p * aces_final , data = dat_for_model)
summary(interaction_model_community)
interaction_model_peers <- lm(Random_Slope_WHtR ~ pbp_ss_prosocial_peers * aces_final , data = dat_for_model)
summary(interaction_model_peers)
interaction_model_school <- lm(Random_Slope_WHtR ~ srpf_y_ss_ses * aces_final , data = dat_for_model)
summary(interaction_model_school)
interaction_model_family <- lm(Random_Slope_WHtR ~ fes_p_ss_cohesion_sum_pr * aces_final , data = dat_for_model)
summary(interaction_model_family)
interaction_model_all <- lm(Random_Slope_WHtR ~ comc_ss_cohesion_p * aces_final + pbp_ss_prosocial_peers * aces_final + srpf_y_ss_ses * aces_final + fes_p_ss_cohesion_sum_pr * aces_final, data = dat_for_model)
summary(interaction_model_all)

# Extract t-values
t_values <- data.frame(
  Component = c("fes_p_ss_cohesion_sum_pr", "pbp_ss_prosocial_peers", "srpf_y_ss_ses", "comc_ss_cohesion_p",
                "fes_p_ss_cohesion_sum_pr_all", "pbp_ss_prosocial_peers_all", "srpf_y_ss_ses_all", "comc_ss_cohesion_p_all"),
  Model = c("Independent", "Independent", "Independent", "Independent", "All", "All", "All", "All"),
  T_value = c(
    summary(interaction_model_family)$coefficients["fes_p_ss_cohesion_sum_pr:aces_final", "t value"],
    summary(interaction_model_peers)$coefficients["pbp_ss_prosocial_peers:aces_final", "t value"],
    summary(interaction_model_school)$coefficients["srpf_y_ss_ses:aces_final", "t value"],
    summary(interaction_model_community)$coefficients["comc_ss_cohesion_p:aces_final", "t value"],
    summary(interaction_model_all)$coefficients["aces_final:fes_p_ss_cohesion_sum_pr", "t value"],
    summary(interaction_model_all)$coefficients["aces_final:pbp_ss_prosocial_peers", "t value"],
    summary(interaction_model_all)$coefficients["aces_final:srpf_y_ss_ses", "t value"],
    summary(interaction_model_all)$coefficients["comc_ss_cohesion_p:aces_final", "t value"]
  )
)

# Reorder factor levels for plotting
t_values$Component <- factor(t_values$Component, levels = c("fes_p_ss_cohesion_sum_pr", "pbp_ss_prosocial_peers", "srpf_y_ss_ses", "comc_ss_cohesion_p",
                                                            "fes_p_ss_cohesion_sum_pr_all", "pbp_ss_prosocial_peers_all", "srpf_y_ss_ses_all", "comc_ss_cohesion_p_all"))

# Extract t-values
t_values <- data.frame(
  Component = c("fes_p_ss_cohesion_sum_pr", "pbp_ss_prosocial_peers", "srpf_y_ss_ses", "comc_ss_cohesion_p",
                "fes_p_ss_cohesion_sum_pr_all", "pbp_ss_prosocial_peers_all", "srpf_y_ss_ses_all", "comc_ss_cohesion_p_all"),
  Model = c("Independent", "Independent", "Independent", "Independent", "All", "All", "All", "All"),
  T_value = c(
    summary(interaction_model_family)$coefficients["fes_p_ss_cohesion_sum_pr:aces_final", "t value"],
    summary(interaction_model_peers)$coefficients["pbp_ss_prosocial_peers:aces_final", "t value"],
    summary(interaction_model_school)$coefficients["srpf_y_ss_ses:aces_final", "t value"],
    summary(interaction_model_community)$coefficients["comc_ss_cohesion_p:aces_final", "t value"],
    summary(interaction_model_all)$coefficients["aces_final:fes_p_ss_cohesion_sum_pr", "t value"],
    summary(interaction_model_all)$coefficients["aces_final:pbp_ss_prosocial_peers", "t value"],
    summary(interaction_model_all)$coefficients["aces_final:srpf_y_ss_ses", "t value"],
    summary(interaction_model_all)$coefficients["comc_ss_cohesion_p:aces_final", "t value"]
  )
)

# Reorder factor levels for plotting
t_values$Component <- factor(t_values$Component, levels = c("fes_p_ss_cohesion_sum_pr", "pbp_ss_prosocial_peers", "srpf_y_ss_ses", "comc_ss_cohesion_p",
                                                            "fes_p_ss_cohesion_sum_pr_all", "pbp_ss_prosocial_peers_all", "srpf_y_ss_ses_all", "comc_ss_cohesion_p_all"))



# Reorder factor levels for plotting
# Simplify component names for readability in the plot
t_values$Component <- factor(t_values$Component, levels = c(
  "comc_ss_cohesion_p_all", "comc_ss_cohesion_p",
  "srpf_y_ss_ses_all", "srpf_y_ss_ses",
  "pbp_ss_prosocial_peers_all", "pbp_ss_prosocial_peers",
  "fes_p_ss_cohesion_sum_pr_all", "fes_p_ss_cohesion_sum_pr"
), labels = c(
  # "COMC (All)", "COMC (Ind.)",
  # "SRPF (All)", "SRPF (Ind.)",
  # "PBP (All)", "PBP (Ind.)",
  # "FES (All)", "FES (Ind.)"
  "COMC", "COMC",
  "SRPF", "SRPF",
  "PBP", "PBP",
  "FES", "FES"
))


# Create the plot with specified x-axis limits and custom fill colors
ggplot(t_values, aes(x = T_value, y = Component, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.925) ) +
  #geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  labs(title = "T-scores for Interaction Effects by Component and Model Type",
       x = "T-score",
       y = "Component",
       fill = "Model Type") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        axis.title.y = element_blank()) +  # Improve readability and fit of component names
  scale_x_continuous(limits = c(-6, NA)) +  # Extend the x-axis to -5
  scale_fill_manual(values = c("Independent" = "gray20", "All" = "gray40"))  # Custom colors for model types


interaction_model_lf <- lm(Random_Slope_WHtR ~ latent_factor_score * aces_final, data = dat_for_model)
summary_lf <- summary(interaction_model_lf)

dof_lf <- summary_lf$df[2] # Residual degrees of freedom
ci_lf <- confint(interaction_model_lf, 'latent_factor_score:aces_final', level = 0.95)

cat("Protective Environment (Latent Factor) Interaction:\n")
cat("  t-statistic DoF:", dof_lf, "\n")
cat("  95% CI for latent_factor_score:aces_final:", ci_lf, "\n")

interaction_model <- lm(Random_Slope_WHtR ~ comc_ss_cohesion_p * aces_final + latent_factor_score * aces_final, data = dat_for_model)
summary(interaction_model)
interaction_model <- lm(Random_Slope_WHtR ~ pbp_ss_prosocial_peers * aces_final + latent_factor_score * aces_final, data = dat_for_model)
summary(interaction_model)
interaction_model <- lm(Random_Slope_WHtR ~ srpf_y_ss_ses * aces_final + latent_factor_score * aces_final, data = dat_for_model)
summary(interaction_model)
interaction_model <- lm(Random_Slope_WHtR ~ fes_p_ss_cohesion_sum_pr * aces_final + latent_factor_score * aces_final, data = dat_for_model)
summary(interaction_model)


# Creating bins for the latent_factor_score
dat_for_model <- ungroup(dat_for_model) %>%
  mutate(latent_factor_group = cut(latent_factor_score,
                                   breaks = quantile(latent_factor_score, probs = seq(0, 1, by = 1/3), na.rm = TRUE),
                                   include.lowest = TRUE,
                                   labels = c("Low", "Medium", "High")))

# View the distribution of bins
table(dat_for_model$latent_factor_group)
interaction_model_lf <- lm(Random_Slope_WHtR ~ latent_factor_group * aces_final, data = dat_for_model)

# Prepare data for prediction
aces_range <- seq(min(dat_for_model$aces_final, na.rm = TRUE), max(dat_for_model$aces_final, na.rm = TRUE), length.out = 100)
pred_data <- expand.grid(aces_final = aces_range,
                         latent_factor_group = levels(dat_for_model$latent_factor_group),
                         Random_Slope_WHtR = 0)  # Placeholder column for the outcome

# Predict values
pred_data$predicted_Random_Slope_WHtR <- predict(interaction_model_lf, newdata = pred_data)

# Plotting
ggplot(pred_data, aes(x = aces_final, y = predicted_Random_Slope_WHtR, color = latent_factor_group)) +
  geom_line(size = 2) +
  labs(title = "Interaction of ACEs Final with Latent Factor Score on Random Slope WHtR",
       x = "ACEs Final",
       y = "Predicted Random Slope WHtR",
       color = "Latent Factor Score Group") +
  theme_minimal()

dat_for_rsq_est <- dat_for_model %>%
  filter(latent_factor_group == "Low" )
interaction_model_lf_low <- lm(Random_Slope_WHtR ~ aces_final, data = dat_for_rsq_est)
summary(interaction_model_lf_low)

dat_for_rsq_est <- dat_for_model %>%
  filter(latent_factor_group == "Medium" )
interaction_model_lf_med <- lm(Random_Slope_WHtR ~ aces_final, data = dat_for_rsq_est)
summary(interaction_model_lf_med)

dat_for_rsq_est <- dat_for_model %>%
  filter(latent_factor_group == "High" )
interaction_model_lf_high <- lm(Random_Slope_WHtR ~ aces_final, data = dat_for_rsq_est)
summary(interaction_model_lf_high)


load(file = "dat_cleaned_04262024.RData")
dat_cleaned <- left_join(dat_cleaned, dat_aces, by = c("src_subject_id"))
dat_fa_only <- dat_for_model %>%
  select(src_subject_id,latent_factor_score)
dat_cleaned <- left_join(dat_cleaned, dat_fa_only, by = c("src_subject_id"))
lme_full_3way <- lmer(cbcl_scr_syn_internal_r ~ whtr * aces_final * latent_factor_score + interview_age + reshist_addr1_adi_wsum +
                   demo_max_ed_v2 + demo_max_income_v2 +
                   demo_sex_v2 + demo_ethn_v2 + pds_p_ss_category_2 + race_cleaned +
                   (1|src_subject_id) + (1|site_id_l) + (1|rel_family_id),
                 data = dat_cleaned, REML = TRUE)
summary(lme_full_3way)
