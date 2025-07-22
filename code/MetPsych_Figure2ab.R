working_dir="/Users/rasmussj/Documents/PaperSubmission/MET_PSYCH/Analysis_5_1/"
setwd(working_dir)
rm(list = ls())

library(dplyr)
library(lme4)
library(ggplot2)
library(lmerTest)

load(file = "dat_cleaned_04262024.RData")

dat_aces <- read.csv("aces_final_5_1.csv") 
dat_slope <- left_join(dat_cleaned, dat_aces, by = c("src_subject_id"))

lme_full <- lmer(cbcl_scr_syn_internal_r ~ whtr + interview_age + reshist_addr1_adi_wsum +
                   demo_sex_v2 + demo_ethn_v2 + pds_p_ss_category_2 + race_cleaned + demo_max_ed_v2 + demo_max_income_v2 +
                   (whtr|src_subject_id) + (1|site_id_l) + (1|rel_family_id),
                 data = dat_slope, REML = TRUE)

# Extract random effects
random_effects <- ranef(lme_full, condVar = TRUE)$src_subject_id

#Merge with covariates
slope_aces_data_cov <- data.frame(src_subject_id = rownames(random_effects), Slope = random_effects$whtr)
dat_slope_unique_cov <- dat_slope %>%
  select(src_subject_id, aces_final, demo_max_ed_v2, demo_sex_v2, demo_max_income_v2, reshist_addr1_adi_wsum, race_cleaned, demo_max_income_v2, demo_ethn_v2) %>%
  distinct()
slope_aces_data_cov <- left_join(slope_aces_data_cov, dat_slope_unique_cov, by = "src_subject_id")

lm_association_cov <- lm(Slope ~ aces_final + demo_max_ed_v2 + demo_sex_v2 + demo_max_income_v2 + reshist_addr1_adi_wsum + race_cleaned + demo_ethn_v2, data = slope_aces_data_cov)
summary(lm_association_cov)

#Merge and test without covariates
slope_aces_data <- data.frame(src_subject_id = rownames(random_effects), Slope = random_effects$whtr)
dat_slope_unique <- dat_slope %>%
  select(src_subject_id, aces_final) %>%
  distinct()
slope_aces_data <- left_join(slope_aces_data, dat_slope_unique, by = "src_subject_id")

lm_association <- lm(Slope ~ aces_final, data = slope_aces_data)
summary(lm_association)

#Consider slope and intercept
slope_int_aces_data <- data.frame(src_subject_id = rownames(random_effects), Slope = random_effects$whtr, Intercept = random_effects$`(Intercept)`)
dat_slope_unique_int <- dat_slope %>%
  select(src_subject_id, aces_final) %>%
  distinct()
slope_int_aces_data <- left_join(slope_int_aces_data, dat_slope_unique, by = "src_subject_id")

lm_association_int <- lm(aces_final ~ Slope + Intercept, data = slope_int_aces_data)
summary(lm_association_int)

#Because not perfectly normal dist.
lm_log_association <- lm(log(Slope) ~ aces_final, data = slope_aces_data)
summary(lm_log_association)

ggplot(slope_aces_data, aes(x = aces_final, y = Slope)) +
  geom_point(shape = 21, color = "black", fill = "lightgray", size = 3, stroke = 0.5) +
  geom_smooth(method = "lm",  size = 1.5) +  # Thicker line for the fit
  labs(title = "Association Between ACEs Final and Random Slopes of WHtR",
       x = "ACEs Final Score",
       y = "Random Slope for WHtR",
       caption = "Linear model fit shown in blue") +
  theme_minimal()

subjects_multi_visit <- dat_cleaned %>%
group_by(src_subject_id) %>%
  summarise(n_visits = n()) %>%
  filter(n_visits > 1) %>%
  pull(src_subject_id)  # Extracts the IDs into a vector

slope_aces_data_filtered <- slope_aces_data %>%
  filter(src_subject_id %in% subjects_multi_visit)

lm_association_multi <- lm(Slope ~ aces_final, data = slope_aces_data_filtered)
summary(lm_association_multi)

# # Names of the visits
visits <- c("baseline_year_1_arm_1", "1_year_follow_up_y_arm_1", "2_year_follow_up_y_arm_1",
            "3_year_follow_up_y_arm_1", "4_year_follow_up_y_arm_1")

# # Empty list to store models
models <- list()

# # Full sample model
models[["full_sample"]] <- lmer(cbcl_scr_syn_internal_r ~ aces_final * whtr + interview_age + reshist_addr1_adi_wsum +
                                  demo_sex_v2 + demo_ethn_v2 + pds_p_ss_category_2 + race_cleaned + demo_max_income_v2 +
                                  # demo_sex_v2 + demo_ethn_v2 + pds_p_ss_category_2 + race_cleaned + demo_max_ed_v2 + demo_max_income_v2 +
                                  (1|src_subject_id) + (1|site_id_l) + (1|rel_family_id),
                                data = dat_slope, REML = TRUE)

# Models for each visit
for (visit in visits) {
  models[[visit]] <- lmer(cbcl_scr_syn_internal_r ~ aces_final * whtr + interview_age + reshist_addr1_adi_wsum +
                            demo_sex_v2 + demo_ethn_v2 + pds_p_ss_category_2 + race_cleaned + demo_max_income_v2 +
                            # demo_sex_v2 + demo_ethn_v2 + pds_p_ss_category_2 + race_cleaned + demo_max_ed_v2 + demo_max_income_v2 +
                            (1|site_id_l) + (1|rel_family_id),
                          data = filter(dat_slope, eventname == visit), REML = TRUE)
}

# Ensure that all model variables are included in pred_data
pred_data <- expand.grid(
  whtr = seq(min(dat_slope$whtr, na.rm = TRUE), max(dat_slope$whtr, na.rm = TRUE), length.out = 100),
  aces_final = mean(dat_slope$aces_final, na.rm = TRUE), # assuming aces_final is continuous
  interview_age = mean(dat_slope$interview_age, na.rm = TRUE),  # Include mean or a representative value
  reshist_addr1_adi_wsum = mean(dat_slope$reshist_addr1_adi_wsum, na.rm = TRUE),
  demo_sex_v2 = mode(dat_slope$demo_sex_v2),  # Choose a mode or typical category
  demo_ethn_v2 = mode(dat_slope$demo_ethn_v2),  # Choose a mode or typical category
  pds_p_ss_category_2 = mean(dat_slope$pds_p_ss_category_2, na.rm = TRUE),
  demo_max_income_v2 = mean(dat_slope$demo_max_income_v2, na.rm = TRUE),
  # demo_max_ed_v2 = mean(dat_slope$demo_max_ed_v2, na.rm = TRUE),
  race_cleaned = mode(dat_slope$race_cleaned),  # Choose a mode or typical category
  eventname = c(visits, "full_sample")
)

dat_slope <- dat_slope %>%
  mutate(aces_final_group = cut(aces_final,
                                breaks = quantile(aces_final, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
                                labels = c("Low", "Medium", "High", "Very High"),
                                include.lowest = TRUE))

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

visits <- c("baseline_year_1_arm_1", "1_year_follow_up_y_arm_1", "2_year_follow_up_y_arm_1",
            "3_year_follow_up_y_arm_1", "4_year_follow_up_y_arm_1", "full_sample")
pred_data <- expand.grid(
  whtr = seq(min(dat_slope$whtr, na.rm = TRUE), max(dat_slope$whtr, na.rm = TRUE), length.out = 100),
  aces_final_group = levels(dat_slope$aces_final_group),
  interview_age = mean(dat_slope$interview_age, na.rm = TRUE),  # Include mean or a representative value
  reshist_addr1_adi_wsum = mean(dat_slope$reshist_addr1_adi_wsum, na.rm = TRUE),
  demo_sex_v2 = mode(dat_slope$demo_sex_v2),  # Choose a mode or typical category
  demo_ethn_v2 = mode(dat_slope$demo_ethn_v2),  # Choose a mode or typical category
  pds_p_ss_category_2 = mean(dat_slope$pds_p_ss_category_2, na.rm = TRUE),
  demo_max_income_v2 = mean(dat_slope$demo_max_income_v2, na.rm = TRUE),
  # demo_max_ed_v2 = mean(dat_slope$demo_max_ed_v2, na.rm = TRUE),
  race_cleaned = mode(dat_slope$race_cleaned),  # Choose a mode or typical category
  eventname = visits
)

for (visit in visits) {
  subset_data <- dat_slope %>% filter(eventname == visit | visit == "full_sample")
  model <- models[[visit]]
  pred_subset <- pred_data[pred_data$eventname == visit,]
  pred_subset$aces_final <- with(subset_data, tapply(aces_final, aces_final_group, mean)[pred_subset$aces_final_group])
  pred_data$predicted_cbcl[pred_data$eventname == visit] <- predict(model, newdata = pred_subset, re.form = NA)
}


ggplot(pred_data, aes(x = whtr, y = predicted_cbcl, color = aces_final_group)) +
  geom_line(aes(linetype = eventname, alpha = eventname),
            size = ifelse(pred_data$eventname == "full_sample", 2, 1)) +
  labs(title = "Predicted CBCL Score by WHtR Conditioned on ACEs Scores Across Visits",
       x = "Waist-to-Height Ratio (WHtR)",
       y = "Predicted CBCL Internalizing Score",
       color = "ACEs Category",
       linetype = "Visit") +
  theme_minimal() +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 8)) +
  guides(linetype = guide_legend(override.aes = list(size = c(1, 1, 1, 1, 1, 2))),
         alpha = guide_legend(override.aes = list(size = c(1, 1, 1, 1, 1, 2)))) +
  scale_linetype_manual(values = c("baseline_year_1_arm_1" = "dashed",
                                   "1_year_follow_up_y_arm_1" = "dotted",
                                   "2_year_follow_up_y_arm_1" = "dotdash",
                                   "3_year_follow_up_y_arm_1" = "longdash",
                                   "4_year_follow_up_y_arm_1" = "twodash",
                                   "full_sample" = "solid")) +
  scale_alpha_manual(values = c("baseline_year_1_arm_1" = 0.4,
                                "1_year_follow_up_y_arm_1" = 0.4,
                                "2_year_follow_up_y_arm_1" = 0.4,
                                "3_year_follow_up_y_arm_1" = 0.4,
                                "4_year_follow_up_y_arm_1" = 0.4,
                                "full_sample" = 1.0))
