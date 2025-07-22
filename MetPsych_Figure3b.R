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
                   demo_sex_v2 + demo_ethn_v2 + pds_p_ss_category_2 + race_cleaned +
                   # demo_max_income_v2 + demo_max_ed_v2 +
                   demo_max_income_v2 + 
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
vars_to_select <- grep("_ss_|select_language|admin", names(dat_fe), value = TRUE, invert = TRUE)
dat_fe_y2 <- dat_fe[dat_fe$eventname == "2_year_follow_up_y_arm_1", c(vars_to_select)]
dat_fe_y2[dat_fe_y2 == 999] <- NA

vars_to_select <- grep("_ss_|select_language|admin", names(dat_pr), value = TRUE, invert = TRUE)
dat_pr_y2 <- dat_pr[dat_pr$eventname == "2_year_follow_up_y_arm_1", c(vars_to_select)]
dat_pr_y2[dat_pr_y2 == 999] <- NA

vars_to_select <- grep("_ss_|select_language|admin|device", names(dat_sch), value = TRUE, invert = TRUE)
dat_sch_y2 <- dat_sch[dat_sch$eventname == "2_year_follow_up_y_arm_1", c(vars_to_select)]
dat_sch_y2[dat_sch_y2 == 999] <- NA

vars_to_select <- grep("_ss_|select_language|admin", names(dat_com), value = TRUE, invert = TRUE)
dat_com_y2 <- dat_com[dat_com$eventname == "2_year_follow_up_y_arm_1", c(vars_to_select)]
dat_com_y2[dat_com_y2 == 999] <- NA

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

vars_to_select <- grep("fes|fam_env|school_|pbp|comc", names(dat_year2), value = TRUE)
# model_var <- lm(Random_Slope_WHtR ~ var_here * aces_final, data = dat_year2)

# Initialize a data frame to store results
results <- data.frame(Variable = character(),
                      T_score = numeric(),
                      P_value = numeric(),
                      stringsAsFactors = FALSE)

# Loop over the selected variables to fit models and extract stats
for (var in vars_to_select) {
  # Construct the formula string dynamically
  formula_str <- paste("Random_Slope_WHtR ~", var, "* aces_final")
  
  # Fit the model using the formula
  model <- lm(as.formula(formula_str), data = dat_year2)
  
  # Extract t-score and p-value for the interaction term
  interaction_term <- paste(var, "aces_final", sep = ":")
  if (interaction_term %in% rownames(summary(model)$coefficients)) {
    t_score <- summary(model)$coefficients[interaction_term, "t value"]
    p_value <- summary(model)$coefficients[interaction_term, "Pr(>|t|)"]
    
    # Append results to the data frame
    results <- rbind(results, data.frame(Variable = var, T_score = t_score, P_value = p_value))
  }
}

# Order results by the absolute t-score for plotting
results <- results[order(abs(results$T_score), decreasing = TRUE),]

# Filter results for the new significance level
significant_results <- results[results$P_value <= 0.05, ]

# Determine group membership based on substrings in variable names
significant_results$Group <- ifelse(grepl("fes|fam_env", significant_results$Variable), "Group 1",
                                    ifelse(grepl("pbp", significant_results$Variable), "Group 2", "Other"))
significant_results$Alpha <- ifelse(significant_results$P_value <= .05/82, 1, 0.5)  # Full opacity for MC significant results

# Update the lollipop plot
ggplot(significant_results, aes(x = reorder(Variable, T_score), y = T_score, color = Group)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +  # Vertical line at t=0
  geom_segment(aes(xend = Variable, yend = 0), color = "grey") +
  geom_point(size = 8, aes(alpha = Alpha)) +  # Adjust alpha based on significance
  coord_flip() +  # Make the plot horizontal
  scale_color_manual(values = c("Group 1" = "#1f77b4", "Group 2" = "#2ca02c", "Other" = "#d62728")) +  # Custom colors for groups
  scale_alpha_continuous(range = c(0.5, 1), guide = 'none') +  # Manage alpha scaling and hide the alpha legend
  labs(title = "Ordered Lollipop Plot of T-scores for Significant Interaction Terms",
       x = "Variable",
       y = "T-score") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text.y = element_text(size = 10),
    axis.title.y = element_blank(),
    legend.title = element_blank()
  )


