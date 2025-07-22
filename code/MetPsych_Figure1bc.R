working_dir="/Users/rasmussj/Documents/PaperSubmission/MET_PSYCH/Analysis_5_1/"
setwd(working_dir)
rm(list = ls())

library(dplyr)
library('gamm4')
library(ggplot2)
library(performance)

load(file = "dat_cleaned_04262024.RData")

####PLOT SOME BASICS
######IN MAIN (FIGURE 1B)
# Create the visit density plot with interview_age converted to years
ggplot(dat_cleaned, aes(x = interview_age / 12, fill = eventname)) +  # Convert months to years by dividing by 12
  geom_density(alpha = 0.5) +  # Adjust transparency with alpha
  labs(title = "Density Plot of Age by Event Name",
       x = "Age (years)",
       y = "Density") +
  scale_fill_brewer(palette = "Set1") +  # This applies a color palette suitable for categorical data
  theme_minimal() +  # Minimal theme for a cleaner look
  theme(legend.title = element_text(face = "bold"))  # Bold the legend title

######IN supplemennt
# Calculate density with scaling to count of observations
ggplot(dat_cleaned, aes(x = interview_age / 12, y = after_stat(count), fill = eventname)) +
  geom_density(alpha = 0.5, aes(weight = 1), adjust = 1) +  # Use weight to ensure scaling by count
  labs(title = "Density Plot of Age by Event Name",
       x = "Age (years)",
       y = "Scaled Density") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.title = element_text(face = "bold"))

# Density plot of Year-Month
ggplot(dat_cleaned, aes(x = year_month, fill = eventname)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Interview Dates by Event Name (Monthly)",
       x = "Month",
       y = "Density") +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####AGE WHTR (FIGURE 1C)
# Convert age to years and round to the nearest whole number
dat_cleaned <- dat_cleaned %>%
  mutate(age_years = round(interview_age / 12),
         # Ensure ages are within the desired range
         age_years = ifelse(age_years < 9, 9, ifelse(age_years > 15, 15, age_years)),
         # Calculate z-scores
         z_whtr = (whtr - mean(whtr, na.rm = TRUE)) / sd(whtr, na.rm = TRUE),
         z_bmi = (bmi - mean(bmi, na.rm = TRUE)) / sd(bmi, na.rm = TRUE),
         z_cbcl_scr_syn_internal_r = (cbcl_scr_syn_internal_r - mean(cbcl_scr_syn_internal_r, na.rm = TRUE)) / sd(cbcl_scr_syn_internal_r, na.rm = TRUE))

# Create the boxplot
ggplot(dat_cleaned, aes(x = factor(age_years), y = z_bmi, fill = demo_sex_v2)) +
  geom_boxplot(stat = "boxplot", outlier.shape = NA, coef = 0) +  # Remove whiskers and outliers
  scale_fill_manual(values = c("male" = "lightblue", "female" = "lightpink")) +  # Assign colors
  labs(title = "Z-Scored BMI by Age and Sex",
       x = "Age (Years)",
       y = "Z-Score",
       fill = "Sex") +
  coord_cartesian(ylim = c(-1.2, 1.2)) +  # Set y-axis limits
  theme_minimal() +  # Use a minimal theme
  theme(axis.title = element_text(size = 12, face = "bold"),  # Style axis titles
        axis.text.x = element_text(angle = 45, hjust = 1),  # Improve x-axis label readability
        legend.title = element_text(face = "bold"),  # Bold the legend title
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))  # Center and bold the plot title

# Create the boxplot w/ whiskers and outliers
ggplot(dat_cleaned, aes(x = factor(age_years), y = bmi, fill = demo_sex_v2)) +
  geom_boxplot(stat = "boxplot") +  # Remove whiskers and outliers
  scale_fill_manual(values = c("male" = "lightblue", "female" = "lightpink")) +  # Assign colors
  labs(title = "Z-Scored BMI by Age and Sex",
       x = "Age (Years)",
       y = "Z-Score",
       fill = "Sex") +
  theme_minimal() +  # Use a minimal theme
  theme(axis.title = element_text(size = 12, face = "bold"),  # Style axis titles
        axis.text.x = element_text(angle = 45, hjust = 1),  # Improve x-axis label readability
        legend.title = element_text(face = "bold"),  # Bold the legend title
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))  # Center and bold the plot title

# Create the boxplot for z-whtr
ggplot(dat_cleaned, aes(x = factor(age_years), y = z_whtr, fill = demo_sex_v2)) +
  geom_boxplot(stat = "boxplot", outlier.shape = NA, coef = 0) +  # Remove whiskers and outliers
  scale_fill_manual(values = c("male" = "lightblue", "female" = "lightpink")) +  # Assign colors
  labs(title = "Z-Scored BMI by Age and Sex",
       x = "Age (Years)",
       y = "Z-Score",
       fill = "Sex") +
  coord_cartesian(ylim = c(-1.2, 1.2)) +  # Set y-axis limits
  theme_minimal() +  # Use a minimal theme
  theme(axis.title = element_text(size = 12, face = "bold"),  # Style axis titles
        axis.text.x = element_text(angle = 45, hjust = 1),  # Improve x-axis label readability
        legend.title = element_text(face = "bold"),  # Bold the legend title
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))  # Center and bold the plot title

# Create the boxplot for whtr with whiskers and outliers
ggplot(dat_cleaned, aes(x = factor(age_years), y = whtr, fill = demo_sex_v2)) +
  geom_boxplot(stat = "boxplot") +  # Remove whiskers and outliers
  scale_fill_manual(values = c("male" = "lightblue", "female" = "lightpink")) +  # Assign colors
  labs(title = "Z-Scored BMI by Age and Sex",
       x = "Age (Years)",
       y = "Z-Score",
       fill = "Sex") +
  theme_minimal() +  # Use a minimal theme
  theme(axis.title = element_text(size = 12, face = "bold"),  # Style axis titles
        axis.text.x = element_text(angle = 45, hjust = 1),  # Improve x-axis label readability
        legend.title = element_text(face = "bold"),  # Bold the legend title
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))  # Center and bold the plot title

# Create the boxplot for CBCL INT with whiskers and outliers
ggplot(dat_cleaned, aes(x = factor(age_years), y = cbcl_scr_syn_internal_r, fill = demo_sex_v2)) +
  geom_boxplot(stat = "boxplot") +  # Remove whiskers and outliers
  scale_fill_manual(values = c("male" = "lightblue", "female" = "lightpink")) +  # Assign colors
  labs(title = "Z-Scored BMI by Age and Sex",
       x = "Age (Years)",
       y = "Z-Score",
       fill = "Sex") +
  theme_minimal() +  # Use a minimal theme
  theme(axis.title = element_text(size = 12, face = "bold"),  # Style axis titles
        axis.text.x = element_text(angle = 45, hjust = 1),  # Improve x-axis label readability
        legend.title = element_text(face = "bold"),  # Bold the legend title
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))  # Center and bold the plot title

gamm_whtr_sex <- gamm4(z_whtr ~ s(interview_age, by = demo_sex_v2) , random = ~(1|src_subject_id) + (1|rel_family_id), data = dat_cleaned)
plot(gamm_whtr_sex$gam, pages = 1, all.terms = TRUE, ylim = c(-1, 1))
gamm_whtr <- gamm4(z_whtr ~ s(interview_age) + demo_sex_v2 , random = ~(1|src_subject_id) + (1|rel_family_id), data = dat_cleaned)
summary(gamm_whtr$gam)

gamm_bmi_sex <- gamm4(z_bmi ~ s(interview_age, by = demo_sex_v2) , random = ~(1|src_subject_id) + (1|rel_family_id), data = dat_cleaned)
plot(gamm_bmi_sex$gam, pages = 1, all.terms = TRUE, ylim = c(-1, 1))
gamm_bmi <- gamm4(z_bmi ~ s(interview_age) + demo_sex_v2 , random = ~(1|src_subject_id) + (1|rel_family_id), data = dat_cleaned)
summary(gamm_bmi$gam)
