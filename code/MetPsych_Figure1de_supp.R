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
library(norm)

load(file = "dat_cleaned_04262024.RData")

set_pushover_user(user = "ux35bvpn8s6hxcxpmkwgmanvgv8461")
set_pushover_app(token = "agsf5h542sfp8q38uzngpm72ntkk6m")

####################
dat_cbcl <- read.csv("mh_p_cbcl.csv") 
dat_cbcl <- dat_cbcl %>%
  select(
    contains("src_subject_id"),
    contains("eventname"),
    contains("cbcl_scr_syn_internal_t")
  )

dat_cleaned <- left_join(dat_cleaned, dat_cbcl, by = c("src_subject_id", "eventname"))
rm(dat_cbcl)

####################
dat_bpm_y <- read.csv("mh_y_bpm.csv") 
dat_bpm_y <- dat_bpm_y %>%
  select(
    contains("src_subject_id"),
    contains("eventname"),
    contains("bpm_y_scr_internal_r")
  )

dat_cleaned <- left_join(dat_cleaned, dat_bpm_y, by = c("src_subject_id", "eventname"))
rm(dat_bpm_y)

####################
dat_bpm_t <- read.csv("mh_t_bpm.csv") 
dat_bpm_t <- dat_bpm_t %>%
  select(
    contains("src_subject_id"),
    contains("eventname"),
    contains("bpm_t_scr_internal_r")
  )

dat_cleaned <- left_join(dat_cleaned, dat_bpm_t, by = c("src_subject_id", "eventname"))
rm(dat_bpm_t)

data_subset <- na.omit(dat_cleaned[, c("bpm_y_scr_internal_r", "bpm_t_scr_internal_r", "cbcl_scr_syn_internal_r")])
ggplot(data_subset, aes(x = bpm_t_scr_internal_r, y = bpm_y_scr_internal_r)) +
  geom_jitter(alpha = 0.05, color = "black") +                                                  # Add jitter for scatter plot
  geom_smooth(method = "lm", color = "red", na.rm = TRUE) +                                    # Linear fit
  geom_boxplot(aes(group = bpm_t_scr_internal_r), outlier.shape = NA, coef = 0, width = 0.6, 
               alpha = 0.3, position = position_dodge(width = 0.75), na.rm = TRUE) +           # Box plot without whiskers and outliers
  theme_minimal()

ggplot(data_subset, aes(x = bpm_t_scr_internal_r, y = cbcl_scr_syn_internal_r)) +
  geom_jitter(alpha = 0.05, color = "black") +                                                  # Add jitter for scatter plot
  geom_smooth(method = "lm", color = "red", na.rm = TRUE) +                                    # Linear fit
  geom_boxplot(aes(group = bpm_t_scr_internal_r), outlier.shape = NA, coef = 0, width = 0.6, 
               alpha = 0.3, position = position_dodge(width = 0.75), na.rm = TRUE) +           # Box plot without whiskers and outliers                                                                # Set y-axis limits
  theme_minimal()

ggplot(data_subset, aes(x = bpm_y_scr_internal_r, y = cbcl_scr_syn_internal_r)) +
  geom_jitter(alpha = 0.05, color = "black") +                                                  # Add jitter for scatter plot
  geom_smooth(method = "lm", color = "red", na.rm = TRUE) +                                    # Linear fit
  geom_boxplot(aes(group = bpm_y_scr_internal_r), outlier.shape = NA, coef = 0, width = 0.6, 
               alpha = 0.3, position = position_dodge(width = 0.75), na.rm = TRUE) +           # Box plot without whiskers and outliers                                                                # Set y-axis limits
  theme_minimal()


####Other measures for internaling
####Teacher BPM
m_s_null_bpmt <- gamm4(bpm_t_scr_internal_r ~ interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                      demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                    random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

m_s_bmi_full_bpmt <- gamm4(bpm_t_scr_internal_r ~ s(bmi) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                          demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                        random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

m_s_whtr_full_bpmt <- gamm4(bpm_t_scr_internal_r ~ s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                           demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                         random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

m_s_whtr_bmi_full_bpmt <- gamm4(bpm_t_scr_internal_r ~ s(bmi) + s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                               demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                             random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

pushover("Done T BPM")

####Youth BPM
m_s_null_bpmy <- gamm4(bpm_y_scr_internal_r ~ interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                      demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                    random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

m_s_bmi_full_bpmy <- gamm4(bpm_y_scr_internal_r ~ s(bmi) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                          demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                        random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

m_s_whtr_full_bpmy <- gamm4(bpm_y_scr_internal_r ~ s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                           demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                         random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

m_s_whtr_bmi_full_bpmy <- gamm4(bpm_y_scr_internal_r ~ s(bmi) + s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                               demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                             random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

pushover("Done T BPM")

####Other measures for internaling
####T-score
m_s_null_t <- gamm4(cbcl_scr_syn_internal_t ~ interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                    demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                  random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

m_s_bmi_full_t <- gamm4(cbcl_scr_syn_internal_t ~ s(bmi) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                        demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                      random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

m_s_whtr_full_t <- gamm4(cbcl_scr_syn_internal_t ~ s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                         demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                       random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

m_s_whtr_bmi_full_t <- gamm4(cbcl_scr_syn_internal_t ~ s(bmi) + s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                             demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                           random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

pushover("Done Y BPM")

####Transform #1: van der Waerden transformation
dat_cleaned$cbcl_scr_syn_internal_rank_inverse_norm <- qnorm((rank(dat_cleaned$cbcl_scr_syn_internal_r) - 0.5) / length(dat_cleaned$cbcl_scr_syn_internal_r))

m_s_null_rin <- gamm4(cbcl_scr_syn_internal_rank_inverse_norm ~ interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                    demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                  random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

m_s_bmi_full_rin <- gamm4(cbcl_scr_syn_internal_rank_inverse_norm ~ s(bmi) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                        demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                      random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

m_s_whtr_full_rin <- gamm4(cbcl_scr_syn_internal_rank_inverse_norm ~ s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                         demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                       random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

m_s_whtr_bmi_full_rin <- gamm4(cbcl_scr_syn_internal_rank_inverse_norm ~ s(bmi) + s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                             demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                           random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

pushover("Done RIN")

####Transform #2
dat_cleaned$cbcl_scr_syn_internal_asinh <-  asinh(dat_cleaned$cbcl_scr_syn_internal_r)

m_s_null_asinh <- gamm4(cbcl_scr_syn_internal_asinh ~ interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                    demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                  random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

m_s_bmi_full_asinh <- gamm4(cbcl_scr_syn_internal_asinh ~ s(bmi) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                        demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                      random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

m_s_whtr_full_asinh <- gamm4(cbcl_scr_syn_internal_asinh ~ s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                         demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                       random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

m_s_whtr_bmi_full_asinh <- gamm4(cbcl_scr_syn_internal_asinh ~ s(bmi) + s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                             demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                           random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

pushover("Done ASINH")


######IN MAIN (FIGURE 1E)
##Identifying linear factors
m_s2_whtr_bmi_full <- gamm4(cbcl_scr_syn_internal_r ~ bmi + s(bmi, m=c(2,0)) + whtr + s(whtr, m=c(2,0)) + interview_age + s(interview_age, m=c(2,0))
                            + reshist_addr1_adi_wsum + s(reshist_addr1_adi_wsum, m=c(2,0)) + demo_max_ed_v2 + demo_max_income_v2 +
                              s(demo_max_income_v2, m=c(2,0)) + demo_sex_v2 + demo_ethn_v2 + pds_p_ss_category_2 + race_cleaned,
                           random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

#####DIfferent error models, sex-specific
m_s_null <- gamm4(cbcl_scr_syn_internal_r ~ interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                    demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                  random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

m_s_null_pois <- gamm4(cbcl_scr_syn_internal_r ~ interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                    demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                  random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), family = poisson(link = "log"), data = dat_cleaned, REML = TRUE)

m_s_null_M <- gamm4(cbcl_scr_syn_internal_r ~ interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                          demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                        random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned %>%
                          filter(demo_sex_v2 == "male"), REML = TRUE)

m_s_null_F <- gamm4(cbcl_scr_syn_internal_r ~ interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                          demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                        random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned %>%
                          filter(demo_sex_v2 == "female"), REML = TRUE)

m_s_bmi_full <- gamm4(cbcl_scr_syn_internal_r ~ s(bmi) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                        demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                      random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

m_s_bmi_full_pois <- gamm4(cbcl_scr_syn_internal_r ~ s(bmi) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                        demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                      random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), family = poisson(link = "log"), data = dat_cleaned, REML = TRUE)

m_s_bmi_full_sex <- gamm4(cbcl_scr_syn_internal_r ~ s(bmi, by = demo_sex_v2) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                             demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                           random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

m_s_bmi_full_M <- gamm4(cbcl_scr_syn_internal_r ~ s(bmi) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                           demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                         random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned %>%
                          filter(demo_sex_v2 == "male"), REML = TRUE)

m_s_bmi_full_F <- gamm4(cbcl_scr_syn_internal_r ~ s(bmi) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                           demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                         random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned %>%
                          filter(demo_sex_v2 == "female"), REML = TRUE)

m_s_whtr_full <- gamm4(cbcl_scr_syn_internal_r ~ s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                         demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                       random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

m_s_whtr_full_pois <- gamm4(cbcl_scr_syn_internal_r ~ s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                         demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                       random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), family = poisson(link = "log"), data = dat_cleaned, REML = TRUE)

m_s_whtr_full_sex <- gamm4(cbcl_scr_syn_internal_r ~ s(whtr, by = demo_sex_v2) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                         demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                       random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

m_s_whtr_full_M <- gamm4(cbcl_scr_syn_internal_r ~ s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                         demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                       random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned %>%
                         filter(demo_sex_v2 == "male"), REML = TRUE)

m_s_whtr_full_F <- gamm4(cbcl_scr_syn_internal_r ~ s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                         demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                       random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned %>%
                         filter(demo_sex_v2 == "female"), REML = TRUE)

m_s_whtr_bmi_full <- gamm4(cbcl_scr_syn_internal_r ~ s(bmi) + s(whtr) + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                         demo_sex_v2 + demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned,
                       random = ~(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)

m_s_whtr_full_sex_lmer <- lmer(cbcl_scr_syn_internal_r ~ whtr * demo_sex_v2 + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                                 demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned + (1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)
m_s_whtr_full_nosex_lmer <- lmer(cbcl_scr_syn_internal_r ~ whtr + demo_sex_v2 + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                                 demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned + (1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)
anova(m_s_whtr_full_nosex_lmer, m_s_whtr_full_sex_lmer)

m_s_whtr_full_age_lmer <- lmer(cbcl_scr_syn_internal_r ~ whtr * interview_age + demo_sex_v2 + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                                 demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned + (1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)
m_s_whtr_full_noage_lmer <- lmer(cbcl_scr_syn_internal_r ~ whtr + demo_sex_v2 + interview_age + reshist_addr1_adi_wsum + demo_max_ed_v2 + demo_max_income_v2 +
                                   demo_ethn_v2  + pds_p_ss_category_2 + race_cleaned + (1|src_subject_id) + (1|site_id_l) + (1|rel_family_id), data = dat_cleaned, REML = TRUE)
anova(m_s_whtr_full_noage_lmer, m_s_whtr_full_age_lmer)
