working_dir="/Users/rasmussj/Documents/PaperSubmission/MET_PSYCH/Analysis_5_1/"
setwd(working_dir)
rm(list = ls())

library(dplyr)
library(lubridate)

#Read in and select on demographics
dat_dems <- read.csv("abcd_p_demo.csv") 

#Propgating sex out to other visits
baseline_sex <- dat_dems %>%
  filter(eventname == "baseline_year_1_arm_1") %>%
  select(src_subject_id, demo_sex_v2)  
dat_dems <- dat_dems %>%
  left_join(baseline_sex, by = "src_subject_id") %>%
  mutate(demo_sex_v2 = coalesce(demo_sex_v2.x, demo_sex_v2.y)) %>%
  select(-demo_sex_v2.x, -demo_sex_v2.y)
rm(baseline_sex)

#Simplifying race based on the lit. precedence and propgating  out to other visits
baseline_race <- dat_dems %>%
  filter(eventname == "baseline_year_1_arm_1") %>%
  select(src_subject_id, contains("demo_race_a_p"))  
baseline_race <- baseline_race %>%
  mutate(
    race_cleaned = case_when(
      demo_race_a_p___10 == 1 ~ "White",        # When this column is 1, 'race_cleaned' should be "White"
      demo_race_a_p___11 == 1 ~ "Black",        # When this column is 1, 'race_cleaned' should be "Black"
      TRUE ~ "Other/Unknown"                    # Otherwise, it should be "Other/Unknown"
    )
  )  %>%
  select(src_subject_id, contains("race_cleaned"))  
dat_dems <- dat_dems %>%
  left_join(baseline_race, by = "src_subject_id") #%>%
# mutate(race_cleaned = coalesce(race_cleaned.x, race_cleaned.y)) %>%
# select(-race_cleaned.x, -race_cleaned.y)
rm(baseline_race)

#Propgating ethnicity out to other visits
baseline_ethn <- dat_dems %>%
  filter(eventname == "baseline_year_1_arm_1") %>%
  select(src_subject_id, demo_ethn_v2)  
dat_dems <- dat_dems %>%
  left_join(baseline_ethn, by = "src_subject_id") %>%
  mutate(demo_ethn_v2 = coalesce(demo_ethn_v2.x, demo_ethn_v2.y)) %>%
  select(-demo_ethn_v2.x, -demo_ethn_v2.y)
rm(baseline_ethn)

#Propgating income out to other visits
baseline_inc <- dat_dems %>%
  filter(eventname == "baseline_year_1_arm_1") %>%
  select(src_subject_id, contains("income"))
baseline_inc <- baseline_inc %>%
  mutate(demo_comb_income_v2 = na_if(demo_comb_income_v2, 777)) %>%
  mutate(demo_comb_income_v2 = na_if(demo_comb_income_v2, 999)) %>%
  select(src_subject_id, demo_comb_income_v2)
dat_dems <- dat_dems %>%
  left_join(baseline_inc, by = "src_subject_id") %>%
  mutate(demo_comb_income_v2 = coalesce(demo_comb_income_v2.x, demo_comb_income_v2.y)) %>%
  select(-demo_comb_income_v2.x, -demo_comb_income_v2.y)
rm(baseline_inc)

inc_2y <- dat_dems %>%
  filter(eventname == "2_year_follow_up_y_arm_1") %>%
  select(src_subject_id, demo_comb_income_v2_l)
inc_2y <- inc_2y %>%
  mutate(demo_comb_income_v2_l = na_if(demo_comb_income_v2_l, 777)) %>%
  mutate(demo_comb_income_v2_l = na_if(demo_comb_income_v2_l, 999)) %>%
  select(src_subject_id, demo_comb_income_v2_l)
dat_dems <- dat_dems %>%
  left_join(inc_2y, by = "src_subject_id") %>%
  mutate(demo_comb_income_v2_l = coalesce(demo_comb_income_v2_l.x, demo_comb_income_v2_l.y)) %>%
  select(-demo_comb_income_v2_l.x, -demo_comb_income_v2_l.y)
rm(inc_2y)

dat_dems <- dat_dems %>%
  mutate(demo_comb_income_v2_l = na_if(demo_comb_income_v2_l, 777)) %>%
  mutate(demo_comb_income_v2_l = na_if(demo_comb_income_v2_l, 999))  %>%
  mutate(demo_comb_income_v2 = na_if(demo_comb_income_v2, 777)) %>%
  mutate(demo_comb_income_v2 = na_if(demo_comb_income_v2, 999)) 
dat_dems <- dat_dems %>%
  mutate(demo_max_income_v2 = pmax(demo_comb_income_v2, demo_comb_income_v2_l, na.rm = TRUE)) %>%
  select(-demo_comb_income_v2, -demo_comb_income_v2_l)

#Propgating education out to other visits
baseline_edu <- dat_dems %>%
  filter(eventname == "baseline_year_1_arm_1") %>%
  select(src_subject_id, demo_prnt_ed_v2, demo_prtnr_ed_v2)  
baseline_edu <- baseline_edu %>%
  mutate(demo_prnt_ed_v2 = na_if(demo_prnt_ed_v2, 777)) %>%
  mutate(demo_prnt_ed_v2 = na_if(demo_prnt_ed_v2, 999))  %>%
  mutate(demo_prtnr_ed_v2 = na_if(demo_prtnr_ed_v2, 777)) %>%
  mutate(demo_prtnr_ed_v2 = na_if(demo_prtnr_ed_v2, 999)) 
baseline_edu <- baseline_edu %>%
  mutate(demo_max_ed_v2 = pmax(demo_prnt_ed_v2, demo_prtnr_ed_v2, na.rm = TRUE)) %>%
  select(-demo_prtnr_ed_v2, -demo_prnt_ed_v2)
baseline_edu <- baseline_edu %>%
  mutate(demo_max_ed_v2 = case_when(
    demo_max_ed_v2 < 13 ~ 12,    # Set values less than 13 to 12
    TRUE ~ demo_max_ed_v2        # Keep all other values as they are
  ))
dat_dems <- dat_dems %>%
  left_join(baseline_edu, by = "src_subject_id")
rm(baseline_edu)

dat_dems <- dat_dems %>%
  select(
    contains("src_subject_id"),
    contains("eventname"),
    contains("demo_ethn_v2"),
    contains("race_cleaned"),
    contains("demo_max_ed_v2"),
    contains("demo_max_income_v2"),
    contains("sex") #####   1 is male, 2 is female
  )

dat_adi <- read.csv("led_l_adi.csv") 
dat_adi <- dat_adi %>%
  select(
    contains("src_subject_id"),
    contains("reshist_addr1_adi_wsum")
  )

#Read in and select on anthro
dat_anthro <- read.csv("ph_y_anthro.csv") 
dat_anthro$bmi <- (dat_anthro$anthroweightcalc*703)/(dat_anthro$anthroheightcalc^2)
dat_anthro$whtr <- (dat_anthro$anthro_waist_cm)/(dat_anthro$anthroheightcalc)

#Cleaning based on visual inspection of extreme values, represents less 0.9% of the total data
dat_anthro <- dat_anthro %>%
  filter( !is.na(anthroweightcalc)  & !is.na(anthroheightcalc) & !is.na(anthro_waist_cm)
          & anthroweightcalc > 45 & anthroheightcalc > 45 & anthroheightcalc < 72 &
            bmi < 45 & bmi > 10 & anthro_waist_cm > 19 & whtr > 0.3  & whtr < 0.9)

dat_anthro <- dat_anthro %>%
  select(
    contains("src_subject_id"),
    contains("eventname"),
    contains("bmi"),
    # contains("anthroweightcalc"),
    # contains("anthroheightcalc"),
    # contains("anthro_waist_cm"),
    contains("whtr")
  )

#Read in and select on internalizing
dat_cbcl <- read.csv("mh_p_cbcl.csv") 
dat_cbcl <- dat_cbcl %>%
  select(
    contains("src_subject_id"),
    contains("eventname"),
    contains("cbcl_scr_syn_internal_r")
  )

#Read in and score puberty
dat_ppdms <- read.csv("ph_p_pds.csv") 
dat_ppdms <- dat_ppdms %>%
  select(
    contains("src_subject_id"),
    contains("eventname"),
    contains("category_2")
  )
dat_ppdms <- dat_ppdms %>%
  mutate(pds_p_ss_category_2 = coalesce(pds_p_ss_female_category_2, pds_p_ss_male_category_2)) %>%
  select(-pds_p_ss_male_category_2, -pds_p_ss_female_category_2)

#Read in and select on interview data
dat_age <- read.csv("abcd_y_lt.csv") 
#Propgating rel_family_id out to other visits
baseline_fam <- dat_age %>%
  filter(eventname == "baseline_year_1_arm_1") %>%
  select(src_subject_id, rel_family_id)  # replace 'id_column' with your actual ID column name
dat_age <- dat_age %>%
  left_join(baseline_fam, by = "src_subject_id") %>%
  mutate(rel_family_id = coalesce(rel_family_id.x, rel_family_id.y)) %>%
  select(-rel_family_id.x, -rel_family_id.y)
rm(baseline_fam)
#Propgating site out to other visits
baseline_site <- dat_age %>%
  filter(eventname == "baseline_year_1_arm_1") %>%
  select(src_subject_id, site_id_l)  # replace 'id_column' with your actual ID column name
dat_age <- dat_age %>%
  left_join(baseline_site, by = "src_subject_id") %>%
  mutate(site_id_l = coalesce(site_id_l.x, site_id_l.y)) %>%
  select(-site_id_l.x, -site_id_l.y)
rm(baseline_site)
dat_age$converted_date <- as.Date(dat_age$interview_date, format = "%m/%d/%Y")
dat_age <- dat_age %>%
  mutate(year_month = floor_date(converted_date, "month"))

dat_age <- dat_age %>%
  select(
    contains("src_subject_id"),
    contains("eventname"),
    contains("site_id_l"),
    contains("rel_family_id"),
    contains("visit_type"),
    contains("year_month"),
    contains("interview_age")
  )


dat_cleaned <- left_join(dat_dems, dat_anthro, by = c("src_subject_id", "eventname"))
dat_cleaned <- left_join(dat_cleaned, dat_cbcl, by = c("src_subject_id", "eventname"))
dat_cleaned <- left_join(dat_cleaned, dat_age, by = c("src_subject_id", "eventname"))
dat_cleaned <- left_join(dat_cleaned, dat_adi, by = c("src_subject_id"))
dat_cleaned <- left_join(dat_cleaned, dat_ppdms, by = c("src_subject_id", "eventname"))

rm(dat_dems, dat_anthro, dat_cbcl, dat_age, dat_adi, dat_ppdms)

# Recode demo_sex_v2 to "male" or "female"
dat_cleaned <- dat_cleaned %>%
  mutate(demo_sex_v2 = case_when(
    demo_sex_v2 == 1 ~ "male",   #  '1' represents male
    demo_sex_v2 == 2 ~ "female"#, #  '2' represents female
    #TRUE ~ as.character(demo_sex_v2) # Keeps the original value if it's not 1 or 2
  ))

dat_cleaned <- dat_cleaned %>%
  mutate(demo_ethn_v2 = case_when(
    demo_ethn_v2 == 1 ~ "hispanic",   
    demo_ethn_v2 == 2 ~ "nonhispanic"#, 
    # TRUE ~ as.character(demo_ethn_v2) # Keeps the original value if it's not 1 or 2
  ))

dat_cleaned$site_id_l <- factor(dat_cleaned$site_id_l)
dat_cleaned$demo_sex_v2 <- factor(dat_cleaned$demo_sex_v2)
dat_cleaned$demo_max_ed_v2 <- factor(dat_cleaned$demo_max_ed_v2)
dat_cleaned$demo_max_ed_v2 <- relevel(dat_cleaned$demo_max_ed_v2, ref = "18")
dat_cleaned$demo_ethn_v2 <- factor(dat_cleaned$demo_ethn_v2)
dat_cleaned$demo_ethn_v2 <- relevel(dat_cleaned$demo_ethn_v2, ref = "nonhispanic")

dat_cleaned <- dat_cleaned %>%
  filter(!is.na(bmi) & !is.na(whtr) & !is.na(demo_sex_v2) & !is.na(cbcl_scr_syn_internal_r) & !is.na(interview_age)
         & !is.na(demo_ethn_v2) & !is.na(reshist_addr1_adi_wsum) & !is.na(pds_p_ss_category_2)
         & !is.na(demo_max_ed_v2) & !is.na(demo_max_income_v2))

save(dat_cleaned, file = "dat_cleaned_04262024.RData")
