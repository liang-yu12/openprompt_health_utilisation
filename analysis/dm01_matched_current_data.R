# Load all packages
source("analysis/settings_packages.R")

# ============== Read in data and combine ==============
# Read in data-sets:
# # Exposure:

lc_exp_matched <- read_csv("output/matched_cases_with_ehr.csv", 
                           col_types = cols(registration_date = col_date(format = "%Y-%m-%d"), 
                                          long_covid_dx_date = col_date(format = "%Y-%m-%d"), 
                                          index_date = col_date(format = "%Y-%m-%d"), 
                                          end_death = col_date(format = "%Y-%m-%d"), 
                                          end_deregist = col_date(format = "%Y-%m-%d"), 
                                          end_lc_cure = col_date(format = "%Y-%m-%d"), 
                                          covid_vacc_1_vacc_tab = col_date(format = "%Y-%m-%d"), 
                                          covid_vacc_2_vacc_tab = col_date(format = "%Y-%m-%d"), 
                                          covid_vacc_3_vacc_tab = col_date(format = "%Y-%m-%d"), 
                                          covid_vacc_4_vacc_tab = col_date(format = "%Y-%m-%d"), 
                                          covid_vacc_5_vacc_tab = col_date(format = "%Y-%m-%d"), 
                                          covid_vacc_6_vacc_tab = col_date(format = "%Y-%m-%d"), 
                                          covid_dx_month = col_date(format = "%Y-%m-%d"), 
                                          bmi_date = col_date(format = "%Y-%m-%d")))
lc_exp_matched$match_counts <- NULL



# Data management of exposure dataset: --------------

# ============== Data management for each variables 
# check the data type

to_be_factors <- c("sex", "region", "exposure", "covid_positive", "ethnicity",
  "previous_covid_hosp","cov_cancer",  "cov_mental_health",   "cov_asthma",
  "cov_organ_transplant",   "cov_chronic_cardiac_disease",   "cov_chronic_liver_disease",
  "cov_stroke_dementia",   "cov_other_neuro_diseases",   "cov_ra_sle_psoriasis",
  "cov_asplenia",   "cov_hiv",   "cov_aplastic_anemia",   "cov_permanent_immune_suppress",
  "cov_temporary_immune_suppress")

lc_exp_matched[to_be_factors] <- lapply(lc_exp_matched[to_be_factors], as.factor)

# drop unused levels
lc_exp_matched[to_be_factors] <- lapply(lc_exp_matched[to_be_factors], droplevels)


# Calculate vaccine numbers: -------
# Added non-NA vaccine dates together. 
c19_vax_dates <- c("covid_vacc_1_vacc_tab", 
                   "covid_vacc_2_vacc_tab", 
                   "covid_vacc_3_vacc_tab", 
                   "covid_vacc_4_vacc_tab", 
                   "covid_vacc_5_vacc_tab", 
                   "covid_vacc_6_vacc_tab")

lc_exp_matched$cov_covid_vaccine_number <- rowSums(!is.na(lc_exp_matched[, c19_vax_dates, with = FALSE]))
# categorize the vaccine number
lc_exp_matched <- lc_exp_matched %>% 
      mutate(cov_covid_vax_n_cat = case_when(
            cov_covid_vaccine_number == 0 ~ 0,
            cov_covid_vaccine_number == 1 ~ 1,
            cov_covid_vaccine_number == 2 ~ 2,
            cov_covid_vaccine_number >= 3 ~ 3)
)
# Change covid vax numbers into categorical vars 
lc_exp_matched$cov_covid_vax_n_cat <- lc_exp_matched$cov_covid_vax_n_cat %>% 
      factor(labels = c("0 dose","1 dose","2 doses","3 or more doses"))

# Label exposure indicator
levels(lc_exp_matched$exposure) <- c("Comparator", "Long covid exposure")

# Other data management: IMD quintiles, ethnicity, BMI categories for ethnicity  
lc_exp_matched <- lc_exp_matched %>% mutate(
      imd_q5 = cut2(imd, g = 5),
      ethnicity_6 = factor(
            ethnicity,
            levels = 1:5, 
            labels = c(
                  "White",
                  "Mixed", 
                  "South Asian", 
                  "Black",
                  "Other")),
      age_cat = cut(
            age, 
            breaks = c(0, seq(30, 70, 10), Inf),
            labels = c(
                  "18-29",
                  "30-39",
                  "40-49",
                  "50-59",
                  "60-69",
                  "70+")),
      bmi_cat = case_when(
            ethnicity_6 %in% c("South Asian", "Other") ~ cut(bmi, breaks = c(0,18.5,23,27.5,Inf), 
                                                             labels = c(
                                                                  "Underweight", 
                                                                  "Normal Weight", 
                                                                  "Overweight", 
                                                                  "Obese")),
            !ethnicity_6 %in% c("South Asian", "Other") ~ cut(bmi, breaks = c(0,18.5,25,30,Inf),
                                                             labels = c(
                                                                  "Underweight", 
                                                                  "Normal Weight", 
                                                                  "Overweight", 
                                                                  "Obese"))
                        )
)

# label the imd cat
levels(lc_exp_matched$imd_q5) <- c("least_deprived",
                                 "2_deprived",
                                 "3_deprived",
                                 "4_deprived",
                                 "most_deprived")


# ============== Define the end date ============== 

lc_exp_matched$end_date %>% summary
lc_exp_matched$index_date %>% summary

# calculate follow-up time

lc_exp_matched$fu_total <- as.numeric(lc_exp_matched$end_date) - as.numeric(lc_exp_matched$index_date)
lc_exp_matched$fu_total %>% summary

filter(lc_exp_matched, end_date == index_date) %>% nrow() # all follow-up time > 0

## ============== Caclulate the number of comorbidities 

comorbidities <- c("cov_cancer",  "cov_mental_health",   "cov_asthma",
"cov_organ_transplant",   "cov_chronic_cardiac_disease",   "cov_chronic_liver_disease",
"cov_stroke_dementia",   "cov_other_neuro_diseases",   "cov_ra_sle_psoriasis",
"cov_asplenia",   "cov_hiv",   "cov_aplastic_anemia",   "cov_permanent_immune_suppress",
"cov_temporary_immune_suppress")
## change them into logical factors
lc_exp_matched <- lc_exp_matched %>% as_tibble()
lc_exp_matched[comorbidities] <- lapply(lc_exp_matched[comorbidities], as.logical)

lc_exp_matched$number_comorbidities <- rowSums(lc_exp_matched[comorbidities], na.rm = T) # add them up

lc_exp_matched <- lc_exp_matched %>% 
      mutate(number_comorbidities_cat = case_when(
            number_comorbidities ==0 ~ 0,
            number_comorbidities ==1 ~ 1,
            number_comorbidities ==2 ~ 2,
            number_comorbidities >=3 ~ 3)) 

lc_exp_matched$number_comorbidities_cat <- lc_exp_matched$number_comorbidities_cat %>% 
      as.factor()


# ============== combine the healthcare visits ============== 
lc_exp_matched$all_month_m1 <- rowSums(lc_exp_matched[, grepl("m1$", names(lc_exp_matched)), with = FALSE], na.rm = T)
lc_exp_matched$all_month_m2 <- rowSums(lc_exp_matched[, grepl("m2$", names(lc_exp_matched)), with = FALSE], na.rm = T)
lc_exp_matched$all_month_m3 <- rowSums(lc_exp_matched[, grepl("m3$", names(lc_exp_matched)), with = FALSE], na.rm = T)
lc_exp_matched$all_month_m4 <- rowSums(lc_exp_matched[, grepl("m4$", names(lc_exp_matched)), with = FALSE], na.rm = T)
lc_exp_matched$all_month_m5 <- rowSums(lc_exp_matched[, grepl("m5$", names(lc_exp_matched)), with = FALSE], na.rm = T)
lc_exp_matched$all_month_m6 <- rowSums(lc_exp_matched[, grepl("m6$", names(lc_exp_matched)), with = FALSE], na.rm = T)
lc_exp_matched$all_month_m7 <- rowSums(lc_exp_matched[, grepl("m7$", names(lc_exp_matched)), with = FALSE], na.rm = T)
lc_exp_matched$all_month_m8 <- rowSums(lc_exp_matched[, grepl("m8$", names(lc_exp_matched)), with = FALSE], na.rm = T)
lc_exp_matched$all_month_m9 <- rowSums(lc_exp_matched[, grepl("m9$", names(lc_exp_matched)), with = FALSE], na.rm = T)
lc_exp_matched$all_month_m10 <- rowSums(lc_exp_matched[, grepl("m10$", names(lc_exp_matched)), with = FALSE], na.rm = T)
lc_exp_matched$all_month_m11 <- rowSums(lc_exp_matched[, grepl("m11$", names(lc_exp_matched)), with = FALSE], na.rm = T)
lc_exp_matched$all_month_m12 <- rowSums(lc_exp_matched[, grepl("m12$", names(lc_exp_matched)), with = FALSE], na.rm = T)



# Data management of the comparator dataset --------------

#  # Comparators:
com_matched <- read_csv("output/matched_control_with_ehr.csv", 
                        col_types = cols(registration_date = col_date(format = "%Y-%m-%d"), 
                                         long_covid_dx_date = col_date(format = "%Y-%m-%d"), 
                                         index_date = col_date(format = "%Y-%m-%d"), 
                                         end_death = col_date(format = "%Y-%m-%d"), 
                                         end_deregist = col_date(format = "%Y-%m-%d"), 
                                         end_lc_cure = col_date(format = "%Y-%m-%d"), 
                                         covid_vacc_1_vacc_tab = col_date(format = "%Y-%m-%d"), 
                                         covid_vacc_2_vacc_tab = col_date(format = "%Y-%m-%d"), 
                                         covid_vacc_3_vacc_tab = col_date(format = "%Y-%m-%d"), 
                                         covid_vacc_4_vacc_tab = col_date(format = "%Y-%m-%d"), 
                                         covid_vacc_5_vacc_tab = col_date(format = "%Y-%m-%d"), 
                                         covid_vacc_6_vacc_tab = col_date(format = "%Y-%m-%d"), 
                                         covid_dx_month = col_date(format = "%Y-%m-%d"), 
                                         bmi_date = col_date(format = "%Y-%m-%d")))
com_matched[to_be_factors] <- lapply(com_matched[to_be_factors], as.factor)

# drop unused levels
com_matched[to_be_factors] <- lapply(com_matched[to_be_factors], droplevels)


# Calculate vaccine numbers: -------

com_matched$cov_covid_vaccine_number <- rowSums(!is.na(com_matched[, c19_vax_dates, with = FALSE]))
# categorize the vaccine number
com_matched <- com_matched %>% 
      mutate(cov_covid_vax_n_cat = case_when(
            cov_covid_vaccine_number == 0 ~ 0,
            cov_covid_vaccine_number == 1 ~ 1,
            cov_covid_vaccine_number == 2 ~ 2,
            cov_covid_vaccine_number >= 3 ~ 3)
      )
# Change covid vax numbers into categorical vars 
com_matched$cov_covid_vax_n_cat <- com_matched$cov_covid_vax_n_cat %>% 
      factor(labels = c("0 dose","1 dose","2 doses","3 or more doses"))

# Label exposure indicator
levels(com_matched$exposure) <- c("Comparator", "Long covid exposure")

# Other data management: IMD quintiles, ethnicity, BMI categories for ethnicity  
com_matched <- com_matched %>% mutate(
      imd_q5 = cut2(imd, g = 5),
      ethnicity_6 = factor(
            ethnicity,
            levels = 1:5, 
            labels = c(
                  "White",
                  "Mixed", 
                  "South Asian", 
                  "Black",
                  "Other")),
      age_cat = cut(
            age, 
            breaks = c(0, seq(30, 70, 10), Inf),
            labels = c(
                  "18-29",
                  "30-39",
                  "40-49",
                  "50-59",
                  "60-69",
                  "70+")),
      bmi_cat = case_when(
            ethnicity_6 %in% c("South Asian", "Other") ~ cut(bmi, breaks = c(0,18.5,23,27.5,Inf), 
                                                             labels = c(
                                                                   "Underweight", 
                                                                   "Normal Weight", 
                                                                   "Overweight", 
                                                                   "Obese")),
            !ethnicity_6 %in% c("South Asian", "Other") ~ cut(bmi, breaks = c(0,18.5,25,30,Inf),
                                                              labels = c(
                                                                    "Underweight", 
                                                                    "Normal Weight", 
                                                                    "Overweight", 
                                                                    "Obese"))
      )
)

# label the imd cat
levels(com_matched$imd_q5) <- c("least_deprived",
                                 "2_deprived",
                                 "3_deprived",
                                 "4_deprived",
                                 "most_deprived")


# ============== Define the end date ============== 
# data management of some unreasonable dates.
com_matched$end_date %>% summary
com_matched$index_date %>% summary

# calculate follow-up time
com_matched$fu_total <- as.numeric(com_matched$end_date) - as.numeric(com_matched$index_date)
com_matched$fu_total %>% summary

# ============== Caclulate the number of comorbidities 
# change them into logical factors
com_matched <- com_matched %>% as_tibble()
com_matched[comorbidities] <- lapply(com_matched[comorbidities], as.logical)

com_matched$number_comorbidities <- rowSums(com_matched[comorbidities], na.rm = T) # add them up

com_matched <- com_matched %>% 
      mutate(number_comorbidities_cat = case_when(
            number_comorbidities ==0 ~ 0,
            number_comorbidities ==1 ~ 1,
            number_comorbidities ==2 ~ 2,
            number_comorbidities >=3 ~ 3)) 

com_matched$number_comorbidities_cat <- com_matched$number_comorbidities_cat %>% 
      as.factor()


# ============== combine the healthcare visits ============== 
com_matched$all_month_m1 <- rowSums(com_matched[, grepl("m1$", names(com_matched)), with = FALSE], na.rm = T)
com_matched$all_month_m2 <- rowSums(com_matched[, grepl("m2$", names(com_matched)), with = FALSE], na.rm = T)
com_matched$all_month_m3 <- rowSums(com_matched[, grepl("m3$", names(com_matched)), with = FALSE], na.rm = T)
com_matched$all_month_m4 <- rowSums(com_matched[, grepl("m4$", names(com_matched)), with = FALSE], na.rm = T)
com_matched$all_month_m5 <- rowSums(com_matched[, grepl("m5$", names(com_matched)), with = FALSE], na.rm = T)
com_matched$all_month_m6 <- rowSums(com_matched[, grepl("m6$", names(com_matched)), with = FALSE], na.rm = T)
com_matched$all_month_m7 <- rowSums(com_matched[, grepl("m7$", names(com_matched)), with = FALSE], na.rm = T)
com_matched$all_month_m8 <- rowSums(com_matched[, grepl("m8$", names(com_matched)), with = FALSE], na.rm = T)
com_matched$all_month_m9 <- rowSums(com_matched[, grepl("m9$", names(com_matched)), with = FALSE], na.rm = T)
com_matched$all_month_m10 <- rowSums(com_matched[, grepl("m10$", names(com_matched)), with = FALSE], na.rm = T)
com_matched$all_month_m11 <- rowSums(com_matched[, grepl("m11$", names(com_matched)), with = FALSE], na.rm = T)
com_matched$all_month_m12 <- rowSums(com_matched[, grepl("m12$", names(com_matched)), with = FALSE], na.rm = T)
