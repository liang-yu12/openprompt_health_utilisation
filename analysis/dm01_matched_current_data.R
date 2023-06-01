# Load all packages
source("analysis/settings_packages.R")

# ============== Read in data and combine ==============
# Read in data-sets:
# # Exposure:
lc_exp_matched <- fread(here("output", "matched_cases_with_ehr.csv"))
lc_exp_matched$match_counts <- NULL

#  # Comparators:
com_matched <- fread(here("output", "matched_control_with_ehr.csv"))

#  combine two datasets
matched_data <- bind_rows(lc_exp_matched, com_matched)
matched_data %>% names

rm(lc_exp_matched, com_matched) # house keeping

# ============== Data management for each variables 
# check the data type
matched_data %>% glimpse # some types need to be corrected. 

to_be_factors <- c("sex", "region", "gp_practice", "exposure", "covid_positive", "ethnicity",
  "previous_covid_hosp","cov_cancer",  "cov_mental_health",   "cov_asthma",
  "cov_organ_transplant",   "cov_chronic_cardiac_disease",   "cov_chronic_liver_disease",
  "cov_stroke_dementia",   "cov_other_neuro_diseases",   "cov_ra_sle_psoriasis",
  "cov_asplenia",   "cov_hiv",   "cov_aplastic_anemia",   "cov_permanent_immune_suppress",
  "cov_temporary_immune_suppress")

matched_data[, (to_be_factors) := lapply(.SD, as.factor), .SDcols = to_be_factors]

# Calculate vaccine numbers: -------
# Added non-NA vaccine dates together. 
c19_vax_dates <- c("covid_vacc_1_vacc_tab", 
                   "covid_vacc_2_vacc_tab", 
                   "covid_vacc_3_vacc_tab", 
                   "covid_vacc_4_vacc_tab", 
                   "covid_vacc_5_vacc_tab", 
                   "covid_vacc_6_vacc_tab")

matched_data$cov_covid_vaccine_number <- rowSums(!is.na(matched_data[, c19_vax_dates, with = FALSE]))
# categorize the vaccine number
matched_data <- matched_data %>% 
      mutate(cov_covid_vax_n_cat = case_when(
            cov_covid_vaccine_number == 0 ~ 0,
            cov_covid_vaccine_number == 1 ~ 1,
            cov_covid_vaccine_number == 2 ~ 2,
            cov_covid_vaccine_number >= 3 ~ 3)
)
# Change covid vax numbers into categorical vars 
matched_data$cov_covid_vax_n_cat <- matched_data$cov_covid_vax_n_cat %>% 
      factor(labels = c("0 dose","1 dose","2 doses","3 or more doses"))

table(matched_data$cov_covid_vax_n_cat, matched_data$cov_covid_vaccine_number, useNA = "ifany")      
                                        
# Label exposure indicator
matched_data$exposure <- matched_data$exposure %>% 
      factor(labels = c("Comparator", "Long covid exposure"))
matched_data$exposure %>% table

# Other data management: IMD quintiles, ethnicity, BMI categories for ethnicity  
matched_data <- matched_data %>% mutate(
      imd_q5 = cut2(imd, g = 5),
      ethnicity_6 = factor(
            ethnicity,
            levels = 1:6, 
            labels = c(
                  "White",
                  "Mixed", 
                  "South Asian", 
                  "Black",
                  "Other",
                  "Not stated")),
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
levels(matched_data$imd_q5) <- c("least_deprived",
                                 "2_deprived",
                                 "3_deprived",
                                 "4_deprived",
                                 "most_deprived")

table(matched_data$exposure)

# ============== Define the end date ============== 
# Correct the deregist date 
matched_data$end_deregist <- as.IDate(matched_data$end_deregist)

matched_data$all_study_end_date <- as.Date("2023-01-31")

# data management of some unreasonable dates.
matched_data$index_date %>% summary

# Only keep people who are 1. alive 2. registered 3 haven't recovered from lc on the index date

matched_data <- matched_data %>% filter(
      ((end_death > index_date) | is.na(end_death)) & 
            ((end_deregist > index_date) | is.na(end_deregist) )& 
            ((end_lc_cure > index_date) | is.na(end_lc_cure))
)


# censored the comparator group if they were diagnosed with long COVID.
matched_data <- matched_data %>% 
      mutate(
      end_date = ifelse(
            exposure == "Comparator",
            pmin(as.numeric(end_death), 
                 as.numeric(end_deregist), 
                 as.numeric(long_covid_dx_date), 
                 as.numeric(all_study_end_date), na.rm = T),
            pmin(as.numeric(end_death), 
                 as.numeric(end_deregist), 
                 as.numeric(end_lc_cure), 
                 as.numeric(all_study_end_date), na.rm = T)
      )
)
matched_data$end_date <- as.Date.numeric(matched_data$end_date, origin = "1970-01-01")
matched_data$end_date %>% summary
matched_data$index_date %>% summary



# calculate follow-up time
matched_data$follow_up_time <- as.numeric(matched_data$end_date) - as.numeric(matched_data$index_date)
matched_data$follow_up_time %>% summary

matched_data <- matched_data %>% filter(follow_up_time != 0)


# ============== Caclulate the number of comorbidities 

comorbidities <- c("cov_cancer",  "cov_mental_health",   "cov_asthma",
"cov_organ_transplant",   "cov_chronic_cardiac_disease",   "cov_chronic_liver_disease",
"cov_stroke_dementia",   "cov_other_neuro_diseases",   "cov_ra_sle_psoriasis",
"cov_asplenia",   "cov_hiv",   "cov_aplastic_anemia",   "cov_permanent_immune_suppress",
"cov_temporary_immune_suppress")
# change them into logical factors
matched_data[, (comorbidities) := lapply(.SD, as.logical), .SDcols = comorbidities] 


matched_data[, number_comorbidities := rowSums(.SD, na.rm = T), .SDcols = comorbidities] # add them up
matched_data <- matched_data %>% 
      mutate(number_comorbidities_cat = case_when(
            number_comorbidities ==0 ~ 0,
            number_comorbidities ==1 ~ 1,
            number_comorbidities ==2 ~ 2,
            number_comorbidities >=3 ~ 3)) 

matched_data$number_comorbidities_cat <- matched_data$number_comorbidities_cat %>% 
      as.factor()
      
# ============== combine the healthcare visits ============== 
matched_data$all_month1 <- rowSums(matched_data[, grepl("m1$", names(matched_data)), with = FALSE], na.rm = T)
matched_data$all_month2 <- rowSums(matched_data[, grepl("m2$", names(matched_data)), with = FALSE], na.rm = T)
matched_data$all_month3 <- rowSums(matched_data[, grepl("m3$", names(matched_data)), with = FALSE], na.rm = T)
matched_data$all_month4 <- rowSums(matched_data[, grepl("m4$", names(matched_data)), with = FALSE], na.rm = T)
matched_data$all_month5 <- rowSums(matched_data[, grepl("m5$", names(matched_data)), with = FALSE], na.rm = T)
matched_data$all_month6 <- rowSums(matched_data[, grepl("m6$", names(matched_data)), with = FALSE], na.rm = T)
matched_data$all_month7 <- rowSums(matched_data[, grepl("m7$", names(matched_data)), with = FALSE], na.rm = T)
matched_data$all_month8 <- rowSums(matched_data[, grepl("m8$", names(matched_data)), with = FALSE], na.rm = T)
matched_data$all_month9 <- rowSums(matched_data[, grepl("m9$", names(matched_data)), with = FALSE], na.rm = T)
matched_data$all_month10 <- rowSums(matched_data[, grepl("m10$", names(matched_data)), with = FALSE], na.rm = T)
matched_data$all_month11 <- rowSums(matched_data[, grepl("m11$", names(matched_data)), with = FALSE], na.rm = T)
matched_data$all_month12 <- rowSums(matched_data[, grepl("m12$", names(matched_data)), with = FALSE], na.rm = T)
