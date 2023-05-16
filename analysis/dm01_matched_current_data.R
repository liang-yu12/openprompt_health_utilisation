# Load all packages
source("analysis/settings_packages.R")

# Read in data-sets:
# # Exposure:
lc_exp_matched <- fread(here("output", "matched_cases_with_ehr.csv"))
lc_exp_matched$match_counts <- NULL

#  # Comparators:
com_matched <- fread(here("output", "matched_control_with_ehr.csv"))

#  combine two datasets
matched_data <- bind_rows(lc_exp_matched, com_matched)
matched_data %>% names

# check the data type
matched_data %>% glimpse # some types need to be corrected. 

to_be_factors <- c("sex", "region", "gp_practice", "exposure", "covid_positive", "ethnicity",
  "previous_covid_hosp","cov_cancer",  "cov_mental_health",   "cov_asthma",
  "cov_organ_transplant",   "cov_chronic_cardiac_disease",   "cov_chronic_liver_disease",
  "cov_stroke_dementia",   "cov_other_neuro_diseases",   "cov_ra_sle_psoriasis",
  "cov_asplenia",   "cov_hiv",   "cov_aplastic_anemia",   "cov_permanent_immune_suppress",
  "cov_temporary_immune_suppress")

matched_data[, (to_be_factors) := lapply(.SD, as.factor), .SDcols = to_be_factors]

c19_vax_dates <- c("covid_vacc_1_vacc_tab", 
                   "covid_vacc_2_vacc_tab", 
                   "covid_vacc_3_vacc_tab", 
                   "covid_vacc_4_vacc_tab", 
                   "covid_vacc_5_vacc_tab", 
                   "covid_vacc_6_vacc_tab")

# Added non-NA vaccine dates together. 
matched_data$cov_covid_vaccine_number <- rowSums(!is.na(matched_data[, c19_vax_dates, with = FALSE]), na.rm = T)
matched_data <- matched_data %>% 
      mutate(cov_covid_vax_n_cat = case_when(
            cov_covid_vaccine_number == 0 ~ 0,
            cov_covid_vaccine_number == 1 ~ 1,
            cov_covid_vaccine_number == 2 ~ 2,
            cov_covid_vaccine_number >= 3 ~ 3)
)
# Change covid vax numbers into categorical vars 
matched_data$cov_covid_vax_n_cat <- matched_data$cov_covid_vax_n_cat %>% 
      factor(labels = c("0 dose","1 dose","2 doses"," More than 3 doses"))

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

