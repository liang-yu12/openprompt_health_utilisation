# Load all packages
source("analysis/settings_packages.R")

# Read in data-sets:
# # Exposure:
lc_exp_matched <- fread(here("output", "matched_cases_with_ehr.csv"), 
                        colClasses = list(
                              integer = 
                                    c("age", "sex", "long_covid_dx",
                                      "set_id", "exposure", "covid_positive", "ethnicity",
                                      "covid_dx_month","imd", "previous_covid_hosp",   "cov_c19_vaccine_number",
                                      "cov_cancer",   "cov_mental_health",   "cov_asthma",
                                      "cov_organ_transplant",   "cov_chronic_cardiac_disease",   "cov_chronic_liver_disease",
                                      "cov_stroke_dementia",   "cov_other_neuro_diseases",   "cov_ra_sle_psoriasis",
                                      "cov_asplenia",   "cov_hiv",   "cov_aplastic_anemia",   "cov_permanent_immune_suppress",
                                      "cov_temporary_immune_suppress",   "gp_visit_m1",   "gp_visit_m2",   "gp_visit_m3",   "gp_visit_m4",   "gp_visit_m5",
                                      "gp_visit_m6",   "gp_visit_m7",   "gp_visit_m8",   "gp_visit_m9",   "gp_visit_m10",
                                      "gp_visit_m11",   "gp_visit_m12",   "hos_visit_m1",   "hos_visit_m2",   "hos_visit_m3",
                                      "hos_visit_m4",   "hos_visit_m5",   "hos_visit_m6",   "hos_visit_m7",   "hos_visit_m8",
                                      "hos_visit_m9",   "hos_visit_m10",   "hos_visit_m11",   "hos_visit_m12",   "ae_visit_m1",
                                      "ae_visit_m2",   "ae_visit_m3",   "ae_visit_m4",   "ae_visit_m5",   "ae_visit_m6", 
                                      "ae_visit_m7",   "ae_visit_m8",   "ae_visit_m9",   "ae_visit_m10",   "ae_visit_m11",
                                      "ae_visit_m12"
                                    ),
                              character = c("region", "gp_practice", "patient_id"),
                              Date = c("long_covid_dx_date", "index_date", "end_death", 
                                       "end_deregist", "end_lc_cure",
                                       "bmi_date"),
                              double = c("bmi")
                              
                        )
                        
)
lc_exp_matched %>% names
lc_exp_matched$match_counts <- NULL

#  # Comparators:
com_matched <- fread(here("output", "matched_control_with_ehr.csv"),
                     colClasses = list(
                           integer = 
                                 c("age", "sex", "long_covid_dx",
                                   "set_id", "exposure", "covid_positive", "ethnicity",
                                   "covid_dx_month","imd", "previous_covid_hosp",   "cov_c19_vaccine_number",
                                   "cov_cancer",   "cov_mental_health",   "cov_asthma",
                                   "cov_organ_transplant",   "cov_chronic_cardiac_disease",   "cov_chronic_liver_disease",
                                   "cov_stroke_dementia",   "cov_other_neuro_diseases",   "cov_ra_sle_psoriasis",
                                   "cov_asplenia",   "cov_hiv",   "cov_aplastic_anemia",   "cov_permanent_immune_suppress",
                                   "cov_temporary_immune_suppress",   "gp_visit_m1",   "gp_visit_m2",   "gp_visit_m3",   "gp_visit_m4",   "gp_visit_m5",
                                   "gp_visit_m6",   "gp_visit_m7",   "gp_visit_m8",   "gp_visit_m9",   "gp_visit_m10",
                                   "gp_visit_m11",   "gp_visit_m12",   "hos_visit_m1",   "hos_visit_m2",   "hos_visit_m3",
                                   "hos_visit_m4",   "hos_visit_m5",   "hos_visit_m6",   "hos_visit_m7",   "hos_visit_m8",
                                   "hos_visit_m9",   "hos_visit_m10",   "hos_visit_m11",   "hos_visit_m12",   "ae_visit_m1",
                                   "ae_visit_m2",   "ae_visit_m3",   "ae_visit_m4",   "ae_visit_m5",   "ae_visit_m6", 
                                   "ae_visit_m7",   "ae_visit_m8",   "ae_visit_m9",   "ae_visit_m10",   "ae_visit_m11",
                                   "ae_visit_m12"
                                 ),
                           character = c("region", "gp_practice", "patient_id"),
                           Date = c("long_covid_dx_date", "index_date", "end_death", 
                                    "end_deregist", "end_lc_cure",
                                    "bmi_date"),
                           double = c("bmi")
                           
                     )
                     
)



#  combine two datasets
matched_data <- bind_rows(lc_exp_matched, com_matched)
matched_data %>% names

matched_data$exposure <- matched_data$exposure %>% 
      factor(label = c("Comparator", "Long COVID exposure"))

matched_data <- matched_data %>% mutate(
      imd_q5 = cut2(
            imd, 
            g = 5, 
            lebels = c(
                  "least_deprived", 
                  "2_deprived",
                  "3_deprived",
                  "4_deprived",
                  "most_deprived")),
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
