# Load all packages
source("analysis/settings_packages.R")

# Read in data-sets:
# # Exposure:
lc_exp_matched <- fread(here("output", "matched_cases_with_ehr.csv"), 
                        colClasses = list(
                                    integer = 
                                          c("age", "sex", "long_covid_dx",
                                            "set_id", "exposure", "covid_positive",
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
                                   "set_id", "exposure", "covid_positive",
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



      
# report numbers: 
matched_data %>% summary_factorlist(
      dependent = "exposure",
      explanatory = c("sex", "age", "ethnicity", "bmi", "imd", 
                      "long_covid_dx", "covid_positive","previous_covid_hosp",
                      "cov_c19_vaccine_number",
                      "cov_cancer",
                      "cov_mental_health",
                      "cov_asthma",
                      "cov_organ_transplant",
                      "cov_chronic_cardiac_disease",
                      "cov_chronic_liver_disease",
                      "cov_stroke_dementia",
                      "cov_other_neuro_diseases",
                      "cov_ra_sle_psoriasis",
                      "cov_asplenia",
                      "cov_hiv",
                      "cov_aplastic_anemia",
                      "cov_permanent_immune_suppress",
                      "cov_temporary_immune_suppress",
                      "gp_visit_m1",
                      "gp_visit_m2",
                      "gp_visit_m3",
                      "gp_visit_m4",
                      "gp_visit_m5",
                      "gp_visit_m6",
                      "gp_visit_m7",
                      "gp_visit_m8",
                      "gp_visit_m9",
                      "gp_visit_m10",
                      "gp_visit_m11",
                      "gp_visit_m12",
                      "hos_visit_m1",
                      "hos_visit_m2",
                      "hos_visit_m3",
                      "hos_visit_m4",
                      "hos_visit_m5",
                      "hos_visit_m6",
                      "hos_visit_m7",
                      "hos_visit_m8",
                      "hos_visit_m9",
                      "hos_visit_m10",
                      "hos_visit_m11",
                      "hos_visit_m12",
                      "ae_visit_m1",
                      "ae_visit_m2",
                      "ae_visit_m3",
                      "ae_visit_m4",
                      "ae_visit_m5",
                      "ae_visit_m6",
                      "ae_visit_m7",
                      "ae_visit_m8",
                      "ae_visit_m9",
                      "ae_visit_m10",
                      "ae_visit_m11",
                      "ae_visit_m12"
                      ),
      p = TRUE
) %>% write.csv(here("output", "matched_numbers_table.csv"), row.names = F)


