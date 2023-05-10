# Load all packages
source("analysis/settings_packages.R")

# Read in data-sets:
visit <- c("gp_visit_m1",   "gp_visit_m2",   "gp_visit_m3",   "gp_visit_m4",   "gp_visit_m5",
           "gp_visit_m6",   "gp_visit_m7",   "gp_visit_m8",   "gp_visit_m9",   "gp_visit_m10",
           "gp_visit_m11",  "gp_visit_m12",  "hos_visit_m1",  "hos_visit_m2",  "hos_visit_m3",
           "hos_visit_m4",  "hos_visit_m5",  "hos_visit_m6",  "hos_visit_m7",  "hos_visit_m8",
           "hos_visit_m9",  "hos_visit_m10", "hos_visit_m11", "hos_visit_m12", "ae_visit_m1",
           "ae_visit_m2",   "ae_visit_m3",   "ae_visit_m4",   "ae_visit_m5",   "ae_visit_m6", 
           "ae_visit_m7",   "ae_visit_m8",   "ae_visit_m9",   "ae_visit_m10",  "ae_visit_m11",
           "ae_visit_m12")

hx_visit <- paste("hx_", visit, sep = "")


# # Exposure:
lc_exp_matched <- fread(here("output", "hx_matched_cases_with_ehr.csv"), 
                        colClasses = list(
                              integer = 
                                    c("age", "sex", "lc_dx",
                                      "imd", "exposure", "ethnicity",
                                      "cov_cancer",   "cov_mental_health",   "cov_asthma",
                                      "cov_organ_transplant",   "cov_chronic_cardiac_disease",   "cov_chronic_liver_disease",
                                      "cov_stroke_dementia",   "cov_other_neuro_diseases",   "cov_ra_sle_psoriasis",
                                      "cov_asplenia",   "cov_hiv",   "cov_aplastic_anemia",   "cov_permanent_immune_suppress",
                                      "cov_temporary_immune_suppress",  visit, hx_visit),
                              character = c("region", "ethnicity"),
                              Date = c( "index_date", "bmi_date"),
                              double = c("bmi")
                              
                        )
                        
)
lc_exp_matched %>% names

#  # Comparators:
com_matched <- fread(here("output", "hx_matched_control_with_ehr.csv"),
                     colClasses = list(
                           integer = 
                                 c("age", "sex", "lc_dx",
                                   "imd", "exposure", "ethnicity",
                                   "cov_cancer",   "cov_mental_health",   "cov_asthma",
                                   "cov_organ_transplant",   "cov_chronic_cardiac_disease",   "cov_chronic_liver_disease",
                                   "cov_stroke_dementia",   "cov_other_neuro_diseases",   "cov_ra_sle_psoriasis",
                                   "cov_asplenia",   "cov_hiv",   "cov_aplastic_anemia",   "cov_permanent_immune_suppress",
                                   "cov_temporary_immune_suppress",  visit, hx_visit),
                           character = c("region", "ethnicity"),
                           Date = c( "index_date", "bmi_date"),
                           double = c("bmi")
                           
                     )
                     
)
com_matched %>% names


#  combine two datasets
hx_matched_data <- bind_rows(lc_exp_matched, com_matched)
hx_matched_data %>% names

hx_matched_data$exposure <- hx_matched_data$exposure %>% 
      factor(label = c("Comparator", "Long COVID exposure"))




# report numbers: 
hx_matched_data %>% summary_factorlist(
      dependent = "exposure",
      explanatory = c("sex", "age", "ethnicity", "bmi", "imd", 
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
                      visit, hx_visit
                  ),
      p = TRUE
) %>% write.csv(here("output", "hx_matched_numbers_table.csv"), row.names = F)


