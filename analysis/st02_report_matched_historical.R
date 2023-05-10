# Load data management files
source("analysis/dm02_matched_hx_data.R")

# report numbers: 
hx_matched_data %>% summary_factorlist(
      dependent = "exposure",
      explanatory = c("sex", "age", "age_cat", "ethnicity_6", "bmi_cat", "imd_q5", 
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


