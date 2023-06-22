# Load data management files
source("analysis/dm02_matched_hx_data.R")

# report numbers: 
hx_matched_data %>% summary_factorlist(
      dependent = "exposure",
      explanatory = c("sex", "age_cat", "ethnicity_6", "bmi_cat", "imd_q5", 
                      "region",
                      "number_comorbidities_cat"
                  ),
      p = TRUE
) %>% write.csv(here("output", "st02_hx_matched_numbers_table.csv"), row.names = F)


