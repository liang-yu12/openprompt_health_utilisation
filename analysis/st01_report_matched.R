# Source data management
source("analysis/dm01_matched_current_data.R")

# report numbers: 
matched_data %>% summary_factorlist(
      dependent = "exposure",
      explanatory = c("sex", "age","age_cat", "ethnicity_6", "bmi_cat", "imd_q5", 
                      "long_covid_dx", "covid_positive","previous_covid_hosp",
                      "number_comorbidities_cat"),
      p = TRUE
) %>% write.csv(here("output", "matched_numbers_table.csv"), row.names = F)
