# Source data management
source("analysis/dm01_matched_current_data.R")


dependent = "exposure"
explanatory = c("sex", "age","age_cat", "ethnicity_6", "bmi_cat", "imd_q5", 
                "long_covid_dx", "covid_positive","previous_covid_hosp",
                "cov_covid_vax_n_cat", "number_comorbidities_cat")

# report numbers: 
matched_data %>% summary_factorlist(dependent, explanatory, p = TRUE) %>% 
      write.csv(here("output", "matched_numbers_table.csv"), row.names = F)


# check missing value distribution:
matched_data %>% ff_glimpse(dependent, explanatory) %>% 
      write.csv(here("output", "missing_distribution_table.csv"), row.names = F)


# check missing pattern:
png(file=here("output", "missing_pattern_current.png"),
    width=600, height=600)
matched_data %>% 
      missing_pattern(dependent, explanatory)
dev.off()
