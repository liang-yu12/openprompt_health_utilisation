# Load all packages
source("analysis/settings_packages.R")

# Read in data-sets:
# # Exposure:
lc_exp_matched <- fread(here("output", "matched_cases_with_ehr.csv"),
                        colClasses = list(
                              integer = c("patient_id", "age", "long_covid_dx","set_id","exposure", "imd"),
                              character = c("region", "gp_practice"),
                              Date = c("long_covid_dx_date", "index_date", "end_death", "end_deregist", "end_lc_cure",
                                       "bmi_date")
                        )
)

lc_exp_matched %>% names

#  # Comparators:
com_matched <- fread(here("output", "matched_control_with_ehr.csv"),
                     colClasses = list(
                           integer = c("patient_id", "age", "long_covid_dx","set_id","exposure", "imd"),
                           character = c("region", "gp_practice"),
                           Date = c("long_covid_dx_date", "index_date", "end_death", "end_deregist", "end_lc_cure",
                                    "bmi_date")
                                       )
)

com_matched %>% names
com_matched$match_counts <- NA

#  combine two datasets
matched_data <- bind_rows(lc_exp_matched, com_matched)
matched_data %>% names

matched_data$exposure <- matched_data$exposure %>% 
      factor(label = c("Comparator", "Long COVID exposure"))

# define some events
matched_data <- matched_data %>% 
      mutate(long_covid = ifelse(long_covid_dx == 1, 1, 0)) %>% # long COVID
      mutate(lc_cure = ifelse(!is.na(end_lc_cure), 1, 0)) %>% #cure lc
      mutate(death = ifelse(!is.na(end_death), 1, 0)) # who died
      
# report numbers: 
matched_data %>% summary_factorlist(
      dependent = "exposure",
      explanatory = c("sex", "age", "ethnicity", "bmi", "imd", "previous_covid_hosp",
                      "long_covid", "lc_cure", "death")
) %>% write.csv(here("output", "matched_numbers_table.csv"), row.names = F)


