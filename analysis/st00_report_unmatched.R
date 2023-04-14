# Load all packages
source("analysis/settings_packages.R")

# Read in data-sets:
# # Exposure:
lc_exp_unmatched <- fread(here("output","dataset_exp_lc_unmatched.csv"),
                          colClasses = c("integer", "integer", "factor", "factor",
                                         "integer", "Date", "integer", "Date",
                                         "Date", "Date", "Date", "Date")
)
lc_exp_unmatched$index_date <- NULL

lc_exp_unmatched$exposure <- 1

#  # Comparators:
com_unmatched <- fread(here("output", "dataset_comparator_unmatched.csv"),
                       colClasses = c("integer", "integer", "factor", "factor",
                                      "integer", "Date", "integer", "Date",
                                      "Date", "Date", "Date")
)

com_unmatched$exposure <- 0


#  combine two datasets
unmatched_data <- rbind(lc_exp_unmatched, com_unmatched)
unmatched_data %>% names

unmatched_data$exposure <- unmatched_data$exposure %>% 
      factor(label = c("Comparator", "Long COVID exposure"))

# define some events
unmatched_data <- unmatched_data %>% 
      mutate(long_covid = ifelse(long_covid_dx == 1, 1, 0)) %>% # long COVID
      mutate(lc_cure = ifelse(!is.na(end_lc_cure), 1, 0)) %>% #cure lc
      mutate(death = ifelse(!is.na(end_death), 1, 0)) # who died
      
# report numbers: 
unmatched_data %>% summary_factorlist(
      dependent = "exposure",
      explanatory = c("sex", "age", "long_covid", "lc_cure", "death")
) %>% write.csv(here("output", "unmatched_numbers.csv"), row.names = F)


