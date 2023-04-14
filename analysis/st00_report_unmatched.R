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

# report numbers: 
arsenal::tableby(exposure ~ sex + age + long_covid_dx + 
                       is.na(end_death) + is.na(end_deregist),
                 data = unmatched_data, 
                 cat.stats=c("countpct")
) %>% 
      summary(text = T, digits = 2, digits.p = 2
) %>% 
      write2(here("output", "unmatched_numbers.doc"))


