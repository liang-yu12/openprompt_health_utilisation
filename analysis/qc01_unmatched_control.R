# Load all packages
source("analysis/settings_packages.R")

lc_exp_matched <- read_csv(here("output", "dataset_exp_lc_unmatched.csv"), 
                           col_types = cols(
                             patient_id = col_double(),
                             age = col_double(),
                             sex = col_character(),
                             region = col_factor(),
                             gp_practice = col_double(),
                             registration_date = col_date(format = ""),
                             long_covid_dx = col_double(),
                             long_covid_dx_date = col_date(format = ""),
                             index_date = col_date(format = ""),
                             end_death = col_date(format = ""),
                             end_deregist = col_logical(),
                             end_lc_cure = col_date(format = "")
                           ))
