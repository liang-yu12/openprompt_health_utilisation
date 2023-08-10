# Load all packages
source("analysis/settings_packages.R")
# unmatched exposure
lc_exp_raw <- read_csv(here("output", "dataset_exp_lc_unmatched.csv"),
                       col_types = cols( 
                             patient_id = col_double(),
                             age = col_double(),
                             sex = col_factor(),
                             region = col_factor(),
                             registration_date = col_date(format = "%Y-%m-%d"),
                             long_covid_dx = col_double(),
                             long_covid_dx_date = col_date(format = "%Y-%m-%d"),
                             index_date = col_skip(),
                             end_death = col_date(format = "%Y-%m-%d"),
                             end_deregist = col_date(format = "%Y-%m-%d"),
                             end_lc_cure = col_date(format = "%Y-%m-%d")
                             )
)

lc_exp_raw <- lc_exp_raw %>% mutate(wrong_date = ifelse((as.numeric(long_covid_dx_date) > as.numeric(end_death) |
                                                            as.numeric(long_covid_dx_date) > as.numeric(end_deregist) |
                                                            as.numeric(long_covid_dx_date) > as.numeric(end_lc_cure)), 1, 0)) %>% 
      mutate(lc_yes=ifelse(!is.na(long_covid_dx_date), 1,0)) %>% mutate(exposure = "long COVID")
lc_exp_raw$wrong_date <- as.factor(lc_exp_raw$wrong_date)
lc_exp_raw$lc_yes <- as.factor(lc_exp_raw$lc_yes)




# unmatched comparator
lc_com_raw <- read_csv(here("output", "dataset_comparator_unmatched.csv"),
                       col_types = cols( 
                             patient_id = col_double(),
                             age = col_double(),
                             sex = col_factor(),
                             region = col_factor(),
                             registration_date = col_date(format = "%Y-%m-%d"),
                             long_covid_dx = col_double(),
                             long_covid_dx_date = col_date(format = "%Y-%m-%d"),
                             end_death = col_date(format = "%Y-%m-%d"),
                             end_deregist = col_date(format = "%Y-%m-%d"),
                             end_lc_cure = col_date(format = "%Y-%m-%d")
                              )
)

lc_com_raw <- lc_com_raw %>% mutate(wrong_date = ifelse((as.numeric(long_covid_dx_date) > as.numeric(end_death) |
                                                                as.numeric(long_covid_dx_date) > as.numeric(end_deregist) |
                                                                as.numeric(long_covid_dx_date) > as.numeric(end_lc_cure)), 1, 0)) %>% 
      mutate(lc_yes=ifelse(!is.na(long_covid_dx_date), 1,0)) %>% mutate(exposure = "Comparison")

lc_com_raw$wrong_date <- as.factor(lc_com_raw$wrong_date)
lc_com_raw$lc_yes <- as.factor(lc_com_raw$lc_yes)


# combind and explore
unmatched_all <- bind_rows(lc_exp_raw, lc_com_raw)
unmatched_all$exposure <- as.factor(unmatched_all$exposure)

# generate basic summarised table:
dependent = "exposure"
explanatory = c("sex", "age", "lc_yes", "wrong_date")

unmatched_results <- unmatched_all %>% summary_factorlist(dependent, explanatory) 
unmatched_results %>% write_csv(here("output", "qc01_check_unmatched.csv"))
