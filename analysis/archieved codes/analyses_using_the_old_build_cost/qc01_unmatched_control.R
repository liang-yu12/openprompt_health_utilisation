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
                             index_date = col_date(format = "%Y-%m-%d"),
                             end_death = col_date(format = "%Y-%m-%d"),
                             end_deregist = col_date(format = "%Y-%m-%d"),
                             end_lc_cure = col_date(format = "%Y-%m-%d")
                             )
)
lc_exp_raw$exposure <- 1

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
lc_com_raw$index_date <- as.Date("2020-11-01")
lc_com_raw <- lc_com_raw %>% relocate(index_date, .before = end_death)
lc_com_raw$exposure <- 0

# combind and explore
unmatched_all <- bind_rows(lc_exp_raw, lc_com_raw)
unmatched_all$exposure <- as.factor(unmatched_all$exposure)

unmatched_all <- unmatched_all %>% 
      mutate(wrong_death = ifelse((as.numeric(index_date) > as.numeric(end_death)), 1, 0)) %>% 
      mutate(wrong_regist= ifelse((as.numeric(index_date) > as.numeric(end_deregist)), 1,0)) %>% 
      mutate(wrong_lc_cure= ifelse((as.numeric(index_date) > as.numeric(end_lc_cure)), 1,0)) 

# generate basic summarised table:
dependent = "exposure"
explanatory = c("sex", "age", "wrong_death", "wrong_regist", "wrong_lc_cure")

unmatched_results <- unmatched_all %>% summary_factorlist(dependent, explanatory) 
unmatched_results %>% write_csv(here("output", "qc01_check_unmatched.csv"))
