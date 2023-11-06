# Load all packages
source("analysis/settings_packages.R")
# unmatched exposure
hx_lc_exp_raw <- read_csv(here("output", "hx_unmatched_exp.csv"),
                       col_types = cols(
                             patient_id = col_double(),
                             age = col_double(),
                             sex = col_character(),
                             lc_exp = col_double(),
                             index_date = col_date(format = "%Y-%m-%d"),
                             region = col_factor(),
                             end_death = col_date(format = "%Y-%m-%d"),
                             end_deregist = col_date(format = "%Y-%m-%d"),
                             end_lc_cure = col_date(format = "%Y-%m-%d")
                       )
)

hx_lc_exp_raw <- hx_lc_exp_raw %>% rename(exposure = lc_exp)

hx_lc_exp_raw %>% names


# unmatched comparator
hx_lc_com_raw <- read_csv(here("output", "hx_dataset_comp_unmatched.csv"),
                       col_types = cols(
                             patient_id = col_double(),
                             age = col_double(),
                             sex = col_character(),
                             lc_exp = col_double(),
                             region = col_factor(),
                             end_death = col_date(format = "%Y-%m-%d"),
                             end_deregist = col_date(format = "%Y-%m-%d"),
                             end_lc_cure = col_date(format = "%Y-%m-%d")
                       )
)
hx_lc_com_raw <- hx_lc_com_raw %>% rename(exposure = lc_exp)

hx_lc_com_raw$index_date <- as.Date("2020-11-01")
hx_lc_com_raw <- hx_lc_com_raw %>% relocate(index_date, .before = region)
hx_lc_com_raw %>% names


# combind and explore
unmatched_all <- bind_rows(hx_lc_exp_raw, hx_lc_com_raw)
unmatched_all$exposure <- as.factor(unmatched_all$exposure)

# examine wrong dates: 
unmatched_all <- unmatched_all  %>% 
      mutate(wrong_death = ifelse((as.numeric(index_date) > as.numeric(end_death)), 1, 0)) %>% 
      mutate(wrong_regist= ifelse((as.numeric(index_date) > as.numeric(end_deregist)), 1,0)) %>% 
      mutate(wrong_lc_cure= ifelse((as.numeric(index_date) > as.numeric(end_lc_cure)), 1,0)) 

# generate basic summarised table:
dependent = "exposure"
explanatory = c("sex", "age", "region", "wrong_death", "wrong_regist", "wrong_lc_cure")

unmatched_results <- unmatched_all %>% summary_factorlist(dependent, explanatory) 
unmatched_results %>% write_csv(here("output", "qc02_hx_check_unmatched.csv"))
