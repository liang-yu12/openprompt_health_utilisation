# Load all packages
source("analysis/settings_packages.R")
# unmatched exposure
hx_lc_exp_raw <- read_csv(here("output", "hx_unmatched_exp.csv"),
                       col_types = cols(
                             patient_id = col_double(),
                             age = col_double(),
                             sex = col_factor(),
                             lc_exp = col_logical(),
                             index_date = col_date(format = "%Y-%m-%d"),
                             region = col_factor()
                       )
)

hx_lc_exp_raw$index_date <- NULL
hx_lc_exp_raw$exposure <- 1

# unmatched comparator
hx_lc_com_raw <- read_csv(here("output", "hx_dataset_comp_unmatched.csv"),
                       col_types = cols(
                             patient_id = col_double(),
                             age = col_double(),
                             sex = col_factor(),
                             lc_exp = col_logical(),
                             region = col_factor()
                       )
)
hx_lc_com_raw$exposure <- 0


# combind and explore
unmatched_all <- bind_rows(hx_lc_exp_raw, hx_lc_com_raw)
unmatched_all$exposure <- as.factor(unmatched_all$exposure)

# generate basic summarised table:
dependent = "exposure"
explanatory = c("sex", "age", "region")

unmatched_results <- unmatched_all %>% summary_factorlist(dependent, explanatory) 
unmatched_results %>% write_csv(here("output", "qc02_hx_check_unmatched.csv"))
