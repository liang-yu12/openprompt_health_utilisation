# Load all packages
source("analysis/settings_packages.R")
# unmatched exposure
lc_exp_raw <- read_csv(here("output", "dataset_exp_lc_unmatched.csv")
                          #  col_types = cols(
                          #    patient_id = col_double(),
                          #    age = col_double(),
                          #    sex = col_character(),
                          #    region = col_character(),
                          #    registration_date = col_date(format =  "%Y-%m-%d"),
                          #    long_covid_dx = col_double(),
                          #    long_covid_dx_date = col_date(format =  "%Y-%m-%d"),
                          #    index_date = col_date(format =  "%Y-%m-%d"),
                          #    end_death = col_date(format =  "%Y-%m-%d"),
                          #    end_deregist = col_date(format =  "%Y-%m-%d"),
                          #    end_lc_cure = col_date(format =  "%Y-%m-%d")
                           ))
# 
# lc_exp_raw <- lc_exp_raw %>% mutate(wrong_date = ifelse((as.numeric(long_covid_dx_date) > as.numeric(end_death) |
#                                                             as.numeric(long_covid_dx_date) > as.numeric(end_deregist) |
#                                                             as.numeric(long_covid_dx_date) > as.numeric(end_lc_cure)), 1, 0))
# lc_exp_raw$wrong_date <- as.factor(lc_exp_raw$wrong_date)
lc_exp_raw$exposure <- "long COVID"



# unmatched comparator
lc_com_raw <- read_csv(here("output", "dataset_comparator_unmatched.csv")
                      #  col_types = cols(
                      #    patient_id = col_double(),
                      #    age = col_double(),
                      #    sex = col_character(),
                      #    region = col_character(),
                      #    registration_date = col_date(format = "%Y-%m-%d"),
                      #    long_covid_dx = col_double(),
                      #    long_covid_dx_date = col_date(format = "%Y-%m-%d"),
                      #    end_death = col_date(format = "%Y-%m-%d"),
                      #    end_deregist = col_date(format = "%Y-%m-%d"),
                      #    end_lc_cure = col_date(format = "%Y-%m-%d")
                       ))

# lc_com_raw <- lc_com_raw %>% mutate(wrong_date = ifelse((as.numeric(long_covid_dx_date) > as.numeric(end_death) |
#                                                             as.numeric(long_covid_dx_date) > as.numeric(end_deregist) |
#                                                             as.numeric(long_covid_dx_date) > as.numeric(end_lc_cure)), 1, 0))
#                                                             
# lc_com_raw$wrong_date <- as.factor(lc_com_raw$wrong_date)
lc_com_raw$exposure <- "Comparison"


# combind and explore
unmatched_all <- bind_rows(lc_exp_raw, lc_com_raw)

# generate basic summarised table:
dependent = "exposure"
explanatory = c("sex", "age")

unmatched_results <- unmatched_all %>% summary_factorlist(dependent, explanatory, 
                                 cont_cut = 5) 
unmatched_results %>% write_csv(here("output", "qc01_check_unmatched.csv"))
