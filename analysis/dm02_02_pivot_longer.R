# Load all packages
source("analysis/settings_packages.R")

# 1. Data management for DID structure:----
# Explanation: Need to create a long table specifying the time period and the exposure group, 
# So that in the following analysis we can add the interaction term in the DID model 

# First define common variables to select:
vars <- c("patient_id","age","sex","region","lc_dx","index_date","exposure",
          "ethnicity" ,"imd", "bmi", "end_death", "end_deregist", "end_lc_cure",
          "cov_cancer" ,"cov_mental_health", "cov_asthma", "cov_organ_transplant",
          "cov_chronic_cardiac_disease", "cov_chronic_liver_disease", 
          "cov_stroke_dementia" ,"cov_other_neuro_diseases", "cov_ra_sle_psoriasis", 
          "cov_asplenia" ,"cov_hiv" ,"cov_aplastic_anemia",          
          "cov_permanent_immune_suppress", "cov_temporary_immune_suppress")

comorbidities <- c("cov_cancer",  "cov_mental_health",   "cov_asthma",
                   "cov_organ_transplant",   "cov_chronic_cardiac_disease",   "cov_chronic_liver_disease",
                   "cov_stroke_dementia",   "cov_other_neuro_diseases",   "cov_ra_sle_psoriasis",
                   "cov_asplenia",   "cov_hiv",   "cov_aplastic_anemia",   "cov_permanent_immune_suppress",
                   "cov_temporary_immune_suppress")

hx_visits <- c("hx_gp_visit_m1", "hx_gp_visit_m2", "hx_gp_visit_m3", "hx_gp_visit_m4", "hx_gp_visit_m5",
        "hx_gp_visit_m6", "hx_gp_visit_m7", "hx_gp_visit_m8", "hx_gp_visit_m9", "hx_gp_visit_m10",
        "hx_gp_visit_m11", "hx_gp_visit_m12", "hx_hos_visit_m1", "hx_hos_visit_m2", "hx_hos_visit_m3",
        "hx_hos_visit_m4", "hx_hos_visit_m5", "hx_hos_visit_m6", "hx_hos_visit_m7", "hx_hos_visit_m8",
        "hx_hos_visit_m9", "hx_hos_visit_m10", "hx_hos_visit_m11", "hx_hos_visit_m12", "hx_ae_visit_m1",
        "hx_ae_visit_m2", "hx_ae_visit_m3", "hx_ae_visit_m4", "hx_ae_visit_m5", "hx_ae_visit_m6",
        "hx_ae_visit_m7", "hx_ae_visit_m8", "hx_ae_visit_m9", "hx_ae_visit_m10", "hx_ae_visit_m11",
        "hx_ae_visit_m12")


now_visits <- c("gp_visit_m1", "gp_visit_m2", "gp_visit_m3", "gp_visit_m4", "gp_visit_m5", "gp_visit_m6",
             "gp_visit_m7", "gp_visit_m8", "gp_visit_m9", "gp_visit_m10", "gp_visit_m11", "gp_visit_m12",
             "hos_visit_m1", "hos_visit_m2", "hos_visit_m3", "hos_visit_m4", "hos_visit_m5", "hos_visit_m6",
             "hos_visit_m7", "hos_visit_m8", "hos_visit_m9", "hos_visit_m10", "hos_visit_m11", "hos_visit_m12",
             "ae_visit_m1", "ae_visit_m2", "ae_visit_m3", "ae_visit_m4", "ae_visit_m5", "ae_visit_m6",
             "ae_visit_m7", "ae_visit_m8", "ae_visit_m9", "ae_visit_m10", "ae_visit_m11", "ae_visit_m12")

## Exposure/cases:-----
hx_cases <- read_csv(here("output", "hx_matched_cases_with_ehr.csv"), 
                                      col_types = cols(index_date = col_date(format = "%Y-%m-%d"), 
                                                       bmi_date = col_skip(),
                                                       end_death = col_date(format = "%Y-%m-%d"), 
                                                       end_deregist = col_date(format = "%Y-%m-%d"), 
                                                       end_lc_cure = col_date(format = "%Y-%m-%d")))
### Combine the all healthcare visit by months:
#### First get the vector of all visits:
m1 <- hx_cases[, grepl("_visit_m1$", names(hx_cases)), with = FALSE] %>% names() %>% as.vector()
m1_hx <- m1[grepl("hx_", m1)] # historical visits
m1_now <- m1[!grepl("hx_", m1)] # current visits

m2 <- hx_cases[, grepl("_visit_m2$", names(hx_cases)), with = FALSE] %>% names() %>% as.vector()
m2_hx <- m2[grepl("hx_", m2)] # historical visits
m2_now <- m2[!grepl("hx_", m2)] # current visits

m3 <- hx_cases[, grepl("_visit_m3$", names(hx_cases)), with = FALSE] %>% names() %>% as.vector()
m3_hx <- m3[grepl("hx_", m3)] # historical visits
m3_now <- m3[!grepl("hx_", m3)] # current visits

m4 <- hx_cases[, grepl("_visit_m4$", names(hx_cases)), with = FALSE] %>% names() %>% as.vector()
m4_hx <- m4[grepl("hx_", m4)] # historical visits
m4_now <- m4[!grepl("hx_", m4)] # current visits

m5 <- hx_cases[, grepl("_visit_m5$", names(hx_cases)), with = FALSE] %>% names() %>% as.vector()
m5_hx <- m5[grepl("hx_", m5)] # historical visits
m5_now <- m5[!grepl("hx_", m5)] # current visits

m6 <- hx_cases[, grepl("_visit_m6$", names(hx_cases)), with = FALSE] %>% names() %>% as.vector()
m6_hx <- m6[grepl("hx_", m6)] # historical visits
m6_now <- m6[!grepl("hx_", m6)] # current visits

m7 <- hx_cases[, grepl("_visit_m7$", names(hx_cases)), with = FALSE] %>% names() %>% as.vector()
m7_hx <- m7[grepl("hx_", m7)] # historical visits
m7_now <- m7[!grepl("hx_", m7)] # current visits

m8 <- hx_cases[, grepl("_visit_m8$", names(hx_cases)), with = FALSE] %>% names() %>% as.vector()
m8_hx <- m8[grepl("hx_", m8)] # historical visits
m8_now <- m8[!grepl("hx_", m8)] # current visits

m9 <- hx_cases[, grepl("_visit_m9$", names(hx_cases)), with = FALSE] %>% names() %>% as.vector()
m9_hx <- m9[grepl("hx_", m9)] # historical visits
m9_now <- m9[!grepl("hx_", m9)] # current visits

m10 <- hx_cases[, grepl("_visit_m10$", names(hx_cases)), with = FALSE] %>% names() %>% as.vector()
m10_hx <- m10[grepl("hx_", m10)] # historical visits
m10_now <- m10[!grepl("hx_", m10)] # current visits

m11 <- hx_cases[, grepl("_visit_m11$", names(hx_cases)), with = FALSE] %>% names() %>% as.vector()
m11_hx <- m11[grepl("hx_", m11)] # historical visits
m11_now <- m11[!grepl("hx_", m11)] # current visits

m12 <- hx_cases[, grepl("_visit_m12$", names(hx_cases)), with = FALSE] %>% names() %>% as.vector()
m12_hx <- m12[grepl("hx_", m12)] # historical visits
m12_now <- m12[!grepl("hx_", m12)] # current visits


# 1. Subset the historical cases: ----
hx_exp <- hx_cases %>% dplyr::select(all_of(vars), all_of(hx_visits)) %>% mutate(time = 0)

# 1.1: Combine the historical healthcare visits
hx_exp$all_month_m1 <- rowSums(hx_exp[,m1_hx, with = F], na.rm = T)
hx_exp$all_month_m2 <- rowSums(hx_exp[,m2_hx, with = F], na.rm = T)
hx_exp$all_month_m3 <- rowSums(hx_exp[,m3_hx, with = F], na.rm = T)
hx_exp$all_month_m4 <- rowSums(hx_exp[,m4_hx, with = F], na.rm = T)
hx_exp$all_month_m5 <- rowSums(hx_exp[,m5_hx, with = F], na.rm = T)
hx_exp$all_month_m6 <- rowSums(hx_exp[,m6_hx, with = F], na.rm = T)
hx_exp$all_month_m7 <- rowSums(hx_exp[,m7_hx, with = F], na.rm = T)
hx_exp$all_month_m8 <- rowSums(hx_exp[,m8_hx, with = F], na.rm = T)
hx_exp$all_month_m9 <- rowSums(hx_exp[,m9_hx, with = F], na.rm = T)
hx_exp$all_month_m10 <- rowSums(hx_exp[,m10_hx, with = F], na.rm = T)
hx_exp$all_month_m11 <- rowSums(hx_exp[,m11_hx, with = F], na.rm = T)
hx_exp$all_month_m12 <- rowSums(hx_exp[,m12_hx, with = F], na.rm = T)


# 1.2 Pivot the dataset to long form 
visit_cols <- hx_exp[grep("all_month_", names(hx_exp))] %>% 
      names %>% as.vector() # set up the columns

hx_exp_ts <- hx_exp %>% 
      pivot_longer(
            cols = all_of(visit_cols),
            names_to = c("month"),
            values_to = "monthly_visits"
      )
hx_




### Define the follow-up time:
hx_exp <- hx_exp %>% mutate(
      fu_time_m1 = 30,
      fu_time_m2 = 60,
      fu_time_m3 = 90,
      fu_time_m4 = 120,
      fu_time_m5 = 150,
      fu_time_m6 = 180,
      fu_time_m7 = 210,
      fu_time_m8 = 240,
      fu_time_m9 = 270,
      fu_time_m10 = 300,
      fu_time_m11 = 330,
      fu_time_m12 = 360)

### subset the current cases: 
now_exp <- hx_cases %>% dplyr::select(all_of(vars), all_of(now_visits)) %>% mutate(time = 1)

### Define the end date of current exposure group:
now_exp <- now_exp %>% mutate(
            end_date = pmin(as.numeric(end_death), 
                            as.numeric(end_deregist), 
                            as.numeric(end_lc_cure), 
                            as.numeric(as.Date("2023-01-31", origin = "1970-01-01")), na.rm = T)
            )
### calculate the follow-up time for each month:
now_exp <- now_exp %>% 
      mutate(
      fu_time_m1 = as.double(case_when(
            as.numeric(index_date) + 30*1 < as.numeric(end_date) ~ 30*1,    #t2 censor later
            (as.numeric(end_date) <= (as.numeric(index_date) + 30*1)) & ((as.numeric(end_date) - as.numeric(index_date)) > 30*(1-1)) ~ 
                  (as.numeric(end_date) - as.numeric(index_date)), # t2 censor before follow 
            (as.numeric(end_date) - as.numeric(index_date)) <= 30*(1-1) ~ NA_real_ # already censored 
      )),
      fu_time_m2 = as.double(case_when(
            as.numeric(index_date) + 30*2 < as.numeric(end_date) ~ 30*2,    #t2 censor later
            (as.numeric(end_date) <= (as.numeric(index_date) + 30*2)) & ((as.numeric(end_date) - as.numeric(index_date)) > 30*(2-1)) ~ 
                  (as.numeric(end_date) - as.numeric(index_date)), # t2 censor before follow 
            (as.numeric(end_date) - as.numeric(index_date)) <= 30*(2-1) ~ NA_real_ # already censored 
      )),
      fu_time_m3 = as.double(case_when(
            as.numeric(index_date) + 30*3 < as.numeric(end_date) ~ 30*3,    #t2 censor later
            (as.numeric(end_date) <= (as.numeric(index_date) + 30*3)) & ((as.numeric(end_date) - as.numeric(index_date)) > 30*(3-1)) ~ 
                  (as.numeric(end_date) - as.numeric(index_date)), # t2 censor before follow 
            (as.numeric(end_date) - as.numeric(index_date)) <= 30*(3-1) ~ NA_real_ # already censored 
      )),
      fu_time_m4 = as.double(case_when(
            as.numeric(index_date) + 30*4 < as.numeric(end_date) ~ 30*4,    #t2 censor later
            (as.numeric(end_date) <= (as.numeric(index_date) + 30*4)) & ((as.numeric(end_date) - as.numeric(index_date)) > 30*(4-1)) ~ 
                  (as.numeric(end_date) - as.numeric(index_date)), # t2 censor before follow 
            (as.numeric(end_date) - as.numeric(index_date)) <= 30*(4-1) ~ NA_real_ # already censored 
      )),
      fu_time_m5 = as.double(case_when(
            as.numeric(index_date) + 30*5 < as.numeric(end_date) ~ 30*5,    #t2 censor later
            (as.numeric(end_date) <= (as.numeric(index_date) + 30*5)) & ((as.numeric(end_date) - as.numeric(index_date)) > 30*(5-1)) ~ 
                  (as.numeric(end_date) - as.numeric(index_date)), # t2 censor before follow 
            (as.numeric(end_date) - as.numeric(index_date)) <= 30*(5-1) ~ NA_real_ # already censored 
      )),
      fu_time_m6 = as.double(case_when(
            as.numeric(index_date) + 30*6 < as.numeric(end_date) ~ 30*6,    #t2 censor later
            (as.numeric(end_date) <= (as.numeric(index_date) + 30*6)) & ((as.numeric(end_date) - as.numeric(index_date)) > 30*(6-1)) ~ 
                  (as.numeric(end_date) - as.numeric(index_date)), # t2 censor before follow 
            (as.numeric(end_date) - as.numeric(index_date)) <= 30*(6-1) ~ NA_real_ # already censored 
      )),
      fu_time_m7 = as.double(case_when(
            as.numeric(index_date) + 30*7 < as.numeric(end_date) ~ 30*7,    #t2 censor later
            (as.numeric(end_date) <= (as.numeric(index_date) + 30*7)) & ((as.numeric(end_date) - as.numeric(index_date)) > 30*(7-1)) ~ 
                  (as.numeric(end_date) - as.numeric(index_date)), # t2 censor before follow 
            (as.numeric(end_date) - as.numeric(index_date)) <= 30*(7-1) ~ NA_real_ # already censored 
      )),
      fu_time_m8 = as.double(case_when(
            as.numeric(index_date) + 30*8 < as.numeric(end_date) ~ 30*8,    #t2 censor later
            (as.numeric(end_date) <= (as.numeric(index_date) + 30*8)) & ((as.numeric(end_date) - as.numeric(index_date)) > 30*(8-1)) ~ 
                  (as.numeric(end_date) - as.numeric(index_date)), # t2 censor before follow 
            (as.numeric(end_date) - as.numeric(index_date)) <= 30*(8-1) ~ NA_real_ # already censored 
      )),
      fu_time_m9 = as.double(case_when(
            as.numeric(index_date) + 30*9 < as.numeric(end_date) ~ 30*9,    #t2 censor later
            (as.numeric(end_date) <= (as.numeric(index_date) + 30*9)) & ((as.numeric(end_date) - as.numeric(index_date)) > 30*(9-1)) ~ 
                  (as.numeric(end_date) - as.numeric(index_date)), # t2 censor before follow 
            (as.numeric(end_date) - as.numeric(index_date)) <= 30*(9-1) ~ NA_real_ # already censored 
      )),
      fu_time_m10 = as.double(case_when(
            as.numeric(index_date) + 30*10 < as.numeric(end_date) ~ 30*10,    #t2 censor later
            (as.numeric(end_date) <= (as.numeric(index_date) + 30*10)) & ((as.numeric(end_date) - as.numeric(index_date)) > 30*(10-1)) ~ 
                  (as.numeric(end_date) - as.numeric(index_date)), # t2 censor before follow 
            (as.numeric(end_date) - as.numeric(index_date)) <= 30*(10-1) ~ NA_real_ # already censored 
      )),
      fu_time_m11 = as.double(case_when(
            as.numeric(index_date) + 30*11 < as.numeric(end_date) ~ 30*11,    #t2 censor later
            (as.numeric(end_date) <= (as.numeric(index_date) + 30*11)) & ((as.numeric(end_date) - as.numeric(index_date)) > 30*(11-1)) ~ 
                  (as.numeric(end_date) - as.numeric(index_date)), # t2 censor before follow 
            (as.numeric(end_date) - as.numeric(index_date)) <= 30*(11-1) ~ NA_real_ # already censored 
      )),
      fu_time_m12 = as.double(case_when(
            as.numeric(index_date) + 30*12 < as.numeric(end_date) ~ 30*12,    #t2 censor later
            (as.numeric(end_date) <= (as.numeric(index_date) + 30*12)) & ((as.numeric(end_date) - as.numeric(index_date)) > 30*(12-1)) ~ 
                  (as.numeric(end_date) - as.numeric(index_date)), # t2 censor before follow 
            (as.numeric(end_date) - as.numeric(index_date)) <= 30*(12-1) ~ NA_real_ # already censored 
      ))
      )

now_exp$end_date <- NULL # remove for combine

## Comparators: -----
hx_control <- read_csv(here("output", "hx_matched_control_with_ehr.csv"), 
                       col_types = cols(index_date = col_date(format = "%Y-%m-%d"), 
                                        bmi_date = col_skip(),
                                        end_death = col_date(format = "%Y-%m-%d"), 
                                        end_deregist = col_date(format = "%Y-%m-%d"), 
                                        end_lc_cure = col_date(format = "%Y-%m-%d")))
### subset the historical cases: 
hx_com <- hx_control %>% dplyr::select(all_of(vars), all_of(hx_visits)) %>% mutate(time = 0) 
hx_com <- setnames(hx_com, old = hx_visits, new = now_visits) # Rename variables for later combinations
hx_com <- hx_com %>% mutate(
      fu_time_m1 = 30,
      fu_time_m2 = 60,
      fu_time_m3 = 90,
      fu_time_m4 = 120,
      fu_time_m5 = 150,
      fu_time_m6 = 180,
      fu_time_m7 = 210,
      fu_time_m8 = 240,
      fu_time_m9 = 270,
      fu_time_m10 = 300,
      fu_time_m11 = 330,
      fu_time_m12 = 360)

### subset the current cases: 
now_com <- hx_control %>% dplyr::select(all_of(vars), all_of(now_visits)) %>% mutate(time = 1)

### Define the end date of current exposure group:
now_com <- now_com %>% mutate(
      end_date = pmin(as.numeric(end_death), 
                      as.numeric(end_deregist), 
                      as.numeric(end_lc_cure), 
                      as.numeric(as.Date("2023-01-31", origin = "1970-01-01")), na.rm = T)
)


now_com <- now_com %>%
      mutate(
            fu_time_m1 = as.double(case_when(
                  as.numeric(index_date) + 30*1 < as.numeric(end_date) ~ 30*1,    #t2 censor later
                  (as.numeric(end_date) <= (as.numeric(index_date) + 30*1)) & ((as.numeric(end_date) - as.numeric(index_date)) > 30*(1-1)) ~ 
                        (as.numeric(end_date) - as.numeric(index_date)), # t2 censor before follow 
                  (as.numeric(end_date) - as.numeric(index_date)) <= 30*(1-1) ~ NA_real_ # already censored 
            )),
            fu_time_m2 = as.double(case_when(
                  as.numeric(index_date) + 30*2 < as.numeric(end_date) ~ 30*2,    #t2 censor later
                  (as.numeric(end_date) <= (as.numeric(index_date) + 30*2)) & ((as.numeric(end_date) - as.numeric(index_date)) > 30*(2-1)) ~ 
                        (as.numeric(end_date) - as.numeric(index_date)), # t2 censor before follow 
                  (as.numeric(end_date) - as.numeric(index_date)) <= 30*(2-1) ~ NA_real_ # already censored 
            )),
            fu_time_m3 = as.double(case_when(
                  as.numeric(index_date) + 30*3 < as.numeric(end_date) ~ 30*3,    #t2 censor later
                  (as.numeric(end_date) <= (as.numeric(index_date) + 30*3)) & ((as.numeric(end_date) - as.numeric(index_date)) > 30*(3-1)) ~ 
                        (as.numeric(end_date) - as.numeric(index_date)), # t2 censor before follow 
                  (as.numeric(end_date) - as.numeric(index_date)) <= 30*(3-1) ~ NA_real_ # already censored 
            )),
            fu_time_m4 = as.double(case_when(
                  as.numeric(index_date) + 30*4 < as.numeric(end_date) ~ 30*4,    #t2 censor later
                  (as.numeric(end_date) <= (as.numeric(index_date) + 30*4)) & ((as.numeric(end_date) - as.numeric(index_date)) > 30*(4-1)) ~ 
                        (as.numeric(end_date) - as.numeric(index_date)), # t2 censor before follow 
                  (as.numeric(end_date) - as.numeric(index_date)) <= 30*(4-1) ~ NA_real_ # already censored 
            )),
            fu_time_m5 = as.double(case_when(
                  as.numeric(index_date) + 30*5 < as.numeric(end_date) ~ 30*5,    #t2 censor later
                  (as.numeric(end_date) <= (as.numeric(index_date) + 30*5)) & ((as.numeric(end_date) - as.numeric(index_date)) > 30*(5-1)) ~ 
                        (as.numeric(end_date) - as.numeric(index_date)), # t2 censor before follow 
                  (as.numeric(end_date) - as.numeric(index_date)) <= 30*(5-1) ~ NA_real_ # already censored 
            )),
            fu_time_m6 = as.double(case_when(
                  as.numeric(index_date) + 30*6 < as.numeric(end_date) ~ 30*6,    #t2 censor later
                  (as.numeric(end_date) <= (as.numeric(index_date) + 30*6)) & ((as.numeric(end_date) - as.numeric(index_date)) > 30*(6-1)) ~ 
                        (as.numeric(end_date) - as.numeric(index_date)), # t2 censor before follow 
                  (as.numeric(end_date) - as.numeric(index_date)) <= 30*(6-1) ~ NA_real_ # already censored 
            )),
            fu_time_m7 = as.double(case_when(
                  as.numeric(index_date) + 30*7 < as.numeric(end_date) ~ 30*7,    #t2 censor later
                  (as.numeric(end_date) <= (as.numeric(index_date) + 30*7)) & ((as.numeric(end_date) - as.numeric(index_date)) > 30*(7-1)) ~ 
                        (as.numeric(end_date) - as.numeric(index_date)), # t2 censor before follow 
                  (as.numeric(end_date) - as.numeric(index_date)) <= 30*(7-1) ~ NA_real_ # already censored 
            )),
            fu_time_m8 = as.double(case_when(
                  as.numeric(index_date) + 30*8 < as.numeric(end_date) ~ 30*8,    #t2 censor later
                  (as.numeric(end_date) <= (as.numeric(index_date) + 30*8)) & ((as.numeric(end_date) - as.numeric(index_date)) > 30*(8-1)) ~ 
                        (as.numeric(end_date) - as.numeric(index_date)), # t2 censor before follow 
                  (as.numeric(end_date) - as.numeric(index_date)) <= 30*(8-1) ~ NA_real_ # already censored 
            )),
            fu_time_m9 = as.double(case_when(
                  as.numeric(index_date) + 30*9 < as.numeric(end_date) ~ 30*9,    #t2 censor later
                  (as.numeric(end_date) <= (as.numeric(index_date) + 30*9)) & ((as.numeric(end_date) - as.numeric(index_date)) > 30*(9-1)) ~ 
                        (as.numeric(end_date) - as.numeric(index_date)), # t2 censor before follow 
                  (as.numeric(end_date) - as.numeric(index_date)) <= 30*(9-1) ~ NA_real_ # already censored 
            )),
            fu_time_m10 = as.double(case_when(
                  as.numeric(index_date) + 30*10 < as.numeric(end_date) ~ 30*10,    #t2 censor later
                  (as.numeric(end_date) <= (as.numeric(index_date) + 30*10)) & ((as.numeric(end_date) - as.numeric(index_date)) > 30*(10-1)) ~ 
                        (as.numeric(end_date) - as.numeric(index_date)), # t2 censor before follow 
                  (as.numeric(end_date) - as.numeric(index_date)) <= 30*(10-1) ~ NA_real_ # already censored 
            )),
            fu_time_m11 = as.double(case_when(
                  as.numeric(index_date) + 30*11 < as.numeric(end_date) ~ 30*11,    #t2 censor later
                  (as.numeric(end_date) <= (as.numeric(index_date) + 30*11)) & ((as.numeric(end_date) - as.numeric(index_date)) > 30*(11-1)) ~ 
                        (as.numeric(end_date) - as.numeric(index_date)), # t2 censor before follow 
                  (as.numeric(end_date) - as.numeric(index_date)) <= 30*(11-1) ~ NA_real_ # already censored 
            )),
            fu_time_m12 = as.double(case_when(
                  as.numeric(index_date) + 30*12 < as.numeric(end_date) ~ 30*12,    #t2 censor later
                  (as.numeric(end_date) <= (as.numeric(index_date) + 30*12)) & ((as.numeric(end_date) - as.numeric(index_date)) > 30*(12-1)) ~ 
                        (as.numeric(end_date) - as.numeric(index_date)), # t2 censor before follow 
                  (as.numeric(end_date) - as.numeric(index_date)) <= 30*(12-1) ~ NA_real_ # already censored 
            ))
      )

now_com$end_date <- NULL

### Combine four datasets
hx_matched_data <- bind_rows(hx_exp, now_exp, hx_com, now_com)
rm(hx_exp, now_exp, hx_com, now_com, hx_cases, hx_control)
# 2. Managing other variables: ------
hx_matched_data <- hx_matched_data %>% mutate(
      imd_q5 = cut2(imd, g = 5),
      ethnicity_6 = factor(
            ethnicity,
            levels = 1:6, 
            labels = c(
                  "White",
                  "Mixed", 
                  "South Asian", 
                  "Black",
                  "Other",
                  "Not stated")
      ),
      age_cat = cut(
            age, 
            breaks = c(0, seq(30, 70, 10), Inf),
            labels = c(
                  "18-29",
                  "30-39",
                  "40-49",
                  "50-59",
                  "60-69",
                  "70+"
      )),
      bmi_cat = case_when(
            ethnicity_6 %in% c("South Asian", "Other") ~ cut(bmi, breaks = c(0,18.5,23,27.5,Inf), 
                                                             labels = c("Underweight", "Normal Weight", "Overweight", "Obese")),
            !ethnicity_6 %in% c("South Asian", "Other") ~ cut(bmi, breaks = c(0,18.5,25,30,Inf),
                                                             labels = c("Underweight", "Normal Weight", "Overweight", "Obese"))
      )
)


### set as factors:
hx_matched_data$exposure <- hx_matched_data$exposure %>% 
      factor(label = c("Comparator", "Long COVID exposure"))

to_be_factors <- c("sex", "region", "ethnicity")
hx_matched_data[to_be_factors] <- lapply(hx_matched_data[to_be_factors], as.factor)

### drop unused levels
hx_matched_data[to_be_factors] <- lapply(hx_matched_data[to_be_factors], droplevels)


### label the imd cat
levels(hx_matched_data$imd_q5) <- c("least_deprived",
                                 "2_deprived",
                                 "3_deprived",
                                 "4_deprived",
                                 "most_deprived")

### number of comorbidities:
hx_matched_data[comorbidities] <- lapply(hx_matched_data[comorbidities], as.logical)
hx_matched_data$number_comorbidities <- rowSums(hx_matched_data[comorbidities], na.rm = T) # add them up

hx_matched_data <- hx_matched_data %>% 
      mutate(number_comorbidities_cat = case_when(
            number_comorbidities ==0 ~ 0,
            number_comorbidities ==1 ~ 1,
            number_comorbidities ==2 ~ 2,
            number_comorbidities >=3 ~ 3)) 

hx_matched_data$number_comorbidities_cat <- hx_matched_data$number_comorbidities_cat %>% as.factor()

hx_matched_data %>% names

# 3. Combine the healthcare visits-------
hx_matched_data$all_month1 <- rowSums(hx_matched_data[, grepl("m1$", names(hx_matched_data)), with = FALSE], na.rm = T)
hx_matched_data$all_month2 <- rowSums(hx_matched_data[, grepl("m2$", names(hx_matched_data)), with = FALSE], na.rm = T)
hx_matched_data$all_month3 <- rowSums(hx_matched_data[, grepl("m3$", names(hx_matched_data)), with = FALSE], na.rm = T)
hx_matched_data$all_month4 <- rowSums(hx_matched_data[, grepl("m4$", names(hx_matched_data)), with = FALSE], na.rm = T)
hx_matched_data$all_month5 <- rowSums(hx_matched_data[, grepl("m5$", names(hx_matched_data)), with = FALSE], na.rm = T)
hx_matched_data$all_month6 <- rowSums(hx_matched_data[, grepl("m6$", names(hx_matched_data)), with = FALSE], na.rm = T)
hx_matched_data$all_month7 <- rowSums(hx_matched_data[, grepl("m7$", names(hx_matched_data)), with = FALSE], na.rm = T)
hx_matched_data$all_month8 <- rowSums(hx_matched_data[, grepl("m8$", names(hx_matched_data)), with = FALSE], na.rm = T)
hx_matched_data$all_month9 <- rowSums(hx_matched_data[, grepl("m9$", names(hx_matched_data)), with = FALSE], na.rm = T)
hx_matched_data$all_month10 <- rowSums(hx_matched_data[, grepl("m10$", names(hx_matched_data)), with = FALSE], na.rm = T)
hx_matched_data$all_month11 <- rowSums(hx_matched_data[, grepl("m11$", names(hx_matched_data)), with = FALSE], na.rm = T)
hx_matched_data$all_month12 <- rowSums(hx_matched_data[, grepl("m12$", names(hx_matched_data)), with = FALSE], na.rm = T)

