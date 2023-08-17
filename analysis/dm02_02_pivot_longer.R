# Load all packages
source("analysis/dm02_01_hx_variables.R")

# Data management for DID structure:----
# Explanation: Need to create a long table specifying the time period and the exposure group, 
# So that in the following analysis we can add the interaction term in the DID model 

# Var: 
# exposure: 1 "exposure" 0 "comparator"
# time: 0 "historical" 1 "current"


common_vars <- c("patient_id","sex","age_cat", "ethnicity_6", "bmi_cat", 
                 "region", "imd_q5", "number_comorbidities_cat")

# Extract the column names for each month: -----
# Recognise the column name patterns, and then use prefix to separate the historical and current visits
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

# Exposure & Historical subset: ----

hx_exp <- hx_cases # Create a dataset with everything. Will simplify it later

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

all_months_vars <- hx_exp[,grepl("all_month_",names(hx_exp))] %>% 
      names %>% as.vector() # extract the names

hx_exp <- hx_exp %>% dplyr::select(all_of(common_vars), all_of(all_months_vars))
hx_exp$exposure <- 1  # This is exposure group
hx_exp$time <- 0 #Historical records 

# 1.2 Pivot the data to long form 

hx_exp_ts <- hx_exp %>% 
      pivot_longer(
            cols = all_of(all_months_vars),
            names_to = c("month"),
            values_to = "monthly_visits"
      )

hx_exp_ts$month <- str_sub(hx_exp_ts$month, 12) %>% 
      as.numeric() # clean the month var

# define follow-up time:
hx_exp_ts <- hx_exp_ts %>% mutate(fu_time = 30)

# Matched control & Historical subset: ----------------
hx_com <- hx_control # Create a dataset with everything. 

# define the monthly visits
hx_com$all_month_m1 <- rowSums(hx_com[,m1_hx, with = F], na.rm = T)
hx_com$all_month_m2 <- rowSums(hx_com[,m2_hx, with = F], na.rm = T)
hx_com$all_month_m3 <- rowSums(hx_com[,m3_hx, with = F], na.rm = T)
hx_com$all_month_m4 <- rowSums(hx_com[,m4_hx, with = F], na.rm = T)
hx_com$all_month_m5 <- rowSums(hx_com[,m5_hx, with = F], na.rm = T)
hx_com$all_month_m6 <- rowSums(hx_com[,m6_hx, with = F], na.rm = T)
hx_com$all_month_m7 <- rowSums(hx_com[,m7_hx, with = F], na.rm = T)
hx_com$all_month_m8 <- rowSums(hx_com[,m8_hx, with = F], na.rm = T)
hx_com$all_month_m9 <- rowSums(hx_com[,m9_hx, with = F], na.rm = T)
hx_com$all_month_m10 <- rowSums(hx_com[,m10_hx, with = F], na.rm = T)
hx_com$all_month_m11 <- rowSums(hx_com[,m11_hx, with = F], na.rm = T)
hx_com$all_month_m12 <- rowSums(hx_com[,m12_hx, with = F], na.rm = T)

# simplify 
hx_com <- hx_com %>% dplyr::select(all_of(common_vars), all_of(all_months_vars))

hx_com$exposure <- 0 # this is comparator group
hx_com$time <- 0 # this is historical subset


# Pivot the data
hx_com_ts <- hx_com %>% 
      pivot_longer(
            cols = all_of(all_months_vars),
            names_to = c("month"),
            values_to = "monthly_visits"
      )
hx_com_ts$month <- str_sub(hx_com_ts$month, 12) %>% 
      as.numeric() # clean the month var
# follow up time
hx_com_ts <- hx_com_ts %>% mutate(fu_time = 30)

# Exposure & Contemporary subset: ----
now_exp <- hx_cases # subset for current group

# Need to sort follow-up time first
now_exp$fu_total <- as.numeric(now_exp$end_date) - as.numeric(now_exp$index_date)

now_exp <- now_exp %>% 
      mutate(fu_time_m1 = case_when(fu_total%/%30>=1 ~ 30,
                                    fu_total%/%30 <1 ~ fu_total,
                                    fu_total == 0 ~ NA_real_))

now_exp$end_date %>% summary
now_exp$index_date %>% summary
now_exp$fu_total %>% summary







# Define the current visits
now_exp$all_month_m1 <- rowSums(now_exp[,m1_now, with = F], na.rm = T)
now_exp$all_month_m2 <- rowSums(now_exp[,m2_now, with = F], na.rm = T)
now_exp$all_month_m3 <- rowSums(now_exp[,m3_now, with = F], na.rm = T)
now_exp$all_month_m4 <- rowSums(now_exp[,m4_now, with = F], na.rm = T)
now_exp$all_month_m5 <- rowSums(now_exp[,m5_now, with = F], na.rm = T)
now_exp$all_month_m6 <- rowSums(now_exp[,m6_now, with = F], na.rm = T)
now_exp$all_month_m7 <- rowSums(now_exp[,m7_now, with = F], na.rm = T)
now_exp$all_month_m8 <- rowSums(now_exp[,m8_now, with = F], na.rm = T)
now_exp$all_month_m9 <- rowSums(now_exp[,m9_now, with = F], na.rm = T)
now_exp$all_month_m10 <- rowSums(now_exp[,m10_now, with = F], na.rm = T)
now_exp$all_month_m11 <- rowSums(now_exp[,m11_now, with = F], na.rm = T)
now_exp$all_month_m12 <- rowSums(now_exp[,m12_now, with = F], na.rm = T)

# simplify the data
now_exp <- now_exp %>% dplyr::select(all_of(common_vars), all_of(all_months_vars))

now_exp$exposure <- 1  # exposure group
now_exp$time <- 1 # contemporary records

# pivot the data:
now_exp_ts <- now_exp %>% 
      pivot_longer(
            cols = all_of(all_months_vars),
            names_to = c("month"),
            values_to = "monthly_visits"
      )

now_exp_ts$month <- str_sub(now_exp_ts$month, 12) %>% 
      as.numeric() # clean the month var

# Define current follow up time:




