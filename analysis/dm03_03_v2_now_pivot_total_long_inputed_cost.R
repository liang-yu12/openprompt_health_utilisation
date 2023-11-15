source("analysis/dm03_02_v2_now_inpute_sec_care_cost_cal_total_costs.R")
# Goal: inpute costs by unit costs

# Imputation steps: ----
# a. Pivot follow-up, keep id, exp, month and follow-up 
# b. Pivot different sec cost vars and visits, keep id, exp, month and vars 
# c. Merge two data together by id, exp, month
# d. filter non-NA follow-up 
# e. Mutate a new cost_var if visit != 0 & cost_var ==0, 
#    impute by visit*unit cost
# f. Collapse the new cost_var to get the overall cost
# g. For the total_cost, add collapsed primary care cost and other collapsed costs
#    together (wide tables)

# a. Pivot the follow-up time -----
# pivot exposure group:
# pivot exposure follow-up time
fu_cols <- c()
for (i in 1:12){
      fu_cols <- c(fu_cols, paste0("follow_up_m", i))
}
      
exp_fu_ts <- lc_exp_matched %>% dplyr::select(patient_id, exposure, all_of(fu_cols)) %>% 
      pivot_longer(
            cols = all_of(fu_cols),
            names_to = c("month_fu"),
            values_to = "follow_up_time"
      )
exp_fu_ts$month_fu <- str_sub(exp_fu_ts$month_fu, 12)  # remove "follow_up_m"
exp_fu_ts$month_fu <- as.numeric(exp_fu_ts$month_fu)

# Pivot the comparator group:
# Pivot the comparator follow_up time: 

com_fu_ts <- com_matched %>% dplyr::select(patient_id, exposure, all_of(fu_cols)) %>% 
      pivot_longer(
            cols = all_of(fu_cols),
            names_to = c("month_fu"),
            values_to = "follow_up_time"
      )

com_fu_ts$month_fu <- str_sub(com_fu_ts$month_fu, 12)  # remove "follow_up_m"
com_fu_ts$month_fu <- as.numeric(com_fu_ts$month_fu)


# b. Pivot different sec cost vars and visits, keep id, exp, month and vars ----

# Hospital admission cost (APC)
# hos_visit_m1
hos_visit_cols <- c()
for (i in 1:12){
      hos_visit_cols <- c(hos_visit_cols, paste0("hos_visit_m", i))
}
# apc_cost_m1
apc_cost_cols <- c()
for (i in 1:12){
      apc_cost_cols <- c(apc_cost_cols, paste0("apc_cost_m", i))
}

# Exposure group:
# hospital admission:
exp_hos_visit <- lc_exp_matched %>% 
      dplyr::select(patient_id, exposure, all_of(hos_visit_cols)) %>% 
      pivot_longer(
            cols = all_of(hos_visit_cols),
            names_to = c("month"),
            values_to = "hospital_admission"
      ) %>% 
      mutate(month = as.numeric(str_sub(month, 12))) 

# hospital cost: 
exp_hos_cost <-  lc_exp_matched %>% 
      dplyr::select(patient_id, exposure, all_of(apc_cost_cols)) %>% 
      pivot_longer(
            cols = all_of(apc_cost_cols),
            names_to = c("month"),
            values_to = "apc_cost"
      ) %>% 
      mutate(month = as.numeric(str_sub(month, 11))) %>% 
      mutate(apc_cost = ifelse(!is.na(apc_cost), apc_cost, 0)) # change NA to 0


# c. Merge two data together by id, exp, month------
# join visit and cost
exp_hos_impute <- left_join(exp_hos_visit, exp_hos_cost, 
                            by = c("patient_id" = "patient_id", 
                                   "exposure" = "exposure",
                                   "month" = "month"))
# Join follow up
exp_hos_impute <- left_join(exp_fu_ts, exp_hos_impute,
                            by = c("patient_id" = "patient_id", 
                                   "exposure" = "exposure",
                                   "month_fu" = "month"))


# d. filter non-NA follow-up -----
# e. Mutate a new cost_var if visit != 0 & cost_var ==0, -----
#    impute by visit*unit cost
exp_hos_impute <- exp_hos_impute %>% 
      filter(!is.na(follow_up_time)) %>% 
      mutate(apc_costs_impute=case_when(
            hospital_admission!=0 & apc_cost!=0 ~ apc_cost,
            hospital_admission!=0 & apc_cost==0 ~ hospital_admission*unit_apc_costs$unit_cost,
            hospital_admission==0 & apc_cost!=0 ~ 0,
            hospital_admission==0 & apc_cost==0 ~ 0
      ))

exp_hos_cost_12m <- exp_hos_impute %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            apc_cost_impute_12m = sum(apc_costs_impute, na.rm = T)) %>% 
      ungroup()



# Comparator group APC:
# hospital admission:
com_hos_visit <- com_matched %>% 
      dplyr::select(patient_id, exposure, all_of(hos_visit_cols)) %>% 
      pivot_longer(
            cols = all_of(hos_visit_cols),
            names_to = c("month"),
            values_to = "hospital_admission"
      ) %>% 
      mutate(month = as.numeric(str_sub(month, 12))) 

# hospital cost: 
com_hos_cost <-  com_matched %>% 
      dplyr::select(patient_id, exposure, all_of(apc_cost_cols)) %>% 
      pivot_longer(
            cols = all_of(apc_cost_cols),
            names_to = c("month"),
            values_to = "apc_cost"
      ) %>% 
      mutate(month = as.numeric(str_sub(month, 11))) %>% 
      mutate(apc_cost = ifelse(!is.na(apc_cost), apc_cost, 0)) # change NA to 0


# c. Merge two data together by id, exp, month------
# join visit and cost
com_hos_impute <- left_join(com_hos_visit, com_hos_cost, 
                            by = c("patient_id" = "patient_id", 
                                   "exposure" = "exposure",
                                   "month" = "month"))
# Join follow up
com_hos_impute <- left_join(com_fu_ts, com_hos_impute,
                            by = c("patient_id" = "patient_id", 
                                   "exposure" = "exposure",
                                   "month_fu" = "month"))


# d. filter non-NA follow-up -----
# e. Mutate a new cost_var if visit != 0 & cost_var ==0, -----
#    impute by visit*unit cost
com_hos_impute <- com_hos_impute %>% 
      filter(!is.na(follow_up_time)) %>% 
      mutate(apc_costs_impute=case_when(
            hospital_admission!=0 & apc_cost!=0 ~ apc_cost,
            hospital_admission!=0 & apc_cost==0 ~ hospital_admission*unit_apc_costs$unit_cost,
            hospital_admission==0 & apc_cost!=0 ~ 0,
            hospital_admission==0 & apc_cost==0 ~ 0
      ))

com_hos_cost_12m <- com_hos_impute %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            apc_cost_impute_12m = sum(apc_costs_impute, na.rm = T)) %>% 
      ungroup()



# Impute A&E cost: -----------
# ae_visit_m
ane_visit_cols <- c()
for (i in 1:12){
      ane_visit_cols <- c(ane_visit_cols, paste0("ae_visit_m", i) )
}

# er_cost_m
ane_cost_cols <- c()
for (i in 1:12){
      ane_cost_cols <- c(ane_cost_cols, paste0("er_cost_m", i))      
}

# Exposure group:
# A&E visits::
exp_ane_visit <- lc_exp_matched %>% 
      dplyr::select(patient_id, exposure, all_of(ane_visit_cols)) %>% 
      pivot_longer(
            cols = all_of(ane_visit_cols),
            names_to = c("month"),
            values_to = "ane_visit"
      ) %>% 
      mutate(month = as.numeric(str_sub(month, 11))) 

# A&E cost: 
exp_ane_cost <-  lc_exp_matched %>% 
      dplyr::select(patient_id, exposure, all_of(ane_cost_cols)) %>% 
      pivot_longer(
            cols = all_of(ane_cost_cols),
            names_to = c("month"),
            values_to = "ane_cost"
      ) %>% 
      mutate(month = as.numeric(str_sub(month, 10))) %>% 
      mutate(ane_cost = ifelse(!is.na(ane_cost), ane_cost, 0)) # change NA to 0


# c. Merge two data together by id, exp, month------
# join visit and cost
exp_ane_impute <- left_join(exp_ane_visit, exp_ane_cost, 
                            by = c("patient_id" = "patient_id", 
                                   "exposure" = "exposure",
                                   "month" = "month"))
# Join follow up
exp_ane_impute <- left_join(exp_fu_ts, exp_ane_impute,
                            by = c("patient_id" = "patient_id", 
                                   "exposure" = "exposure",
                                   "month_fu" = "month"))


# d. filter non-NA follow-up -----
# e. Mutate a new cost_var if visit != 0 & cost_var ==0, -----
#    impute by visit*unit cost
exp_ane_impute <- exp_ane_impute %>% 
      filter(!is.na(follow_up_time)) %>% 
      mutate(ane_costs_impute=case_when(
            ane_visit!=0 & ane_cost!=0 ~ ane_cost,
            ane_visit!=0 & ane_cost==0 ~ ane_visit*unit_ane_cost$unit_cost,
            ane_visit==0 & ane_cost!=0 ~ 0,
            ane_visit==0 & ane_cost==0 ~ 0
      ))

exp_ane_cost_12m <- exp_ane_impute %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            ane_cost_impute_12m = sum(ane_costs_impute, na.rm = T)) %>% 
      ungroup()






























# Combine two datasets: 
matched_cost_ts <- bind_rows(exp_fu_ts, com_fu_ts)
# fix the exposure levels
matched_cost_ts$exposure <- factor(matched_cost_ts$exposure, levels = c("Comparator", "Long covid exposure"))
matched_cost_ts$exposure  %>% levels()

# Data management: colllapsing data by different follow-up time. -------
# 12 months
matched_cost_12m <- matched_cost_ts %>% 
      filter(!is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup()

matched_cost_ts %>% names
# # Add covariates for adjustment
for_covariates <- bind_rows(lc_exp_matched, com_matched) %>% 
      distinct(patient_id, exposure, .keep_all = T) %>% 
      dplyr::select("patient_id",     
                    "exposure",           
                    "age", "age_cat",               
                    "sex",                     
                    "bmi_cat",
                    "ethnicity_6",             
                    "imd_q5",                  
                    "region",      
                    "cov_asthma",
                    "cov_mental_health",   
                    "previous_covid_hosp",     
                    "cov_covid_vax_n_cat",     
                    "number_comorbidities_cat")

levels_check <- c("exposure", "sex", "bmi_cat", "ethnicity_6", "imd_q5",                  
                  "region", "previous_covid_hosp", "cov_covid_vax_n_cat", "number_comorbidities_cat")


for_covariates$sex <- relevel(for_covariates$sex, ref = "male")
for_covariates$bmi_cat <- relevel(for_covariates$bmi_cat, ref = "Normal Weight")
for_covariates$ethnicity_6 <- relevel(for_covariates$ethnicity_6, ref = "White")
for_covariates$imd_q5 <- relevel(for_covariates$imd_q5, ref = "least_deprived")
for_covariates$region <- relevel(for_covariates$region, ref = "London" )
for_covariates$cov_mental_health <- relevel(for_covariates$cov_mental_health, ref = "FALSE")
for_covariates$previous_covid_hosp <- relevel(for_covariates$previous_covid_hosp, ref = "FALSE")
for_covariates$previous_covid_hosp <- relevel(for_covariates$previous_covid_hosp, ref = "FALSE")
for_covariates$cov_covid_vax_n_cat <- relevel(for_covariates$cov_covid_vax_n_cat, ref = "0 dose")
for_covariates$number_comorbidities_cat <- relevel(for_covariates$number_comorbidities_cat, ref = "0")

lapply(for_covariates[levels_check], levels) # need to correct some levels

# # add covariates back to the summarised data frame
matched_cost_12m <- left_join(matched_cost_12m, for_covariates,
                              by = c("patient_id" = "patient_id", "exposure" = "exposure"))

# Make sure the exposure level is correct
matched_cost_12m$exposure <- factor(matched_cost_12m$exposure, levels = c("Comparator", "Long covid exposure"))
