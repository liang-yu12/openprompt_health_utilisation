source("analysis/dm03_02_v2_now_inpute_sec_care_cost_cal_total_costs.R")
# Goal: pivot the total_cost_ into long form for analysis

# Pivot the follow-up time

# pivot exposure group: -----

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

# Pivot the comparator group: ---------------
# Pivot the comparator follow_up time: 

com_fu_ts <- com_matched %>% dplyr::select(patient_id, exposure, all_of(fu_cols)) %>% 
      pivot_longer(
            cols = all_of(fu_cols),
            names_to = c("month_fu"),
            values_to = "follow_up_time"
      )

com_fu_ts$month_fu <- str_sub(com_fu_ts$month_fu, 12)  # remove "follow_up_m"
com_fu_ts$month_fu <- as.numeric(com_fu_ts$month_fu)

# Combine two datasets: ---------
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
                    "number_comorbidities_cat",
                    "total_cost", 
                    "apc_costs_inputed",
                    "ane_costs_inputed", 
                    "opa_costs_inputed")

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
