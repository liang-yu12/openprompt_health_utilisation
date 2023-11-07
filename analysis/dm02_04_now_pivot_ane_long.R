# Goal: Data management for pivoting ae_visit
# run the analyses using different outcomes: 
# But need to reshape the data by different healthcare visits # ae_visit

# Load previous data management
source("analysis/dm01_02_now_monthly_follow_up.R")

# ae vivists:
ae_visit_cols <- lc_exp_matched[grep("ae_visit", names(lc_exp_matched))] %>% 
      names() %>% as.vector()
# Follow-up time:
fu_cols <- lc_exp_matched[grep("follow_up_m", names(lc_exp_matched))] %>% 
      names %>% as.vector()

#Pivot ae visits in the exposure dataset: ==================
exp_ae_visit_ts <- lc_exp_matched %>% 
      pivot_longer(
            cols = all_of(ae_visit_cols),
            names_to = c("month"),
            values_to = "monthly_ae_visits"
      )
exp_ae_visit_ts$month <- str_sub(exp_ae_visit_ts$month, 11) # remove ae_visit_m
exp_ae_visit_ts$month <- as.numeric(exp_ae_visit_ts$month)

# Pivot the follow_up time in the exposure data: ========================
exp_fu_ts <- lc_exp_matched %>% dplyr::select(patient_id, all_of(fu_cols)) %>% 
      pivot_longer(
            cols = all_of(fu_cols),
            names_to = c("month_fu"),
            values_to = "follow_up_time"
      )

exp_fu_ts$month_fu <- str_sub(exp_fu_ts$month_fu, 12)  # remove "follow_up_m"
exp_fu_ts$month_fu <- as.numeric(exp_fu_ts$month_fu)

# # Combine the exposure data: 
exp_ae_long <- left_join(exp_ae_visit_ts, exp_fu_ts,
                      by = c("patient_id" = "patient_id", "month" = "month_fu")
)
exp_ae_long %>% names # looks good

rm(exp_ae_visit_ts) # housekeeping

# Pivot the comparator ==========
com_ae_visit_ts <- com_matched %>% 
      pivot_longer(
            cols = all_of(ae_visit_cols),
            names_to = c("month"),
            values_to = "monthly_ae_visits"
      )
com_ae_visit_ts$month <- str_sub(com_ae_visit_ts$month, 11) # remove ae_visit_m
com_ae_visit_ts$month <- as.numeric(com_ae_visit_ts$month)

# Pivot the comparator follow_up time: ========================

com_fu_ts <- com_matched %>% dplyr::select(patient_id, all_of(fu_cols)) %>% 
      pivot_longer(
            cols = all_of(fu_cols),
            names_to = c("month_fu"),
            values_to = "follow_up_time"
      )

com_fu_ts$month_fu <- str_sub(com_fu_ts$month_fu, 12)  # remove "follow_up_m"
com_fu_ts$month_fu <- as.numeric(com_fu_ts$month_fu)


# Combine the data: =============
com_ae_long <- left_join(com_ae_visit_ts, com_fu_ts,
                      by = c("patient_id" = "patient_id", "month" = "month_fu")
)
com_ae_long %>% names
com_ae_long$follow_up_time %>% summary

matched_data_ae_ts <- bind_rows(exp_ae_long, com_ae_long)
matched_data_ae_ts$exposure %>% levels
matched_data_ae_ts$exposure <- factor(matched_data_ae_ts$exposure, levels = c("Comparator", "Long covid exposure"))
matched_data_hos_ts$exposure %>% levels()



# Data management for modeling:: --------
# Collapsing data by summarising the visits and follow-up time, and 
# generate three datasets for follow-up  12m

# follow 12 months 
matched_data_ae_12m <- matched_data_ae_ts %>% 
      filter(!is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_ae_visits),
            follow_up = sum(follow_up_time)) %>% 
      ungroup()


# # Add covariates for adjustment
for_covariates <- matched_data_ae_ts %>% distinct(patient_id, exposure, .keep_all = T) %>% 
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

# # add covariates back to the summarised data frame
matched_data_ae_12m <- left_join(matched_data_ae_12m, for_covariates,
                                 by = c("patient_id" = "patient_id", "exposure" = "exposure"))

# correct the level of exposure groups
matched_data_ae_12m$exposure <- relevel(matched_data_ae_12m$exposure, ref = "Comparator")

