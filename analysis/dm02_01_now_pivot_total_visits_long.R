# Load previous data management
source("analysis/dm01_02_now_monthly_follow_up.R")
# concept: 
# 1. Transpose the healthcare utilisation first, save it to an object;
# 2. Transpose the follow-up time, save it to another object;
# 3. Make sure they have the same row counts, and then cbind them

# Pivot the exposure group: lc_exp_matched

# Pivot the healthcare utilisation: ==============
visit_cols <- c()
for (i in 1:12){
      visit_cols <- c(visit_cols, paste0("all_month_m", i))
}

exp_visit_ts <- lc_exp_matched %>% 
      pivot_longer(
            cols = all_of(visit_cols),
            names_to = c("month"),
            values_to = "monthly_visits"
)
exp_visit_ts$month <- str_sub(exp_visit_ts$month, 12) # remove all_month_m
exp_visit_ts$month <- as.numeric(exp_visit_ts$month)


# Pivot the follow_up time: ========================
fu_cols <- lc_exp_matched[grep("follow_up_m", names(lc_exp_matched))] %>% 
      names %>% as.vector()

exp_fu_ts <- lc_exp_matched %>% dplyr::select(patient_id, all_of(fu_cols)) %>% 
      pivot_longer(
            cols = all_of(fu_cols),
            names_to = c("month_fu"),
            values_to = "follow_up_time"
      )

exp_fu_ts$month_fu <- str_sub(exp_fu_ts$month_fu, 12)  # remove "follow_up_m"
exp_fu_ts$month_fu <- as.numeric(exp_fu_ts$month_fu)

# Combine the data: =============
exp_long <- left_join(exp_visit_ts, exp_fu_ts,
                      by = c("patient_id" = "patient_id", "month" = "month_fu")
                      )
exp_long %>% names # looks good


# Pivot the comparator dataset: ------------
com_visit_ts <- com_matched %>% 
      pivot_longer(
            cols = all_of(visit_cols),
            names_to = c("month"),
            values_to = "monthly_visits"
      )
com_visit_ts$month <- str_sub(com_visit_ts$month, 12) # remove all_month_m
com_visit_ts$month <- as.numeric(com_visit_ts$month)


# Pivot the follow_up time: ========================

com_fu_ts <- com_matched %>% dplyr::select(patient_id, all_of(fu_cols)) %>% 
      pivot_longer(
            cols = all_of(fu_cols),
            names_to = c("month_fu"),
            values_to = "follow_up_time"
      )

com_fu_ts$month_fu <- str_sub(com_fu_ts$month_fu, 12)  # remove "follow_up_m"
com_fu_ts$month_fu <- as.numeric(com_fu_ts$month_fu)

# Combine the data: =============
com_long <- left_join(com_visit_ts, com_fu_ts,
                      by = c("patient_id" = "patient_id", "month" = "month_fu")
)
com_long %>% names
com_long$follow_up_time %>% summary

# Combine two datasets: ----
matched_data_ts <- bind_rows(exp_long, com_long)
matched_data_ts$exposure <- factor(matched_data_ts$exposure, levels = c("Comparator", "Long covid exposure"))


# further data management for modeling:: --------
# Collapsing data by summarising the visits and follow-up time, and 
# generate a dataset for follow-up 12m

# follow 12 months 
matched_data_12m <- matched_data_ts %>% 
      filter(!is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_visits),
            follow_up = sum(follow_up_time)) %>% 
      ungroup()


# # Add covariates and additional outcome (prescription) for adjustment
for_covariates <- matched_data_ts %>% distinct(patient_id, exposure, .keep_all = T) %>% 
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
                    "total_drug_visit")

for_covariates$sex <- relevel(for_covariates$sex, ref = "male")
for_covariates$bmi_cat <- relevel(for_covariates$bmi_cat, ref = "Normal Weight")
for_covariates$ethnicity_6 <- relevel(for_covariates$ethnicity_6, ref = "White")
for_covariates$imd_q5 <- relevel(for_covariates$imd_q5, ref = "least_deprived")
for_covariates$region <- relevel(for_covariates$region, ref = "London" )
for_covariates$cov_asthma <- relevel(for_covariates$cov_asthma, ref = "FALSE")
for_covariates$cov_mental_health <- relevel(for_covariates$cov_mental_health, ref = "FALSE")
for_covariates$previous_covid_hosp <- relevel(for_covariates$previous_covid_hosp, ref = "FALSE")
for_covariates$cov_covid_vax_n_cat <- relevel(for_covariates$cov_covid_vax_n_cat, ref = "0 dose")
for_covariates$number_comorbidities_cat <- relevel(for_covariates$number_comorbidities_cat, ref = "0")


# # add covariates back to the summarised data frame

matched_data_12m <- left_join(matched_data_12m, for_covariates,
                              by = c("patient_id" = "patient_id", "exposure" = "exposure"))


# add the total_drug_visit to the outcomes: 
matched_data_12m <- matched_data_12m %>% mutate(visits = visits + total_drug_visit)

# correct the level of exposure groups
matched_data_12m$exposure <- relevel(matched_data_12m$exposure, ref = "Comparator")
