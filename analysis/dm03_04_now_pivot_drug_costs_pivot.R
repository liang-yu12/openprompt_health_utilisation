source("analysis/dm03_02_now_add_primary_sec_care_costs.R")

# Data management of the GP cost data for two part model

# Pivot the GP cost ------

# set the columns to pivot
total_drug_cost <- c()
for (i in 1:12) {
      total_drug_cost <- c(total_drug_cost, paste0("drug_cost_", i))
}


# pivot exposure group: -----
# pivot costs
exp_cost_ts <- lc_exp_matched %>% 
      pivot_longer(
            cols = all_of(total_drug_cost),
            names_to = c("month"),
            values_to = "monthly_drug_cost"
      )

exp_cost_ts$month <- str_sub(exp_cost_ts$month, 11) # remove "total_drug_cost_"
exp_cost_ts$month <- as.numeric(exp_cost_ts$month)

# pivot exposure follow-up time
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


# Combine the exposure data:

exp_cost_long <- left_join(exp_cost_ts, exp_fu_ts,
                           by = c("patient_id" = "patient_id", "month" = "month_fu"))



# Pivot the comparator group: ---------------
# Pivot the comparator costs
com_cost_ts <- com_matched %>% 
      pivot_longer(
            cols = all_of(total_drug_cost),
            names_to = c("month"),
            values_to = "monthly_drug_cost"
      )

com_cost_ts$month <- str_sub(com_cost_ts$month, 11)
com_cost_ts$month <- as.numeric(com_cost_ts$month)

# Pivot the comparator follow_up time: 

com_fu_ts <- com_matched %>% dplyr::select(patient_id, all_of(fu_cols)) %>% 
      pivot_longer(
            cols = all_of(fu_cols),
            names_to = c("month_fu"),
            values_to = "follow_up_time"
      )

com_fu_ts$month_fu <- str_sub(com_fu_ts$month_fu, 12)  # remove "follow_up_m"
com_fu_ts$month_fu <- as.numeric(com_fu_ts$month_fu)

# Combine dataset
com_cost_long <- left_join(com_cost_ts, com_fu_ts, 
                           by = c("patient_id" = "patient_id", "month" = "month_fu")
)



# Combine two datasets: ---------
matched_drug_cost_ts <- bind_rows(exp_cost_long, com_cost_long)
# fix the exposure levels
matched_drug_cost_ts$exposure <- factor(matched_drug_cost_ts$exposure, levels = c("Comparator", "Long covid exposure"))
matched_drug_cost_ts$exposure %>% levels()

# Data management for model ------

# 12 months
matched_cost_12m <- matched_drug_cost_ts %>% 
      filter(!is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            gp_cost = sum(monthly_drug_cost, na.rm =T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup()


# # Add covariates for adjustment
for_covariates <- matched_drug_cost_ts %>% distinct(patient_id, exposure, .keep_all = T) %>% 
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

levels_check <- c("exposure", "age_cat", "sex", "bmi_cat", "ethnicity_6", "imd_q5",                  
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
matched_cost_12m$exposure <- relevel(matched_cost_12m$exposure, ref = "Comparator")
