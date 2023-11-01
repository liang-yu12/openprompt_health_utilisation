# Goal: Data management for pivoting drug_visit
# run the analyses using different outcomes: 
# But need to reshape the data by different healthcare visits # drug_visit

# Load previous data management
source("analysis/dm03_matched_define_monthly_follow_up_time.R")

# setup vectors for data management: -----
# prescription counts:
drug_visit_cols <- c()
for (i in 1:12){
      drug_visit_cols <- c(drug_visit_cols, paste0("prescription_", i))
}

# Follow-up time:
fu_cols <- c()
for (i in 1:12){
      fu_cols <- c(fu_cols, paste0("follow_up_m", i))
}


#Pivot drug visits in the exposure dataset: ==================
exp_drug_visit_ts <- lc_exp_matched %>% 
      pivot_longer(
            cols = all_of(drug_visit_cols),
            names_to = c("month"),
            values_to = "monthly_drug_visits"
      )
exp_drug_visit_ts$month <- str_sub(exp_drug_visit_ts$month, 14) # remove drug_visit_m
exp_drug_visit_ts$month <- as.numeric(exp_drug_visit_ts$month)

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
exp_drug_long <- left_join(exp_drug_visit_ts, exp_fu_ts,
                      by = c("patient_id" = "patient_id", "month" = "month_fu")
)
exp_drug_long %>% names # looks good

rm(exp_drug_visit_ts) # housekeeping

# Pivot the comparator ==========
com_drug_visit_ts <- com_matched %>% 
      pivot_longer(
            cols = all_of(drug_visit_cols),
            names_to = c("month"),
            values_to = "monthly_drug_visits"
      )
com_drug_visit_ts$month <- str_sub(com_drug_visit_ts$month, 14) # remove drug_visit_m
com_drug_visit_ts$month <- as.numeric(com_drug_visit_ts$month)

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
com_drug_long <- left_join(com_drug_visit_ts, com_fu_ts,
                      by = c("patient_id" = "patient_id", "month" = "month_fu")
)
com_drug_long %>% names
com_drug_long$follow_up_time %>% summary

matched_data_drug_ts <- bind_rows(exp_drug_long, com_drug_long)
matched_data_drug_ts$exposure <- factor(matched_data_drug_ts$exposure, 
                                        levels = c("Comparator", "Long covid exposure"))
