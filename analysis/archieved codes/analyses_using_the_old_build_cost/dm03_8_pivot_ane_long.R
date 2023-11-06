# Goal: Data management for pivoting ae_visit
# run the analyses using different outcomes: 
# But need to reshape the data by different healthcare visits # ae_visit

# Load previous data management
source("analysis/dm03_matched_define_monthly_follow_up_time.R")

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
matched_data_ae_ts$exposure <- relevel(matched_data_ae_ts$exposure, "Comparator")
