# Goal: Data management for pivoting hos_visit
# run the analyses using different outcomes: 
# But need to reshape the data by different healthcare visits # hos_visit

# Load previous data management
source("analysis/dm01_02_now_monthly_follow_up.R")

# hos vivists:
hos_visit_cols <- lc_exp_matched[grep("hos_visit", names(lc_exp_matched))] %>% 
      names() %>% as.vector()
# Follow-up time:
fu_cols <- lc_exp_matched[grep("follow_up_m", names(lc_exp_matched))] %>% 
      names %>% as.vector()

#Pivot hos_ visits in the exposure dataset: ==================
exp_hos_visit_ts <- lc_exp_matched %>% 
      pivot_longer(
            cols = all_of(hos_visit_cols),
            names_to = c("month"),
            values_to = "monthly_hos_visits"
      )
exp_hos_visit_ts$month <- str_sub(exp_hos_visit_ts$month, 12) # remove hos_visit_m
exp_hos_visit_ts$month <- as.numeric(exp_hos_visit_ts$month)

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
exp_hos_long <- left_join(exp_hos_visit_ts, exp_fu_ts,
                      by = c("patient_id" = "patient_id", "month" = "month_fu")
)
exp_hos_long %>% names # looks good

rm(exp_hos_visit_ts) # housekeeping

# Pivot the comparator ==========
com_hos_visit_ts <- com_matched %>% 
      pivot_longer(
            cols = all_of(hos_visit_cols),
            names_to = c("month"),
            values_to = "monthly_hos_visits"
      )
com_hos_visit_ts$month <- str_sub(com_hos_visit_ts$month, 12) # remove hos_visit_m
com_hos_visit_ts$month <- as.numeric(com_hos_visit_ts$month)

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
com_hos_long <- left_join(com_hos_visit_ts, com_fu_ts,
                      by = c("patient_id" = "patient_id", "month" = "month_fu")
)
com_hos_long %>% names
com_hos_long$follow_up_time %>% summary

matched_data_hos_ts <- bind_rows(exp_hos_long, com_hos_long)
matched_data_hos_ts$exposure <- factor(matched_data_hos_ts$exposure, levels = c("Comparator", "Long covid exposure"))
matched_data_hos_ts$exposure %>% levels()
