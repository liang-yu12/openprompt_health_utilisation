# Goal: Data management for pivoting gp_visit
# run the analyses using different outcomes: 
# But need to reshape the data by different healthcare visits # gp_visit

# Load previous data management
source("analysis/dm01_02_now_monthly_follow_up.R")

# GP vivists:
gp_visit_cols <- lc_exp_matched[grep("gp_visit", names(lc_exp_matched))] %>% 
      names() %>% as.vector()
# Follow-up time:
fu_cols <- lc_exp_matched[grep("follow_up_m", names(lc_exp_matched))] %>% 
      names %>% as.vector()

#Pivot GP visits in the exposure dataset: ==================
exp_gp_visit_ts <- lc_exp_matched %>% 
      pivot_longer(
            cols = all_of(gp_visit_cols),
            names_to = c("month"),
            values_to = "monthly_gp_visits"
      )
exp_gp_visit_ts$month <- str_sub(exp_gp_visit_ts$month, 11) # remove gp_visit_m
exp_gp_visit_ts$month <- as.numeric(exp_gp_visit_ts$month)

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
exp_gp_long <- left_join(exp_gp_visit_ts, exp_fu_ts,
                      by = c("patient_id" = "patient_id", "month" = "month_fu")
)
exp_gp_long %>% names # looks good

rm(exp_gp_visit_ts) # housekeeping

# Pivot the comparator ==========
com_gp_visit_ts <- com_matched %>% 
      pivot_longer(
            cols = all_of(gp_visit_cols),
            names_to = c("month"),
            values_to = "monthly_gp_visits"
      )
com_gp_visit_ts$month <- str_sub(com_gp_visit_ts$month, 11) # remove gp_visit_m
com_gp_visit_ts$month <- as.numeric(com_gp_visit_ts$month)

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
com_gp_long <- left_join(com_gp_visit_ts, com_fu_ts,
                      by = c("patient_id" = "patient_id", "month" = "month_fu")
)
com_gp_long %>% names
com_gp_long$follow_up_time %>% summary

matched_data_gp_ts <- bind_rows(exp_gp_long, com_gp_long)

matched_data_gp_ts$exposure <- factor(matched_data_gp_ts$exposure, levels = c("Comparator", "Long covid exposure"))
matched_data_gp_ts$exposure %>% levels