# Goal: Data management for pivoting opa_visit
# run the analyses using different outcomes: 
# But need to reshape the data by different healthcare visits # opa_visit

# Load previous data management
source("analysis/dm01_02_now_monthly_follow_up.R")

# opa vivists:
opa_visit_cols <- lc_exp_matched[grep("opa_visit", names(lc_exp_matched))] %>% 
      names() %>% as.vector()
# Follow-up time:
fu_cols <- lc_exp_matched[grep("follow_up_m", names(lc_exp_matched))] %>% 
      names %>% as.vector()

#Pivot opa visits in the exposure dataset: ==================
exp_opa_visit_ts <- lc_exp_matched %>% 
      pivot_longer(
            cols = all_of(opa_visit_cols),
            names_to = c("month"),
            values_to = "monthly_opa_visits"
      )
exp_opa_visit_ts$month <- str_sub(exp_opa_visit_ts$month, 12) # remove opa_visit_m
exp_opa_visit_ts$month <- as.numeric(exp_opa_visit_ts$month)

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
exp_opa_long <- left_join(exp_opa_visit_ts, exp_fu_ts,
                      by = c("patient_id" = "patient_id", "month" = "month_fu")
)
exp_opa_long %>% names # looks good

rm(exp_opa_visit_ts) # housekeeping

# Pivot the comparator ==========
com_opa_visit_ts <- com_matched %>% 
      pivot_longer(
            cols = all_of(opa_visit_cols),
            names_to = c("month"),
            values_to = "monthly_opa_visits"
      )
com_opa_visit_ts$month <- str_sub(com_opa_visit_ts$month, 12) # remove opa_visit_m
com_opa_visit_ts$month <- as.numeric(com_opa_visit_ts$month)

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
com_opa_long <- left_join(com_opa_visit_ts, com_fu_ts,
                      by = c("patient_id" = "patient_id", "month" = "month_fu")
)
com_opa_long %>% names
com_opa_long$follow_up_time %>% summary

matched_data_opa_ts <- bind_rows(exp_opa_long, com_opa_long)

matched_data_opa_ts$exposure <- factor(matched_data_opa_ts$exposure, levels = c("Comparator", "Long covid exposure"))
matched_data_opa_ts$exposure %>% levels()
