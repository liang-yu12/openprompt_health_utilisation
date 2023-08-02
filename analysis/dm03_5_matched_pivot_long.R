# Load previous data management
source("analysis/dm03_matched_define_monthly_follow_up_time.R")


# concept: 
# 1. Transpose the healthcare utilisation first, save it to an object;
# 2. Transpose the follow-up time, save it to another object;
# 3. Make sure they have the same row counts, and then cbind them

# Pivot the exposure group: lc_exp_matched

# Pivot the healthcare utilisation: ==============
visit_cols <- lc_exp_matched[grep("all_month_", names(lc_exp_matched))] %>% 
      names() %>% as.vector()

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
matched_data_ts$exposure %>% levels()
matched_data_ts$exposure <- relevel(matched_data_ts$exposure, "Comparator")
