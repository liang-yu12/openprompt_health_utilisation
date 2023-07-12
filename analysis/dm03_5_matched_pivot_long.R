# Load previous data management
source("analysis/dm03_matched_define_monthly_follow_up_time.R")


# concept: 
# 1. Transpose the healthcare utilisation first, save it to an object;
# 2. Transpose the follow-up time, save it to another object;
# 3. Make sure they have the same row counts, and then cbind them


# Pivot the healthcare utilisation: ==============
visit_cols <- matched_data[grep("all_month_", names(matched_data))] %>% 
      names() %>% as.vector()

visit_ts <- matched_data %>% 
      pivot_longer(
            cols = all_of(visit_cols),
            names_to = c("month"),
            values_to = "monthly_visits"
)
visit_ts$month <- str_sub(visit_ts$month, 12) # remove all_month_m
visit_ts$month <- as.numeric(visit_ts$month)


# Pivot the follow_up time: ========================
fu_cols <- matched_data[grep("follow_up_m", names(matched_data))] %>% 
      names %>% as.vector()

fu_ts <- matched_data %>% dplyr::select(patient_id, all_of(fu_cols)) %>% 
      pivot_longer(
            cols = all_of(fu_cols),
            names_to = c("month_fu"),
            values_to = "follow_up_time"
      )

fu_ts$month_fu <- str_sub(fu_ts$month_fu, 12)  # remove "follow_up_m"
fu_ts$month_fu <- as.numeric(fu_ts$month_fu)
fu_ts$patient_id <- NULL


# Combine the data: =============
matched_data_ts <- cbind(visit_ts, fu_ts)
matched_data_ts %>% names

table(matched_data_ts$month ==matched_data_ts$month_fu) # looks good


