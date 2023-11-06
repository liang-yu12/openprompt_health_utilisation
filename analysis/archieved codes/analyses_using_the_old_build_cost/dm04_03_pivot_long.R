source("analysis/dm04_02_combine_costs.R")
# Goal: pivot the total_cost_ into long form for analysis

# set the columns to pivot
total_cost <- c()
for (i in 1:12) {
      total_cost <- c(total_cost, paste0("total_cost_", i))
}

# pivot exposure group: -----
# pivot costs
exp_cost_ts <- lc_exp_matched %>% 
      pivot_longer(
            cols = all_of(total_cost),
            names_to = c("month"),
            values_to = "monthly_total_cost"
      )

exp_cost_ts$month <- str_sub(exp_cost_ts$month, 12) # remove "total_cost_"
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
            cols = all_of(total_cost),
            names_to = c("month"),
            values_to = "monthly_total_cost"
      )

com_cost_ts$month <- str_sub(com_cost_ts$month, 12)
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
matched_cost_ts <- bind_rows(exp_cost_long, com_cost_long)
# fix the exposure levels
matched_cost_ts$exposure <- relevel(matched_cost_ts$exposure, ref = "Comparator")

