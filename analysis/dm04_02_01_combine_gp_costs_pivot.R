source("analysis/dm04_02_combine_costs.R")

# Data management of the GP cost data for two part model

# Combine GP costs (gp consultation and prescriptions) -----

# Exposure group
lc_exp_matched$total_gp_cost_1 <- rowSums(lc_exp_matched[,c("primary_cost_1")])
lc_exp_matched$total_gp_cost_2 <- rowSums(lc_exp_matched[,c("primary_cost_2")])
lc_exp_matched$total_gp_cost_3 <- rowSums(lc_exp_matched[,c("primary_cost_3")])
lc_exp_matched$total_gp_cost_4 <- rowSums(lc_exp_matched[, c("primary_cost_4")])
lc_exp_matched$total_gp_cost_5 <- rowSums(lc_exp_matched[, c("primary_cost_5")])
lc_exp_matched$total_gp_cost_6 <- rowSums(lc_exp_matched[, c("primary_cost_6")])
lc_exp_matched$total_gp_cost_7 <- rowSums(lc_exp_matched[, c("primary_cost_7")])
lc_exp_matched$total_gp_cost_8 <- rowSums(lc_exp_matched[, c("primary_cost_8")])
lc_exp_matched$total_gp_cost_9 <- rowSums(lc_exp_matched[, c("primary_cost_9")])
lc_exp_matched$total_gp_cost_10 <- rowSums(lc_exp_matched[, c("primary_cost_10")])
lc_exp_matched$total_gp_cost_11 <- rowSums(lc_exp_matched[, c("primary_cost_11")])
lc_exp_matched$total_gp_cost_12 <- rowSums(lc_exp_matched[, c("primary_cost_12")])

# Comparator group:
com_matched$total_gp_cost_1 <- rowSums(com_matched[,c("primary_cost_1")])
com_matched$total_gp_cost_2 <- rowSums(com_matched[,c("primary_cost_2")])
com_matched$total_gp_cost_3 <- rowSums(com_matched[,c("primary_cost_3")])
com_matched$total_gp_cost_4 <- rowSums(com_matched[, c("primary_cost_4")])
com_matched$total_gp_cost_5 <- rowSums(com_matched[, c("primary_cost_5")])
com_matched$total_gp_cost_6 <- rowSums(com_matched[, c("primary_cost_6")])
com_matched$total_gp_cost_7 <- rowSums(com_matched[, c("primary_cost_7")])
com_matched$total_gp_cost_8 <- rowSums(com_matched[, c("primary_cost_8")])
com_matched$total_gp_cost_9 <- rowSums(com_matched[, c("primary_cost_9")])
com_matched$total_gp_cost_10 <- rowSums(com_matched[, c("primary_cost_10")])
com_matched$total_gp_cost_11 <- rowSums(com_matched[, c("primary_cost_11")])
com_matched$total_gp_cost_12 <- rowSums(com_matched[, c("primary_cost_12")])

# Pivot the GP cost ------

# set the columns to pivot
total_gp_cost <- c()
for (i in 1:12) {
      total_gp_cost <- c(total_gp_cost, paste0("total_gp_cost_", i))
}


# pivot exposure group: -----
# pivot costs
exp_cost_ts <- lc_exp_matched %>% 
      pivot_longer(
            cols = all_of(total_gp_cost),
            names_to = c("month"),
            values_to = "monthly_gp_cost"
      )

exp_cost_ts$month <- str_sub(exp_cost_ts$month, 15) # remove "total_gp_cost_"
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
            cols = all_of(total_gp_cost),
            names_to = c("month"),
            values_to = "monthly_gp_cost"
      )

com_cost_ts$month <- str_sub(com_cost_ts$month, 15)
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
matched_gp_cost_ts <- bind_rows(exp_cost_long, com_cost_long)
# fix the exposure levels
matched_gp_cost_ts$exposure <- relevel(matched_gp_cost_ts$exposure, ref = "Comparator")

