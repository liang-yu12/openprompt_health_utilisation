# Overall goal: Data management for unit hospitalisation costs:
# 1. Pivot the hospitalisation counts
# 2. Pivot the hospitalisation costs
# 3. Filter the !is.na() rows
# 4. Collapse the rows by visits and costs
# 5. use the summary counts and costs to sstimate the unit costs (costs/unit)

# 1. [Pivot the visit data] ----

# run the analyses using different outcomes: 
# But need to reshape the data by different healthcare visits # hos_visit

# Load previous data management
source("analysis/dm03_matched_define_monthly_follow_up_time.R")

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
matched_data_hos_ts$exposure %>% levels
matched_data_hos_ts$exposure <- relevel(matched_data_hos_ts$exposure, "Comparator")


# 2. [Pivot the costs data] -----
source("analysis/dm04_02_combine_costs.R")

# Data management of the APC cost data for two part model

# Combine APC costs
# Pivot the apc cost ------

# set the columns to pivot
total_apc_cost <- c()
for (i in 1:12) {
      total_apc_cost <- c(total_apc_cost, paste0("apc_cost_m", i))
}

# pivot exposure group: -----
# pivot costs
exp_cost_ts <- lc_exp_matched %>% 
      pivot_longer(
            cols = all_of(total_apc_cost),
            names_to = c("month"),
            values_to = "monthly_apc_cost"
      )

exp_cost_ts$month <- str_sub(exp_cost_ts$month, 11) # remove "apc_cost_m"
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
            cols = all_of(total_apc_cost),
            names_to = c("month"),
            values_to = "monthly_apc_cost"
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
matched_apc_cost_ts <- bind_rows(exp_cost_long, com_cost_long)
# fix the exposure levels
matched_apc_cost_ts$exposure <- relevel(matched_apc_cost_ts$exposure, ref = "Comparator")


# 3. [Combine the two datasets] -----

# First reduce the variable counts:
apc_counts <- matched_data_hos_ts %>% dplyr::select("patient_id", "exposure", "month", "monthly_hos_visits")
apc_costs <- matched_apc_cost_ts %>%  dplyr::select("patient_id", "exposure", "month", "monthly_apc_cost")

# Join them together
apc_data_with_cost_counts <- left_join(apc_counts, apc_costs,
          by = c("patient_id" = "patient_id", 
                 "exposure" = "exposure",
                 "month" = "month")
          )

unit_cost <- apc_data_with_cost_counts %>% 
      filter(!is.na(monthly_hos_visits) & 
                   monthly_hos_visits > 0 &
                   !is.na(monthly_apc_cost) & 
                   monthly_apc_cost > 0) %>%  # keep people with both data
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_hos_visits, na.rm = T),
            costs = sum(monthly_apc_cost, na.rm = T)) %>% ungroup() %>% 
      summarise(unit_cost = sum(costs, na.rm = T)/sum(visits, na.rm = T))

# save outputs 
unit_cost %>% write_csv(here("output", "st04_apc_unit_costs.csv"))
