# Overall goal: Data management for unit hospitalisation costs:
# 1. Pivot the hospitalisation counts
# 2. Pivot the hospitalisation costs
# 3. Filter the !is.na() rows
# 4. Collapse the rows by visits and costs
# 5. use the summary counts and costs to sstimate the unit costs (costs/unit)


# Goal: calculate ane unit cost 

# read in apc visit count data
source("analysis/dm03_7_pivot_hos_long.R")
# keep var needed
apc_counts <- matched_data_hos_ts %>% dplyr::select("patient_id", "exposure", "month", "monthly_hos_visits")

# read in new apc cost data
source("analysis/new_dm04_02_02_apc_costs_pivot.R")
# keep var needed
apc_costs <- matched_apc_cost_ts %>%  dplyr::select("patient_id", "exposure", "month", "monthly_apc_cost")

# Join them together
apc_data_with_cost_counts <- left_join(apc_counts, apc_costs,
          by = c("patient_id" = "patient_id", 
                 "exposure" = "exposure",
                 "month" = "month")
          )

apc_new_unit_cost <- apc_data_with_cost_counts %>% 
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
apc_new_unit_cost %>% write_csv(here("output", "st04_apc_new_unit_costs.csv"))
