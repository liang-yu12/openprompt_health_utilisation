# Goal: calculate ane unit cost 

# read in opa visit count data
source("analysis/dm03_8_pivot_ane_long.R")
# keep var needed
ane_counts <- matched_data_ae_ts %>% dplyr::select("patient_id", "exposure", "month", "monthly_ae_visits")


# read in opa cost data
source("analysis/new_dm04_02_03_ane_costs_pivot.R")
# keep var needed
ane_costs <- matched_ane_cost_ts %>% dplyr::select("patient_id", "exposure", "month", "monthly_ane_cost")

# Join them together
ane_data_with_cost_counts <- left_join(ane_counts, ane_costs,
                                       by = c("patient_id" = "patient_id", 
                                              "exposure" = "exposure",
                                              "month" = "month")
)

ane_new_unit_cost <- ane_data_with_cost_counts %>% 
      filter(!is.na(monthly_ae_visits) & 
                   monthly_ae_visits > 0 &
                   !is.na(monthly_ane_cost) & 
                   monthly_ane_cost > 0) %>%  # keep people with both data
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_ae_visits, na.rm = T),
            costs = sum(monthly_ane_cost, na.rm = T)) %>% ungroup() %>% 
      summarise(unit_cost = sum(costs, na.rm = T)/sum(visits, na.rm = T))

# save outputs 
ane_new_unit_cost %>% write_csv(here("output", "st04_ane_new_unit_costs.csv"))
