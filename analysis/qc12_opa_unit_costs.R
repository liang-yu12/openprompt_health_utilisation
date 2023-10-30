# read in opa visit count data
source("analysis/dm03_9_pivot_opa_long.R")
# keep var needed
opa_counts <- matched_data_opa_ts %>% dplyr::select("patient_id", "exposure", "month", "monthly_opa_visits")


# read in opa cost data
source("analysis/dm04_02_04_opa_costs_pivot.R")
# keep var needed
opa_costs <- matched_opa_cost_ts %>% dplyr::select("patient_id", "exposure", "month", "monthly_opa_cost")

# Join them together
opa_data_with_cost_counts <- left_join(opa_counts, opa_costs,
                                       by = c("patient_id" = "patient_id", 
                                              "exposure" = "exposure",
                                              "month" = "month")
)

unit_cost <- opa_data_with_cost_counts %>% 
      filter(!is.na(monthly_opa_visits) & 
                   monthly_opa_visits > 0 &
                   !is.na(monthly_opa_cost) & 
                   monthly_opa_cost > 0) %>%  # keep people with both data
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_opa_visits, na.rm = T),
            costs = sum(monthly_opa_cost, na.rm = T)) %>% ungroup() %>% 
      summarise(unit_cost = sum(costs, na.rm = T)/sum(visits, na.rm = T))

# save outputs 
unit_cost %>% write_csv(here("output", "st04_opa_unit_costs.csv"))
