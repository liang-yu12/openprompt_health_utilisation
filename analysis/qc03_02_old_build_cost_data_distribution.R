source("analysis/dm01_02_v0_qc1_old_build_cost.R")

# Goal: check the secondary cost distribution

# Check total_admission_costs, total_ane_costs, total_opa_cost

hos <- matched_combine %>% filter(total_admission_costs > 0 & !is.na(total_admission_costs)) %>% 
      summarise(
            minimal = min(total_admission_costs, na.rm = T),
            median = median(total_admission_costs, na.rm = T),
            mean = mean(total_admission_costs, na.rm = T),
            max = max(total_admission_costs, na.rm = T)) %>% 
      mutate(Table = "APC") %>% relocate(Table)

hos_count <-  matched_combine  %>% summarise(
      zero_count = sum(total_admission_costs==0, na.rm = T),
      missing_count = sum(is.na(total_admission_costs), na.rm = T),
      total_n = n()
)


ec <- matched_combine %>% filter(total_ane_costs > 0 & !is.na(total_ane_costs)) %>% 
      summarise(
            minimal = min(total_ane_costs, na.rm = T),
            median = median(total_ane_costs, na.rm = T),
            mean = mean(total_ane_costs, na.rm = T),
            max = max(total_ane_costs, na.rm = T)) %>% 
      mutate(Table = "EC") %>% 
      relocate(Table)


ec_count <-  matched_combine  %>% summarise(
      zero_count = sum(total_ane_costs==0, na.rm = T),
      missing_count = sum(is.na(total_ane_costs), na.rm = T),
      total_n = n()
)

opa <- matched_combine %>% filter(total_opa_cost > 0 & !is.na(total_opa_cost)) %>% 
      summarise(
            minimal = min(total_opa_cost, na.rm = T),
            median = median(total_opa_cost, na.rm = T),
            mean = mean(total_opa_cost, na.rm = T),
            max = max(total_opa_cost, na.rm = T)) %>% 
      mutate(Table = "OPA") %>% 
      relocate(Table)

opa_count <-  matched_combine  %>% summarise(
      zero_count = sum(total_opa_cost==0, na.rm = T),
      missing_count = sum(is.na(total_opa_cost), na.rm = T),
      total_n = n()
)

# matched_combine results
bind_rows(
      bind_cols(hos, hos_count),
      bind_cols(ec, ec_count),
      bind_cols(opa, opa_count)
) %>% write_csv(here("output", "qc03_02_v0_oldbuild_all_cost_distribution.csv"))