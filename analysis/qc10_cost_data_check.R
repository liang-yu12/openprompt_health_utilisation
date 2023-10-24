source("analysis/dm04_02_01_combine_gp_costs_pivot.R")

# Check the distribution of gp cost datasets ------
# check distribution by month
gp_cost_by_month <- matched_gp_cost_ts %>%
      group_by(month, exposure) %>% 
      summarise(
            gp_cost_min = min(monthly_gp_cost, na.rm = T),
            gp_cost_median = median(monthly_gp_cost),
            gp_cost_mean = mean(monthly_gp_cost, na.rm =T),
            gp_cost_max = max(monthly_gp_cost, na.rm = T),
            fu_time = mean(follow_up_time, na.rm =T),
            zero_count = sum(monthly_gp_cost==0),
            na_count = sum(is.na(monthly_gp_cost))) %>% 
      as.data.frame() %>% 
      mutate(month = as.character(month))

      
gp_cost_total <- 
      matched_gp_cost_ts %>% 
      filter(!is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            gp_costs = sum(monthly_gp_cost, na.rm = T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup() %>% 
      group_by(exposure) %>% 
      summarise(
            gp_cost_min = min(gp_costs, na.rm = T),
            gp_cost_median = median(gp_costs, na.rm = T),
            gp_cost_mean = mean(gp_costs, na.rm =T),
            gp_cost_max = max(gp_costs, na.rm = T),
            fu_time = mean(follow_up, na.rm =T),
            zero_count = sum(gp_costs==0, na.rm = T),
            na_count = sum(is.na(gp_costs), na.rm = T)) %>% 
      as.data.frame() %>% mutate(month = "Total 12 months") %>% 
      relocate(month)


# combine and save
bind_rows(gp_cost_total, gp_cost_by_month) %>% write_csv(here("output", "qc10_gp_cost_distribution.csv"))

# Hospital data check: ------
source("analysis/dm04_02_02_apc_costs_pivot.R")

# Monthly records
hos_cost_month <- matched_apc_cost_ts %>% 
      group_by(month, exposure) %>% 
      summarise(
            apc_cost_min = min(monthly_apc_cost, na.rm = T),
            apc_cost_median = median(monthly_apc_cost, na.rm = T),
            apc_cost_mean = mean(monthly_apc_cost, na.rm =T),
            apc_cost_max = max(monthly_apc_cost, na.rm = T),
            fu_time = mean(follow_up_time, na.rm =T),
            zero_count = sum(monthly_apc_cost==0),
            na_count = sum(is.na(monthly_apc_cost))) %>% 
      as.data.frame() %>% 
      mutate(month = as.character(month))

hos_cost_total <- matched_apc_cost_ts %>% 
      filter(!is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            apc_costs = sum(monthly_apc_cost, na.rm = T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup() %>% 
      group_by(exposure) %>% 
      summarise(
            apc_cost_min = min(apc_costs, na.rm = T),
            apc_cost_median = median(apc_costs, na.rm = T),
            apc_cost_mean = mean(apc_costs, na.rm =T),
            apc_cost_max = max(apc_costs, na.rm = T),
            fu_time = mean(follow_up, na.rm =T),
            zero_count = sum(apc_costs==0, na.rm = T),
            na_count = sum(is.na(apc_costs), na.rm = T)) %>% 
      as.data.frame() %>% mutate(month = "Total 12 months") %>% 
      relocate(month)

# combine and save: 
bind_rows(hos_cost_total, hos_cost_month) %>% write_csv(here("output", "qc10_hos_cost_distribution.csv"))

# A&E distribution ------
source("analysis/dm04_02_03_ane_costs_pivot.R")

ane_cost_month <- matched_ane_cost_ts %>% 
      group_by(month, exposure) %>% 
      summarise(
            ane_cost_min = min(monthly_ane_cost, na.rm = T),
            ane_cost_median = median(monthly_ane_cost, na.rm = T),
            ane_cost_mean = mean(monthly_ane_cost, na.rm =T),
            ane_cost_max = max(monthly_ane_cost, na.rm = T),
            fu_time = mean(follow_up_time, na.rm =T),
            zero_count = sum(monthly_ane_cost==0, na.rm = T),
            na_count = sum(is.na(monthly_ane_cost), na.rm = T)) %>% 
      as.data.frame() %>% 
      mutate(month = as.character(month))

ane_cost_total <- matched_ane_cost_ts %>% 
      filter(!is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            ane_costs = sum(monthly_ane_cost, na.rm = T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup() %>% 
      group_by(exposure) %>% 
      summarise(
            ane_cost_min = min(ane_costs, na.rm = T),
            ane_cost_median = median(ane_costs, na.rm = T),
            ane_cost_mean = mean(ane_costs, na.rm =T),
            ane_cost_max = max(ane_costs, na.rm = T),
            fu_time = mean(follow_up, na.rm =T),
            zero_count = sum(ane_costs==0, na.rm = T),
            na_count = sum(is.na(ane_costs), na.rm = T)) %>% 
      as.data.frame() %>% mutate(month = "Total 12 months") %>% 
      relocate(month)

bind_rows(ane_cost_total, ane_cost_month) %>% write_csv(here("output", "qc10_ane_cost_distribution.csv"))

# OPA costs: ----
source("analysis/dm04_02_04_opa_costs_pivot.R")


opa_cost_month <- matched_opa_cost_ts %>% 
      group_by(month, exposure) %>% 
      summarise(
            opa_cost_min = min(monthly_opa_cost, na.rm = T),
            opa_cost_median = median(monthly_opa_cost, na.rm = T),
            opa_cost_mean = mean(monthly_opa_cost, na.rm =T),
            opa_cost_max = max(monthly_opa_cost, na.rm = T),
            fu_time = mean(follow_up_time, na.rm =T),
            zero_count = sum(monthly_opa_cost==0, na.rm = T),
            na_count = sum(is.na(monthly_opa_cost), na.rm = T)) %>% 
      as.data.frame() %>% 
      mutate(month = as.character(month))

opa_cost_total <- matched_opa_cost_ts %>% 
      filter(!is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            opa_costs = sum(monthly_opa_cost, na.rm = T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup() %>% 
      group_by(exposure) %>% 
      summarise(
            opa_cost_min = min(opa_costs, na.rm = T),
            opa_cost_median = median(opa_costs, na.rm = T),
            opa_cost_mean = mean(opa_costs, na.rm =T),
            opa_cost_max = max(opa_costs, na.rm = T),
            fu_time = mean(follow_up, na.rm =T),
            zero_count = sum(opa_costs==0, na.rm = T),
            na_count = sum(is.na(opa_costs), na.rm = T)) %>% 
      as.data.frame() %>% mutate(month = "Total 12 months") %>% 
      relocate(month)

bind_rows(opa_cost_total, opa_cost_month) %>% write_csv(here("output", "qc10_opa_cost_distribution.csv"))
