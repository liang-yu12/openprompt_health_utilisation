source("analysis/dm03_03_v2_now_pivot_total_long_inputed_cost.R")

# Check the proportion of inputation
# APC
apc_summary <- matched_cost_12m %>% summarise(
  Total_n = n(),
  non_zero_visit = sum(total_admission_counts > 0, na.rm = T),
  no_cost = sum(total_admission_counts > 0 & total_admission_costs==0, na.rm = T),
  mean_raw_cost = mean(total_admission_costs, na.rm = T),
  mean_inpute_cost = mean(apc_costs_inputed, na.rm = T)) %>% 
  mutate(unit_cost = unit_apc_costs$unit_cost) %>% 
  mutate(Table = "APC") %>% relocate(Table)

# A&E
ane_summary <- matched_cost_12m %>% summarise(
  Total_n = n(),
  non_zero_visit = sum(total_ane_visits > 0, na.rm = T),
  no_cost = sum(total_ane_visits > 0 & total_ane_costs==0, na.rm = T),
  mean_raw_cost = mean(total_ane_costs, na.rm = T),
  mean_inpute_cost = mean(ane_costs_inputed, na.rm = T)) %>% 
  mutate(unit_cost = unit_ane_cost$unit_cost) %>% 
  mutate(Table = "EC(A&E)") %>% relocate(Table)

# OPA 
opa_summary <- matched_cost_12m %>% summarise(
  Total_n = n(),
  non_zero_visit = sum(total_opa_visit > 0, na.rm = T),
  no_cost = sum(total_opa_visit > 0 & total_opa_cost==0, na.rm = T),
  mean_raw_cost = mean(total_opa_cost, na.rm = T),
  mean_inpute_cost = mean(opa_costs_inputed, na.rm = T)) %>% 
  mutate(unit_cost = unit_opa_cost$unit_cost) %>% 
  mutate(Table = "OPA") %>% relocate(Table)


# Check total change:
total <- matched_cost_12m %>% mutate(
  raw_2nd_care_cost = total_admission_costs + total_ane_costs + total_opa_cost,
  raw_total_cost = total_primary_cost+raw_2nd_care_cost)

total_summary <- total %>% summarise(
  Total_n = n(),
  non_zero_visit = sum(raw_total_cost>0, na.rm = T),
  no_cost = NA, 
  mean_raw_cost = mean(raw_total_cost, na.rm = T),
  mean_inpute_cost = mean(total_cost, na.rm = T),
  unit_cost = NA) %>% 
  mutate(Table = "Total") %>% relocate(Table)
  
bind_rows(
  apc_summary, 
  ane_summary,
  opa_summary,
  total_summary) %>% write_csv(here("output", "qc03_03_compare_2nd_care_inputation.csv"))
