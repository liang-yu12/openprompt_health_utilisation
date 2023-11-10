source("analysis/dm03_03_v2_now_pivot_total_long_inputed_cost.R")

# Goal: check the secondary cost distribution

description_fn <- function(dataset, var, cost_table){
      results <- dataset %>% 
            summarise(
                  minimal = min(var, na.rm = T),
                  median = median(var, na.rm = T),
                  mean = mean(var, na.rm = T),
                  max = max(var, na.rm = T)
            ) %>% mutate(Table = cost_table) %>% 
            relocate(Table)
      return(results)
}

description_group_fn <- function(dataset, var, cost_table){
      results <- dataset %>% group_by(exposure) %>% 
            summarise(
                  minimal = min(var, na.rm = T),
                  median = median(var, na.rm = T),
                  mean = mean(var, na.rm = T),
                  max = max(var, na.rm = T)
            ) %>% mutate(Table = cost_table) %>% 
            relocate(Table)
      return(results)
}



# Check total_admission_costs, total_ane_costs, total_opa_cost
combine <- bind_rows(lc_exp_matched, com_matched)

# First need to exclude people without any cost data:
hos <- combine %>% filter(total_admission_costs > 0)
ane <- combine %>% filter(total_ane_costs > 0)
opa <- combine %>% filter(total_opa_cost > 0)

bind_rows(
      description_fn(hos, hos$total_admission_costs, "APC"),
      description_fn(ane, ane$total_ane_costs, "EC (A&E)"),
      description_fn(opa, opa$total_opa_cost, "OPA"),
) %>% write_csv(here("output", "qc03_01_study_all_cost_distribution.csv"))


bind_rows(
      description_group_fn(hos, hos$total_admission_costs, "APC"),
      description_group_fn(ane, ane$total_ane_costs, "EC (A&E)"),
      description_group_fn(opa, opa$total_opa_cost, "OPA"),
) %>% write_csv(here("output", "qc03_01_study_by_exp_cost_distribution.csv"))

