source("analysis/dm03_03_v2_now_pivot_total_long_inputed_cost.R")

# Goal: check the secondary cost distribution

description_fn <- function(var, cost_table){
      result_1 <- combine %>% 
            filter(var > 0 & !is.na(var)) %>% 
            summarise(
                  minimal = min(var, na.rm = T),
                  median = median(var, na.rm = T),
                  mean = mean(var, na.rm = T),
                  max = max(var, na.rm = T)
            ) %>% mutate(Table = cost_table) %>% 
            relocate(Table)
      
      reault_2 <- combine  %>% summarise(
            zero_count = sum(var==0, na.rm = T),
            missing_count = sum(is.na(var), na.rm = T),
            total_n = n()
      )
      
      output <- bind_cols(result_1, reault_2)
      
      return(output)
}

description_group_fn <- function(var, cost_table){
      result_1 <- combine %>% 
            filter(var > 0 & !is.na(var)) %>% 
            group_by(exposure) %>%
            summarise(
                  minimal = min(var, na.rm = T),
                  median = median(var, na.rm = T),
                  mean = mean(var, na.rm = T),
                  max = max(var, na.rm = T)) %>% 
            mutate(Table = cost_table) %>% 
            relocate(Table)
      
      reault_2 <- combine %>% group_by(exposure) %>% 
            summarise(
                  zero_count = sum(var==0, na.rm = T),
                  missing_count = sum(is.na(var), na.rm = T),
                  total_n = n()
                  )
      output <- bind_cols(result_1, reault_2)
      
      return(output)
}



# Check total_admission_costs, total_ane_costs, total_opa_cost
combine <- bind_rows(lc_exp_matched, com_matched)

bind_rows(
      description_fn(combine$total_admission_costs, "APC"),
      description_fn(combine$total_ane_costs, "EC (A&E)"),
      description_fn(combine$total_opa_cost, "OPA"),
) %>% write_csv(here("output", "qc03_01_study_all_cost_distribution.csv"))


bind_rows(
      description_group_fn(combine$total_admission_costs, "APC"),
      description_group_fn(combine$total_ane_costs, "EC (A&E)"),
      description_group_fn(combine$total_opa_cost, "OPA"),
) %>% write_csv(here("output", "qc03_01_study_by_exp_cost_distribution.csv"))

