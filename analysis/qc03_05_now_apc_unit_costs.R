# Overall goal: Data management for unit hospitalisation costs:
# 1. Pivot the hospitalisation counts
# 2. Pivot the hospitalisation costs
# 3. Filter the !is.na() rows
# 4. Collapse the rows by visits and costs
# 5. use the summary counts and costs to sstimate the unit costs (costs/unit)

source("analysis/dm01_02_now_monthly_follow_up.R")
matched_combine <- bind_rows(com_matched, lc_exp_matched)

# define admin cols
all_admission <- c()
for (i in 1:12){
      all_admission <-c(all_admission, paste0("hos_visit_m", i))      
}

# define cost cols
all_apc_cost <- c()
for(i in 1:12){
      all_apc_cost <- c(all_apc_cost, paste0("apc_cost_m", i))
}

# add up total admission counts
matched_combine$total_admission_counts <- rowSums(matched_combine[,all_admission], na.rm = T)

# add up total costs
matched_combine$total_admission_costs <- rowSums(matched_combine[,all_apc_cost], na.rm = T)


# Keep people with both records for inputation
unit_apc_costs <- matched_combine %>% 
      filter(total_admission_counts > 0 & total_admission_costs > 0) %>% 
      summarise(unit_cost = sum(total_admission_costs, na.rm = T)/sum(total_admission_counts, na.rm = T))


# save outputs 
unit_apc_costs %>% write_csv(here("output", "st03_05_apc_unit_costs.csv"))
