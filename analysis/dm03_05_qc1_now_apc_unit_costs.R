# Overall goal: Data management for unit hospitalisation costs:
# 1. Pivot the hospitalisation counts
# 2. Pivot the hospitalisation costs
# 3. Filter the !is.na() rows
# 4. Collapse the rows by visits and costs
# 5. use the summary counts and costs to sstimate the unit costs (costs/unit)

source("analysis/dm01_02_now_monthly_follow_up.R")
matched_combine <- bind_rows(com_matched, lc_exp_matched)


# Hospitalisation unit cost: ------

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


# OPA unit cost:
# cost vectors
opa_cost <- c()
for (i in 1:12){
      opa_cost <- c(opa_cost, paste0("opd_cost_m", i))
}
matched_combine$total_opa_cost <- rowSums(matched_combine[,opa_cost], na.rm = T)

# visit vectors
opa_visit <- c()
for (i in 1:12) {
      opa_visit <- c(opa_visit, paste0("opa_visit_m", i))
}
matched_combine$total_opa_visit <- rowSums(matched_combine[,opa_visit], na.rm = T)


# Keep people with both records, and then calculate the average cost for inputation
unit_opa_cost <- matched_combine %>% 
      filter(total_opa_visit>0 & total_opa_cost>0) %>% 
      summarise(unit_cost = sum(total_opa_cost, na.rm = T)/sum(total_opa_visit, na.rm = T))


# A&E visit unit cost: ----
# A&E visit costs
ane_cost <- c()
for (i in 1:12){
      ane_cost <- c(ane_cost, paste0("er_cost_m", i))
}
# ane visit cols
ane_visit <- c()
for (i in 1:12){
      ane_visit <- c(ane_visit, paste0("ae_visit_m", i))      
}

matched_combine$total_ane_costs <- rowSums(matched_combine[, ane_cost], na.rm = T)
matched_combine$total_ane_visits <- rowSums(matched_combine[, ane_visit], na.rm = T)

unit_ane_cost <- matched_combine %>% 
      filter(total_ane_visits>0 & total_ane_costs>0) %>% 
      summarise(unit_cost = sum(total_ane_costs, na.rm = T)/sum(total_ane_visits, na.rm = T))