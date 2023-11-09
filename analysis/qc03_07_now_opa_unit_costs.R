# Goal: calculate OPA unit cost 

source("analysis/dm01_02_now_monthly_follow_up.R")
matched_combine <- bind_rows(com_matched, lc_exp_matched)

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


# save outputs 
unit_opa_cost %>% write_csv(here("output", "qc03_07_opa_unit_costs.csv"))
