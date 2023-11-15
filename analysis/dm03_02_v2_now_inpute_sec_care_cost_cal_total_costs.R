source("analysis/dm03_01_v2_now_read_cal_gp_cost_and_unit_costs.R")

# Data management of the cost data: combing costs
# 1. GP costs + GP prescription costs
# 2. Combining monthly costs
# 3. Combine the total cost in 12 months

# 1. GP costs: combine monthy GP visit cost + GP prescription ----
# Combine the cost vectors in the same month period in a vector
total_primary_cost <- c(gp_cost, gi_cost, cv_cost, chest_cost, cns_cost,
                        inf_cost,meta_cost, gyn_cost,cancer_cost, diet_cost,
                        muscle_cost, eye_cost, ent_cost, skin_cost)

# then regroup them by month
p_1 <- total_primary_cost[grepl("_cost_1$", total_primary_cost)]
p_2 <- total_primary_cost[grepl("_cost_2$", total_primary_cost)]
p_3 <- total_primary_cost[grepl("_cost_3$", total_primary_cost)]
p_4 <- total_primary_cost[grepl("_cost_4$", total_primary_cost)]
p_5 <- total_primary_cost[grepl("_cost_5$", total_primary_cost)]
p_6 <- total_primary_cost[grepl("_cost_6$", total_primary_cost)]
p_7 <- total_primary_cost[grepl("_cost_7$", total_primary_cost)]
p_8 <- total_primary_cost[grepl("_cost_8$", total_primary_cost)]
p_9 <- total_primary_cost[grepl("_cost_9$", total_primary_cost)]
p_10 <- total_primary_cost[grepl("_cost_10$", total_primary_cost)]
p_11 <- total_primary_cost[grepl("_cost_11$", total_primary_cost)]
p_12 <- total_primary_cost[grepl("_cost_12$", total_primary_cost)]

# Then calculate the rowSums

# Exposure group:
lc_exp_matched$primary_cost_1 <- rowSums(lc_exp_matched[p_1], na.rm = T)
lc_exp_matched$primary_cost_2 <- rowSums(lc_exp_matched[p_2], na.rm = T)
lc_exp_matched$primary_cost_3 <- rowSums(lc_exp_matched[p_3], na.rm = T)
lc_exp_matched$primary_cost_4 <- rowSums(lc_exp_matched[p_4], na.rm = T)
lc_exp_matched$primary_cost_5 <- rowSums(lc_exp_matched[p_5], na.rm = T)
lc_exp_matched$primary_cost_6 <- rowSums(lc_exp_matched[p_6], na.rm = T)
lc_exp_matched$primary_cost_7 <- rowSums(lc_exp_matched[p_7], na.rm = T)
lc_exp_matched$primary_cost_8 <- rowSums(lc_exp_matched[p_8], na.rm = T)
lc_exp_matched$primary_cost_9 <- rowSums(lc_exp_matched[p_9], na.rm = T)
lc_exp_matched$primary_cost_10 <- rowSums(lc_exp_matched[p_10], na.rm = T)
lc_exp_matched$primary_cost_11 <- rowSums(lc_exp_matched[p_11], na.rm = T)
lc_exp_matched$primary_cost_12 <- rowSums(lc_exp_matched[p_12], na.rm = T)

# Comparator group
com_matched$primary_cost_1 <- rowSums(com_matched[p_1], na.rm = T)
com_matched$primary_cost_2 <- rowSums(com_matched[p_2], na.rm = T)
com_matched$primary_cost_3 <- rowSums(com_matched[p_3], na.rm = T)
com_matched$primary_cost_4 <- rowSums(com_matched[p_4], na.rm = T)
com_matched$primary_cost_5 <- rowSums(com_matched[p_5], na.rm = T)
com_matched$primary_cost_6 <- rowSums(com_matched[p_6], na.rm = T)
com_matched$primary_cost_7 <- rowSums(com_matched[p_7], na.rm = T)
com_matched$primary_cost_8 <- rowSums(com_matched[p_8], na.rm = T)
com_matched$primary_cost_9 <- rowSums(com_matched[p_9], na.rm = T)
com_matched$primary_cost_10 <- rowSums(com_matched[p_10], na.rm = T)
com_matched$primary_cost_11 <- rowSums(com_matched[p_11], na.rm = T)
com_matched$primary_cost_12 <- rowSums(com_matched[p_12], na.rm = T)


parimary_cost <- c()
for (i in 1:12){
      parimary_cost <- c(parimary_cost, paste0("primary_cost_", i))
}

# Add them together
lc_exp_matched$total_primary_cost <- rowSums(lc_exp_matched[,parimary_cost], na.rm = T)
com_matched$total_primary_cost <- rowSums(com_matched[,parimary_cost], na.rm = T)
