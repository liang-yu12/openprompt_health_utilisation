source("analysis/new_dm04_01_new_costs_data.R")

# Data management of the cost data: combing costs
# 1. GP costs + GP prescription costs
# 2. Combining monthly costs
# 3. Pivot to long dataset by month

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


# 2. Combine secondary care -----
# Secondary care: apc_cost_m, er_cost_m, opd_cost_m
s_1 <- lc_exp_matched[grepl("_cost_m1$", names(lc_exp_matched))] %>% 
      names %>% as.vector()

s_2 <- lc_exp_matched[grepl("_cost_m2$", names(lc_exp_matched))] %>% 
      names %>% as.vector()

s_3 <- lc_exp_matched[grepl("_cost_m3$", names(lc_exp_matched))] %>% 
      names %>% as.vector()

s_4 <- lc_exp_matched[grepl("_cost_m4$", names(lc_exp_matched))] %>% 
      names %>% as.vector()

s_5 <- lc_exp_matched[grepl("_cost_m5$", names(lc_exp_matched))] %>% 
      names %>% as.vector()

s_6 <- lc_exp_matched[grepl("_cost_m6$", names(lc_exp_matched))] %>% 
      names %>% as.vector()

s_7 <- lc_exp_matched[grepl("_cost_m7$", names(lc_exp_matched))] %>% 
      names %>% as.vector()

s_8 <- lc_exp_matched[grepl("_cost_m8$", names(lc_exp_matched))] %>% 
      names %>% as.vector()

s_9 <- lc_exp_matched[grepl("_cost_m9$", names(lc_exp_matched))] %>% 
      names %>% as.vector()

s_10 <- lc_exp_matched[grepl("_cost_m10$", names(lc_exp_matched))] %>% 
      names %>% as.vector()

s_11 <- lc_exp_matched[grepl("_cost_m11$", names(lc_exp_matched))] %>% 
      names %>% as.vector()

s_12 <- lc_exp_matched[grepl("_cost_m12$", names(lc_exp_matched))] %>% 
      names %>% as.vector()


# Then calculate the rowSums
# Exposure group:
lc_exp_matched$secondary_cost_1 <- rowSums(lc_exp_matched[s_1], na.rm = TRUE)
lc_exp_matched$secondary_cost_2 <- rowSums(lc_exp_matched[s_2], na.rm = TRUE)
lc_exp_matched$secondary_cost_3 <- rowSums(lc_exp_matched[s_3], na.rm = TRUE)
lc_exp_matched$secondary_cost_4 <- rowSums(lc_exp_matched[s_4], na.rm = TRUE)
lc_exp_matched$secondary_cost_5 <- rowSums(lc_exp_matched[s_5], na.rm = TRUE)
lc_exp_matched$secondary_cost_6 <- rowSums(lc_exp_matched[s_6], na.rm = TRUE)
lc_exp_matched$secondary_cost_7 <- rowSums(lc_exp_matched[s_7], na.rm = TRUE)
lc_exp_matched$secondary_cost_8 <- rowSums(lc_exp_matched[s_8], na.rm = TRUE)
lc_exp_matched$secondary_cost_9 <- rowSums(lc_exp_matched[s_9], na.rm = TRUE)
lc_exp_matched$secondary_cost_10 <- rowSums(lc_exp_matched[s_10], na.rm = TRUE)
lc_exp_matched$secondary_cost_11 <- rowSums(lc_exp_matched[s_11], na.rm = TRUE)
lc_exp_matched$secondary_cost_12 <- rowSums(lc_exp_matched[s_12], na.rm = TRUE)

# comparator group:
com_matched$secondary_cost_1 <- rowSums(com_matched[s_1], na.rm = TRUE)
com_matched$secondary_cost_2 <- rowSums(com_matched[s_2], na.rm = TRUE)
com_matched$secondary_cost_3 <- rowSums(com_matched[s_3], na.rm = TRUE)
com_matched$secondary_cost_4 <- rowSums(com_matched[s_4], na.rm = TRUE)
com_matched$secondary_cost_5 <- rowSums(com_matched[s_5], na.rm = TRUE)
com_matched$secondary_cost_6 <- rowSums(com_matched[s_6], na.rm = TRUE)
com_matched$secondary_cost_7 <- rowSums(com_matched[s_7], na.rm = TRUE)
com_matched$secondary_cost_8 <- rowSums(com_matched[s_8], na.rm = TRUE)
com_matched$secondary_cost_9 <- rowSums(com_matched[s_9], na.rm = TRUE)
com_matched$secondary_cost_10 <- rowSums(com_matched[s_10], na.rm = TRUE)
com_matched$secondary_cost_11 <- rowSums(com_matched[s_11], na.rm = TRUE)
com_matched$secondary_cost_12 <- rowSums(com_matched[s_12], na.rm = TRUE)


# 3. Combine total care costs: ----
# primary + secondary

# Exposure group
lc_exp_matched$total_cost_1 <- rowSums(lc_exp_matched[,c("primary_cost_1", "secondary_cost_1")])
lc_exp_matched$total_cost_2 <- rowSums(lc_exp_matched[,c("primary_cost_2", "secondary_cost_2")])
lc_exp_matched$total_cost_3 <- rowSums(lc_exp_matched[,c("primary_cost_3", "secondary_cost_3")])
lc_exp_matched$total_cost_4 <- rowSums(lc_exp_matched[, c("primary_cost_4", "secondary_cost_4")])
lc_exp_matched$total_cost_5 <- rowSums(lc_exp_matched[, c("primary_cost_5", "secondary_cost_5")])
lc_exp_matched$total_cost_6 <- rowSums(lc_exp_matched[, c("primary_cost_6", "secondary_cost_6")])
lc_exp_matched$total_cost_7 <- rowSums(lc_exp_matched[, c("primary_cost_7", "secondary_cost_7")])
lc_exp_matched$total_cost_8 <- rowSums(lc_exp_matched[, c("primary_cost_8", "secondary_cost_8")])
lc_exp_matched$total_cost_9 <- rowSums(lc_exp_matched[, c("primary_cost_9", "secondary_cost_9")])
lc_exp_matched$total_cost_10 <- rowSums(lc_exp_matched[, c("primary_cost_10", "secondary_cost_10")])
lc_exp_matched$total_cost_11 <- rowSums(lc_exp_matched[, c("primary_cost_11", "secondary_cost_11")])
lc_exp_matched$total_cost_12 <- rowSums(lc_exp_matched[, c("primary_cost_12", "secondary_cost_12")])

# Comparator group:
com_matched$total_cost_1 <- rowSums(com_matched[,c("primary_cost_1", "secondary_cost_1")])
com_matched$total_cost_2 <- rowSums(com_matched[,c("primary_cost_2", "secondary_cost_2")])
com_matched$total_cost_3 <- rowSums(com_matched[,c("primary_cost_3", "secondary_cost_3")])
com_matched$total_cost_4 <- rowSums(com_matched[, c("primary_cost_4", "secondary_cost_4")])
com_matched$total_cost_5 <- rowSums(com_matched[, c("primary_cost_5", "secondary_cost_5")])
com_matched$total_cost_6 <- rowSums(com_matched[, c("primary_cost_6", "secondary_cost_6")])
com_matched$total_cost_7 <- rowSums(com_matched[, c("primary_cost_7", "secondary_cost_7")])
com_matched$total_cost_8 <- rowSums(com_matched[, c("primary_cost_8", "secondary_cost_8")])
com_matched$total_cost_9 <- rowSums(com_matched[, c("primary_cost_9", "secondary_cost_9")])
com_matched$total_cost_10 <- rowSums(com_matched[, c("primary_cost_10", "secondary_cost_10")])
com_matched$total_cost_11 <- rowSums(com_matched[, c("primary_cost_11", "secondary_cost_11")])
com_matched$total_cost_12 <- rowSums(com_matched[, c("primary_cost_12", "secondary_cost_12")])
