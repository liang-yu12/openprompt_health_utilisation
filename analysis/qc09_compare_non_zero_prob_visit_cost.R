source("analysis/dm04_02_combine_costs.R")

lc_exp_matched <- lc_exp_matched %>%
      mutate(
            bi_visit_m1 = ifelse(all_month_m1>0, 1, 0),
            bi_visit_m2 = ifelse(all_month_m2>0, 1, 0),
            bi_visit_m3 = ifelse(all_month_m3>0, 1, 0),
            bi_visit_m4 = ifelse(all_month_m4>0, 1, 0),
            bi_visit_m5 = ifelse(all_month_m5>0, 1, 0),
            bi_visit_m6 = ifelse(all_month_m6>0, 1, 0),
            bi_visit_m7 = ifelse(all_month_m7>0, 1, 0),
            bi_visit_m8 = ifelse(all_month_m8>0, 1, 0),
            bi_visit_m9 = ifelse(all_month_m9>0, 1, 0),
            bi_visit_m10 = ifelse(all_month_m10>0, 1, 0),
            bi_visit_m11 = ifelse(all_month_m11>0, 1, 0),
            bi_visit_m12 = ifelse(all_month_m12>0, 1, 0)
      )

lc_exp_matched <- lc_exp_matched %>% 
      mutate(
            bi_cost_m1 = ifelse(total_cost_1>0, 1, 0),
            bi_cost_m2 = ifelse(total_cost_2>0, 1, 0),
            bi_cost_m3 = ifelse(total_cost_3>0, 1, 0),
            bi_cost_m4 = ifelse(total_cost_4>0, 1, 0),
            bi_cost_m5 = ifelse(total_cost_5>0, 1, 0),
            bi_cost_m6 = ifelse(total_cost_6>0, 1, 0),
            bi_cost_m7 = ifelse(total_cost_7>0, 1, 0),
            bi_cost_m8 = ifelse(total_cost_8>0, 1, 0),
            bi_cost_m9 = ifelse(total_cost_9>0, 1, 0),
            bi_cost_m10 = ifelse(total_cost_10>0, 1, 0),
            bi_cost_m11 = ifelse(total_cost_11>0, 1, 0),
            bi_cost_m12 = ifelse(total_cost_12>0, 1, 0)
      )

total_m1 <- lc_exp_matched %>% group_by(bi_visit_m1) %>% 
      summarise(have_cost = sum(bi_cost_m1, na.rm = TRUE)) %>% 
      mutate(month = 1, 
             visit = ifelse(bi_visit_m1==1, "Yes", "No")) %>% 
      dplyr::select(month, visit, have_cost)

total_m2 <- lc_exp_matched %>% group_by(bi_visit_m2) %>% 
      summarise(have_cost = sum(bi_cost_m2, na.rm = TRUE)) %>% 
      mutate(month = 2, 
             visit = ifelse(bi_visit_m2==1, "Yes", "No")) %>% 
      dplyr::select(month, visit, have_cost)

total_m3 <- lc_exp_matched %>% group_by(bi_visit_m3) %>% 
      summarise(have_cost = sum(bi_cost_m3, na.rm = TRUE)) %>% 
      mutate(month = 3, 
             visit = ifelse(bi_visit_m3==1, "Yes", "No")) %>% 
      dplyr::select(month, visit, have_cost)

total_m4 <- lc_exp_matched %>% group_by(bi_visit_m4) %>% 
      summarise(have_cost = sum(bi_cost_m4, na.rm = TRUE)) %>% 
      mutate(month = 4, 
             visit = ifelse(bi_visit_m4==1, "Yes", "No")) %>% 
      dplyr::select(month, visit, have_cost)

total_m5 <- lc_exp_matched %>% group_by(bi_visit_m5) %>% 
      summarise(have_cost = sum(bi_cost_m5, na.rm = TRUE)) %>% 
      mutate(month = 5, 
             visit = ifelse(bi_visit_m5==1, "Yes", "No")) %>% 
      dplyr::select(month, visit, have_cost)

total_m6 <- lc_exp_matched %>% group_by(bi_visit_m6) %>% 
      summarise(have_cost = sum(bi_cost_m6, na.rm = TRUE)) %>% 
      mutate(month = 6, 
             visit = ifelse(bi_visit_m6==1, "Yes", "No")) %>% 
      dplyr::select(month, visit, have_cost)

total_m7 <- lc_exp_matched %>% group_by(bi_visit_m7) %>% 
      summarise(have_cost = sum(bi_cost_m7, na.rm = TRUE)) %>% 
      mutate(month = 7, 
             visit = ifelse(bi_visit_m7==1, "Yes", "No")) %>% 
      dplyr::select(month, visit, have_cost)

total_m8 <- lc_exp_matched %>% group_by(bi_visit_m8) %>% 
      summarise(have_cost = sum(bi_cost_m8, na.rm = TRUE)) %>% 
      mutate(month = 8, 
             visit = ifelse(bi_visit_m8==1, "Yes", "No")) %>% 
      dplyr::select(month, visit, have_cost)

total_m9 <- lc_exp_matched %>% group_by(bi_visit_m9) %>% 
      summarise(have_cost = sum(bi_cost_m9, na.rm = TRUE)) %>% 
      mutate(month = 9, 
             visit = ifelse(bi_visit_m9==1, "Yes", "No")) %>% 
      dplyr::select(month, visit, have_cost)

total_m10 <- lc_exp_matched %>% group_by(bi_visit_m10) %>% 
      summarise(have_cost = sum(bi_cost_m10, na.rm = TRUE)) %>% 
      mutate(month = 10, 
             visit = ifelse(bi_visit_m10==1, "Yes", "No")) %>% 
      dplyr::select(month, visit, have_cost)

total_m11 <- lc_exp_matched %>% group_by(bi_visit_m11) %>% 
      summarise(have_cost = sum(bi_cost_m11, na.rm = TRUE)) %>% 
      mutate(month = 11, 
             visit = ifelse(bi_visit_m11==1, "Yes", "No")) %>% 
      dplyr::select(month, visit, have_cost)

total_m12 <- lc_exp_matched %>% group_by(bi_visit_m12) %>% 
      summarise(have_cost = sum(bi_cost_m12, na.rm = TRUE)) %>% 
      mutate(month = 12, 
             visit = ifelse(bi_visit_m12==1, "Yes", "No")) %>% 
      dplyr::select(month, visit, have_cost)


total_compare <- bind_rows(total_m1, total_m2, total_m3,total_m4, total_m5, total_m6,
                           total_m7, total_m8, total_m8, total_m9, total_m10, total_m11, total_m12)

