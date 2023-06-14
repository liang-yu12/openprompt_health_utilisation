# Load previous data management
source("analysis/dm03_matched_define_monthly_follow_up_time.R")

# For organising the outputs
options(digits=2)

# A. Crude hurdle model----
cumulative_visit_crude_fn <- function(visit, fu_time, data, month) {
      
      # Create a hurdle model
      model <- hurdle(
            visit ~ exposure + offset(fu_time) | age_cat + sex+ region,
            data = data,
            zero.dist = "binomial",
            dist = "poisson"
      )
      
      # because original dataset has NA, need to first exclude them first
      # then add the predicted value back to the table. 
      no_na <- filter(matched_data, !is.na(fu_time))
      no_na$predict <- predict(model, na.action = na.exclude)
      results <- no_na %>% # summarise the mean visit and sd.
            group_by(exposure) %>% 
            summarise(mean = mean(predict),
                      sd = sd(predict)) %>% 
            mutate(month = month) %>% relocate(month)
      
      return(results)
}

results_crude <- bind_rows(
      cumulative_visit_crude_fn(visit = matched_data$all_month1,
                                fu_time = matched_data$follow_up_m1,
                                data = matched_data,
                                month = 1),
      cumulative_visit_crude_fn(visit = matched_data$all_month2,
                                fu_time = matched_data$follow_up_m2,
                                data = matched_data,
                                month = 2),
      cumulative_visit_crude_fn(visit = matched_data$all_month3,
                                fu_time = matched_data$follow_up_m3,
                                data = matched_data,
                                month = 3),
      cumulative_visit_crude_fn(visit = matched_data$all_month4,
                                fu_time = matched_data$follow_up_m4,
                                data = matched_data,
                                month = 4),
      cumulative_visit_crude_fn(visit = matched_data$all_month5,
                                fu_time = matched_data$follow_up_m5,
                                data = matched_data,
                                month = 5),
      cumulative_visit_crude_fn(visit = matched_data$all_month6,
                                fu_time = matched_data$follow_up_m6,
                                data = matched_data,
                                month = 6),
      cumulative_visit_crude_fn(visit = matched_data$all_month7,
                                fu_time = matched_data$follow_up_m7,
                                data = matched_data,
                                month = 7),
      cumulative_visit_crude_fn(visit = matched_data$all_month8,
                                fu_time = matched_data$follow_up_m8,
                                data = matched_data,
                                month = 8),
      cumulative_visit_crude_fn(visit = matched_data$all_month9,
                                fu_time = matched_data$follow_up_m9,
                                data = matched_data,
                                month = 9),
      cumulative_visit_crude_fn(visit = matched_data$all_month10,
                                fu_time = matched_data$follow_up_m10,
                                data = matched_data,
                                month = 10),
      cumulative_visit_crude_fn(visit = matched_data$all_month11,
                                fu_time = matched_data$follow_up_m11,
                                data = matched_data,
                                month = 11),
      cumulative_visit_crude_fn(visit = matched_data$all_month12,
                                fu_time = matched_data$follow_up_m12,
                                data = matched_data,
                                month = 12)) %>% 
      mutate(model = "Crude") %>% 
      relocate(model)

# B. Partially adjusted model ------
cumulative_visit_partially_adj_fn <- function(visit, fu_time, data, month) {
      
      # Create a hurdle model
      model <- hurdle(
            visit ~ exposure + sex + age_cat + offset(fu_time) | age_cat + sex+ region,
            data = data,
            zero.dist = "binomial",
            dist = "poisson"
      )
      
      # because original dataset has NA, need to first exclude them first
      # then add the predicted value back to the table. 
      no_na <- filter(matched_data, !is.na(fu_time))
      cov <- c("exposure","sex","age_cat")
      no_na <- no_na[complete.cases(no_na[, cov]), ]
      
      no_na$predict <- predict(model, na.action = na.exclude)
      results <- no_na %>% # summarise the mean visit and sd.
            group_by(exposure) %>% 
            summarise(mean = mean(predict),
                      sd = sd(predict)) %>% 
            mutate(month = month) %>% relocate(month)
      
      return(results)
}

results_part_adjusted <- bind_rows(
      cumulative_visit_partially_adj_fn(visit = matched_data$all_month1,
                                fu_time = matched_data$follow_up_m1,
                                data = matched_data,
                                month = 1),
      cumulative_visit_partially_adj_fn(visit = matched_data$all_month2,
                                fu_time = matched_data$follow_up_m2,
                                data = matched_data,
                                month = 2),
      cumulative_visit_partially_adj_fn(visit = matched_data$all_month3,
                                fu_time = matched_data$follow_up_m3,
                                data = matched_data,
                                month = 3),
      cumulative_visit_partially_adj_fn(visit = matched_data$all_month4,
                                fu_time = matched_data$follow_up_m4,
                                data = matched_data,
                                month = 4),
      cumulative_visit_partially_adj_fn(visit = matched_data$all_month5,
                                fu_time = matched_data$follow_up_m5,
                                data = matched_data,
                                month = 5),
      cumulative_visit_partially_adj_fn(visit = matched_data$all_month6,
                                fu_time = matched_data$follow_up_m6,
                                data = matched_data,
                                month = 6),
      cumulative_visit_partially_adj_fn(visit = matched_data$all_month7,
                                fu_time = matched_data$follow_up_m7,
                                data = matched_data,
                                month = 7),
      cumulative_visit_partially_adj_fn(visit = matched_data$all_month8,
                                fu_time = matched_data$follow_up_m8,
                                data = matched_data,
                                month = 8),
      cumulative_visit_partially_adj_fn(visit = matched_data$all_month9,
                                fu_time = matched_data$follow_up_m9,
                                data = matched_data,
                                month = 9),
      cumulative_visit_partially_adj_fn(visit = matched_data$all_month10,
                                fu_time = matched_data$follow_up_m10,
                                data = matched_data,
                                month = 10),
      cumulative_visit_partially_adj_fn(visit = matched_data$all_month11,
                                fu_time = matched_data$follow_up_m11,
                                data = matched_data,
                                month = 11),
      cumulative_visit_partially_adj_fn(visit = matched_data$all_month12,
                                fu_time = matched_data$follow_up_m12,
                                data = matched_data,
                                month = 12)) %>% 
      mutate(model = "Partially adjusted") %>% 
      relocate(model)



# C. Fully adjusted model  ---------


cumulative_visit_fully_adj_fn <- function(visit, fu_time, data, month) {
      
      # Create a hurdle model
      model <- hurdle(
            visit ~ exposure + sex + age_cat + cov_covid_vax_n_cat + bmi_cat + imd_q5 + ethnicity_6 + region + 
                  number_comorbidities_cat + offset(fu_time) | age_cat + sex+ region,
            data = data,
            zero.dist = "binomial",
            dist = "poisson"
      )
      
      # because original dataset has NA, need to first exclude them first
      # then add the predicted value back to the table. 
      no_na <- filter(matched_data, !is.na(fu_time))
      cov <- c("exposure","sex","age_cat", "cov_covid_vax_n_cat", "bmi_cat", "imd_q5", "ethnicity_6", "region",
            "number_comorbidities_cat")
      no_na <- no_na[complete.cases(no_na[, cov]), ]
      
      no_na$predict <- predict(model, na.action = na.exclude)
      results <- no_na %>% # summarise the mean visit and sd.
            group_by(exposure) %>% 
            summarise(mean = mean(predict),
                      sd = sd(predict)) %>% 
            mutate(month = month) %>% relocate(month)
      
      return(results)
}

results_fully_adjusted <- bind_rows(
      cumulative_visit_fully_adj_fn(visit = matched_data$all_month1,
                                        fu_time = matched_data$follow_up_m1,
                                        data = matched_data,
                                        month = 1),
      cumulative_visit_fully_adj_fn(visit = matched_data$all_month2,
                                        fu_time = matched_data$follow_up_m2,
                                        data = matched_data,
                                        month = 2),
      cumulative_visit_fully_adj_fn(visit = matched_data$all_month3,
                                        fu_time = matched_data$follow_up_m3,
                                        data = matched_data,
                                        month = 3),
      cumulative_visit_fully_adj_fn(visit = matched_data$all_month4,
                                        fu_time = matched_data$follow_up_m4,
                                        data = matched_data,
                                        month = 4),
      cumulative_visit_fully_adj_fn(visit = matched_data$all_month5,
                                        fu_time = matched_data$follow_up_m5,
                                        data = matched_data,
                                        month = 5),
      cumulative_visit_fully_adj_fn(visit = matched_data$all_month6,
                                        fu_time = matched_data$follow_up_m6,
                                        data = matched_data,
                                        month = 6),
      cumulative_visit_fully_adj_fn(visit = matched_data$all_month7,
                                        fu_time = matched_data$follow_up_m7,
                                        data = matched_data,
                                        month = 7),
      cumulative_visit_fully_adj_fn(visit = matched_data$all_month8,
                                        fu_time = matched_data$follow_up_m8,
                                        data = matched_data,
                                        month = 8),
      cumulative_visit_fully_adj_fn(visit = matched_data$all_month9,
                                        fu_time = matched_data$follow_up_m9,
                                        data = matched_data,
                                        month = 9),
      cumulative_visit_fully_adj_fn(visit = matched_data$all_month10,
                                        fu_time = matched_data$follow_up_m10,
                                        data = matched_data,
                                        month = 10),
      cumulative_visit_fully_adj_fn(visit = matched_data$all_month11,
                                        fu_time = matched_data$follow_up_m11,
                                        data = matched_data,
                                        month = 11),
      cumulative_visit_fully_adj_fn(visit = matched_data$all_month12,
                                        fu_time = matched_data$follow_up_m12,
                                        data = matched_data,
                                        month = 12)) %>% 
      mutate(model = "Fully adjusted") %>% 
      relocate(model)


# Final output: ------
results_hurdle <- bind_rows(results_crude, results_part_adjusted, results_fully_adjusted)
results_hurdle %>% write.csv(here("output", "st_03_result_cumulative_visit_hurdle.csv"), row.names = F)
# 
# 
# # D. Cumulative GP visit: ----
# # Crude
# gp_crude <- bind_rows(
#       cumulative_visit_crude_fn(visit = matched_data$gp_ac_visit_m1,
#                                 fu_time = matched_data$follow_up_m1,
#                                 data = matched_data,
#                                 month = 1),
#       cumulative_visit_crude_fn(visit = matched_data$gp_ac_visit_m2,
#                                 fu_time = matched_data$follow_up_m2,
#                                 data = matched_data,
#                                 month = 2),
#       cumulative_visit_crude_fn(visit = matched_data$gp_ac_visit_m3,
#                                 fu_time = matched_data$follow_up_m3,
#                                 data = matched_data,
#                                 month = 3),
#       cumulative_visit_crude_fn(visit = matched_data$gp_ac_visit_m4,
#                                 fu_time = matched_data$follow_up_m4,
#                                 data = matched_data,
#                                 month = 4),
#       cumulative_visit_crude_fn(visit = matched_data$gp_ac_visit_m5,
#                                 fu_time = matched_data$follow_up_m5,
#                                 data = matched_data,
#                                 month = 5),
#       cumulative_visit_crude_fn(visit = matched_data$gp_ac_visit_m6,
#                                 fu_time = matched_data$follow_up_m6,
#                                 data = matched_data,
#                                 month = 6),
#       cumulative_visit_crude_fn(visit = matched_data$gp_ac_visit_m7,
#                                 fu_time = matched_data$follow_up_m7,
#                                 data = matched_data,
#                                 month = 7),
#       cumulative_visit_crude_fn(visit = matched_data$gp_ac_visit_m8,
#                                 fu_time = matched_data$follow_up_m8,
#                                 data = matched_data,
#                                 month = 8),
#       cumulative_visit_crude_fn(visit = matched_data$gp_ac_visit_m9,
#                                 fu_time = matched_data$follow_up_m9,
#                                 data = matched_data,
#                                 month = 9),
#       cumulative_visit_crude_fn(visit = matched_data$gp_ac_visit_m10,
#                                 fu_time = matched_data$follow_up_m10,
#                                 data = matched_data,
#                                 month = 10),
#       cumulative_visit_crude_fn(visit = matched_data$gp_ac_visit_m11,
#                                 fu_time = matched_data$follow_up_m11,
#                                 data = matched_data,
#                                 month = 11),
#       cumulative_visit_crude_fn(visit = matched_data$gp_ac_visit_m12,
#                                 fu_time = matched_data$follow_up_m12,
#                                 data = matched_data,
#                                 month = 12)) %>% 
#       mutate(model = "Crude") %>% 
#       relocate(model)
# 
# # Partially adjusted 
# 
# gp_part_adjusted <- bind_rows(
#       cumulative_visit_partially_adj_fn(visit = matched_data$gp_ac_visit_m1,
#                                         fu_time = matched_data$follow_up_m1,
#                                         data = matched_data,
#                                         month = 1),
#       cumulative_visit_partially_adj_fn(visit = matched_data$gp_ac_visit_m2,
#                                         fu_time = matched_data$follow_up_m2,
#                                         data = matched_data,
#                                         month = 2),
#       cumulative_visit_partially_adj_fn(visit = matched_data$gp_ac_visit_m3,
#                                         fu_time = matched_data$follow_up_m3,
#                                         data = matched_data,
#                                         month = 3),
#       cumulative_visit_partially_adj_fn(visit = matched_data$gp_ac_visit_m4,
#                                         fu_time = matched_data$follow_up_m4,
#                                         data = matched_data,
#                                         month = 4),
#       cumulative_visit_partially_adj_fn(visit = matched_data$gp_ac_visit_m5,
#                                         fu_time = matched_data$follow_up_m5,
#                                         data = matched_data,
#                                         month = 5),
#       cumulative_visit_partially_adj_fn(visit = matched_data$gp_ac_visit_m6,
#                                         fu_time = matched_data$follow_up_m6,
#                                         data = matched_data,
#                                         month = 6),
#       cumulative_visit_partially_adj_fn(visit = matched_data$gp_ac_visit_m7,
#                                         fu_time = matched_data$follow_up_m7,
#                                         data = matched_data,
#                                         month = 7),
#       cumulative_visit_partially_adj_fn(visit = matched_data$gp_ac_visit_m8,
#                                         fu_time = matched_data$follow_up_m8,
#                                         data = matched_data,
#                                         month = 8),
#       cumulative_visit_partially_adj_fn(visit = matched_data$gp_ac_visit_m9,
#                                         fu_time = matched_data$follow_up_m9,
#                                         data = matched_data,
#                                         month = 9),
#       cumulative_visit_partially_adj_fn(visit = matched_data$gp_ac_visit_m10,
#                                         fu_time = matched_data$follow_up_m10,
#                                         data = matched_data,
#                                         month = 10),
#       cumulative_visit_partially_adj_fn(visit = matched_data$gp_ac_visit_m11,
#                                         fu_time = matched_data$follow_up_m11,
#                                         data = matched_data,
#                                         month = 11),
#       cumulative_visit_partially_adj_fn(visit = matched_data$gp_ac_visit_m12,
#                                         fu_time = matched_data$follow_up_m12,
#                                         data = matched_data,
#                                         month = 12)) %>% 
#       mutate(model = "Partially adjusted") %>% 
#       relocate(model)
# 
# 
# 
# # Fully adjusted
# gp_fully_adjusted <- bind_rows(
#       cumulative_visit_fully_adj_fn(visit = matched_data$gp_ac_visit_m1,
#                                     fu_time = matched_data$follow_up_m1,
#                                     data = matched_data,
#                                     month = 1),
#       cumulative_visit_fully_adj_fn(visit = matched_data$gp_ac_visit_m2,
#                                     fu_time = matched_data$follow_up_m2,
#                                     data = matched_data,
#                                     month = 2),
#       cumulative_visit_fully_adj_fn(visit = matched_data$gp_ac_visit_m3,
#                                     fu_time = matched_data$follow_up_m3,
#                                     data = matched_data,
#                                     month = 3),
#       cumulative_visit_fully_adj_fn(visit = matched_data$gp_ac_visit_m4,
#                                     fu_time = matched_data$follow_up_m4,
#                                     data = matched_data,
#                                     month = 4),
#       cumulative_visit_fully_adj_fn(visit = matched_data$gp_ac_visit_m5,
#                                     fu_time = matched_data$follow_up_m5,
#                                     data = matched_data,
#                                     month = 5),
#       cumulative_visit_fully_adj_fn(visit = matched_data$gp_ac_visit_m6,
#                                     fu_time = matched_data$follow_up_m6,
#                                     data = matched_data,
#                                     month = 6),
#       cumulative_visit_fully_adj_fn(visit = matched_data$gp_ac_visit_m7,
#                                     fu_time = matched_data$follow_up_m7,
#                                     data = matched_data,
#                                     month = 7),
#       cumulative_visit_fully_adj_fn(visit = matched_data$gp_ac_visit_m8,
#                                     fu_time = matched_data$follow_up_m8,
#                                     data = matched_data,
#                                     month = 8),
#       cumulative_visit_fully_adj_fn(visit = matched_data$gp_ac_visit_m9,
#                                     fu_time = matched_data$follow_up_m9,
#                                     data = matched_data,
#                                     month = 9),
#       cumulative_visit_fully_adj_fn(visit = matched_data$gp_ac_visit_m10,
#                                     fu_time = matched_data$follow_up_m10,
#                                     data = matched_data,
#                                     month = 10),
#       cumulative_visit_fully_adj_fn(visit = matched_data$gp_ac_visit_m11,
#                                     fu_time = matched_data$follow_up_m11,
#                                     data = matched_data,
#                                     month = 11),
#       cumulative_visit_fully_adj_fn(visit = matched_data$gp_ac_visit_m12,
#                                     fu_time = matched_data$follow_up_m12,
#                                     data = matched_data,
#                                     month = 12)) %>% 
#       mutate(model = "Fully adjusted") %>% 
#       relocate(model)
# 
# gp_results_hurdle <- bind_rows(gp_crude, gp_part_adjusted, gp_fully_adjusted)
# gp_results_hurdle %>% write.csv(here("output", "st_03_gp_result_cumulative_visit_hurdle.csv"), row.names = F)
# 
# 
# # E. Hospital admission: -----
# # Crude
# hos_crude <- bind_rows(
#       cumulative_visit_crude_fn(visit = matched_data$hos_ac_visit_m1,
#                                 fu_time = matched_data$follow_up_m1,
#                                 data = matched_data,
#                                 month = 1),
#       cumulative_visit_crude_fn(visit = matched_data$hos_ac_visit_m2,
#                                 fu_time = matched_data$follow_up_m2,
#                                 data = matched_data,
#                                 month = 2),
#       cumulative_visit_crude_fn(visit = matched_data$hos_ac_visit_m3,
#                                 fu_time = matched_data$follow_up_m3,
#                                 data = matched_data,
#                                 month = 3),
#       cumulative_visit_crude_fn(visit = matched_data$hos_ac_visit_m4,
#                                 fu_time = matched_data$follow_up_m4,
#                                 data = matched_data,
#                                 month = 4),
#       cumulative_visit_crude_fn(visit = matched_data$hos_ac_visit_m5,
#                                 fu_time = matched_data$follow_up_m5,
#                                 data = matched_data,
#                                 month = 5),
#       cumulative_visit_crude_fn(visit = matched_data$hos_ac_visit_m6,
#                                 fu_time = matched_data$follow_up_m6,
#                                 data = matched_data,
#                                 month = 6),
#       cumulative_visit_crude_fn(visit = matched_data$hos_ac_visit_m7,
#                                 fu_time = matched_data$follow_up_m7,
#                                 data = matched_data,
#                                 month = 7),
#       cumulative_visit_crude_fn(visit = matched_data$hos_ac_visit_m8,
#                                 fu_time = matched_data$follow_up_m8,
#                                 data = matched_data,
#                                 month = 8),
#       cumulative_visit_crude_fn(visit = matched_data$hos_ac_visit_m9,
#                                 fu_time = matched_data$follow_up_m9,
#                                 data = matched_data,
#                                 month = 9),
#       cumulative_visit_crude_fn(visit = matched_data$hos_ac_visit_m10,
#                                 fu_time = matched_data$follow_up_m10,
#                                 data = matched_data,
#                                 month = 10),
#       cumulative_visit_crude_fn(visit = matched_data$hos_ac_visit_m11,
#                                 fu_time = matched_data$follow_up_m11,
#                                 data = matched_data,
#                                 month = 11),
#       cumulative_visit_crude_fn(visit = matched_data$hos_ac_visit_m12,
#                                 fu_time = matched_data$follow_up_m12,
#                                 data = matched_data,
#                                 month = 12)) %>% 
#       mutate(model = "Crude") %>% 
#       relocate(model)
# 
# # Partially adjusted 
# hos_part_adjusted <- bind_rows(
#       cumulative_visit_partially_adj_fn(visit = matched_data$hos_ac_visit_m1,
#                                         fu_time = matched_data$follow_up_m1,
#                                         data = matched_data,
#                                         month = 1),
#       cumulative_visit_partially_adj_fn(visit = matched_data$hos_ac_visit_m2,
#                                         fu_time = matched_data$follow_up_m2,
#                                         data = matched_data,
#                                         month = 2),
#       cumulative_visit_partially_adj_fn(visit = matched_data$hos_ac_visit_m3,
#                                         fu_time = matched_data$follow_up_m3,
#                                         data = matched_data,
#                                         month = 3),
#       cumulative_visit_partially_adj_fn(visit = matched_data$hos_ac_visit_m4,
#                                         fu_time = matched_data$follow_up_m4,
#                                         data = matched_data,
#                                         month = 4),
#       cumulative_visit_partially_adj_fn(visit = matched_data$hos_ac_visit_m5,
#                                         fu_time = matched_data$follow_up_m5,
#                                         data = matched_data,
#                                         month = 5),
#       cumulative_visit_partially_adj_fn(visit = matched_data$hos_ac_visit_m6,
#                                         fu_time = matched_data$follow_up_m6,
#                                         data = matched_data,
#                                         month = 6),
#       cumulative_visit_partially_adj_fn(visit = matched_data$hos_ac_visit_m7,
#                                         fu_time = matched_data$follow_up_m7,
#                                         data = matched_data,
#                                         month = 7),
#       cumulative_visit_partially_adj_fn(visit = matched_data$hos_ac_visit_m8,
#                                         fu_time = matched_data$follow_up_m8,
#                                         data = matched_data,
#                                         month = 8),
#       cumulative_visit_partially_adj_fn(visit = matched_data$hos_ac_visit_m9,
#                                         fu_time = matched_data$follow_up_m9,
#                                         data = matched_data,
#                                         month = 9),
#       cumulative_visit_partially_adj_fn(visit = matched_data$hos_ac_visit_m10,
#                                         fu_time = matched_data$follow_up_m10,
#                                         data = matched_data,
#                                         month = 10),
#       cumulative_visit_partially_adj_fn(visit = matched_data$hos_ac_visit_m11,
#                                         fu_time = matched_data$follow_up_m11,
#                                         data = matched_data,
#                                         month = 11),
#       cumulative_visit_partially_adj_fn(visit = matched_data$hos_ac_visit_m12,
#                                         fu_time = matched_data$follow_up_m12,
#                                         data = matched_data,
#                                         month = 12)) %>% 
#       mutate(model = "Partially adjusted") %>% 
#       relocate(model)
# 
# # Fully adjusted
# hos_fully_adjusted <- bind_rows(
#       cumulative_visit_fully_adj_fn(visit = matched_data$hos_ac_visit_m1,
#                                     fu_time = matched_data$follow_up_m1,
#                                     data = matched_data,
#                                     month = 1),
#       cumulative_visit_fully_adj_fn(visit = matched_data$hos_ac_visit_m2,
#                                     fu_time = matched_data$follow_up_m2,
#                                     data = matched_data,
#                                     month = 2),
#       cumulative_visit_fully_adj_fn(visit = matched_data$hos_ac_visit_m3,
#                                     fu_time = matched_data$follow_up_m3,
#                                     data = matched_data,
#                                     month = 3),
#       cumulative_visit_fully_adj_fn(visit = matched_data$hos_ac_visit_m4,
#                                     fu_time = matched_data$follow_up_m4,
#                                     data = matched_data,
#                                     month = 4),
#       cumulative_visit_fully_adj_fn(visit = matched_data$hos_ac_visit_m5,
#                                     fu_time = matched_data$follow_up_m5,
#                                     data = matched_data,
#                                     month = 5),
#       cumulative_visit_fully_adj_fn(visit = matched_data$hos_ac_visit_m6,
#                                     fu_time = matched_data$follow_up_m6,
#                                     data = matched_data,
#                                     month = 6),
#       cumulative_visit_fully_adj_fn(visit = matched_data$hos_ac_visit_m7,
#                                     fu_time = matched_data$follow_up_m7,
#                                     data = matched_data,
#                                     month = 7),
#       cumulative_visit_fully_adj_fn(visit = matched_data$hos_ac_visit_m8,
#                                     fu_time = matched_data$follow_up_m8,
#                                     data = matched_data,
#                                     month = 8),
#       cumulative_visit_fully_adj_fn(visit = matched_data$hos_ac_visit_m9,
#                                     fu_time = matched_data$follow_up_m9,
#                                     data = matched_data,
#                                     month = 9),
#       cumulative_visit_fully_adj_fn(visit = matched_data$hos_ac_visit_m10,
#                                     fu_time = matched_data$follow_up_m10,
#                                     data = matched_data,
#                                     month = 10),
#       cumulative_visit_fully_adj_fn(visit = matched_data$hos_ac_visit_m11,
#                                     fu_time = matched_data$follow_up_m11,
#                                     data = matched_data,
#                                     month = 11),
#       cumulative_visit_fully_adj_fn(visit = matched_data$hos_ac_visit_m12,
#                                     fu_time = matched_data$follow_up_m12,
#                                     data = matched_data,
#                                     month = 12)) %>% 
#       mutate(model = "Fully adjusted") %>% 
#       relocate(model)
# 
# hos_results_hurdle <- bind_rows(hos_crude, hos_part_adjusted, hos_fully_adjusted)
# hos_results_hurdle %>% write.csv(here("output", "st_03_hos_result_cumulative_visit_hurdle.csv"), row.names = F)
# 
# 
# # F. A&E visits: -----
# # Crude
# ae_crude <- bind_rows(
#       cumulative_visit_crude_fn(visit = matched_data$ae_ac_visit_m1,
#                                 fu_time = matched_data$follow_up_m1,
#                                 data = matched_data,
#                                 month = 1),
#       cumulative_visit_crude_fn(visit = matched_data$ae_ac_visit_m2,
#                                 fu_time = matched_data$follow_up_m2,
#                                 data = matched_data,
#                                 month = 2),
#       cumulative_visit_crude_fn(visit = matched_data$ae_ac_visit_m3,
#                                 fu_time = matched_data$follow_up_m3,
#                                 data = matched_data,
#                                 month = 3),
#       cumulative_visit_crude_fn(visit = matched_data$ae_ac_visit_m4,
#                                 fu_time = matched_data$follow_up_m4,
#                                 data = matched_data,
#                                 month = 4),
#       cumulative_visit_crude_fn(visit = matched_data$ae_ac_visit_m5,
#                                 fu_time = matched_data$follow_up_m5,
#                                 data = matched_data,
#                                 month = 5),
#       cumulative_visit_crude_fn(visit = matched_data$ae_ac_visit_m6,
#                                 fu_time = matched_data$follow_up_m6,
#                                 data = matched_data,
#                                 month = 6),
#       cumulative_visit_crude_fn(visit = matched_data$ae_ac_visit_m7,
#                                 fu_time = matched_data$follow_up_m7,
#                                 data = matched_data,
#                                 month = 7),
#       cumulative_visit_crude_fn(visit = matched_data$ae_ac_visit_m8,
#                                 fu_time = matched_data$follow_up_m8,
#                                 data = matched_data,
#                                 month = 8),
#       cumulative_visit_crude_fn(visit = matched_data$ae_ac_visit_m9,
#                                 fu_time = matched_data$follow_up_m9,
#                                 data = matched_data,
#                                 month = 9),
#       cumulative_visit_crude_fn(visit = matched_data$ae_ac_visit_m10,
#                                 fu_time = matched_data$follow_up_m10,
#                                 data = matched_data,
#                                 month = 10),
#       cumulative_visit_crude_fn(visit = matched_data$ae_ac_visit_m11,
#                                 fu_time = matched_data$follow_up_m11,
#                                 data = matched_data,
#                                 month = 11),
#       cumulative_visit_crude_fn(visit = matched_data$ae_ac_visit_m12,
#                                 fu_time = matched_data$follow_up_m12,
#                                 data = matched_data,
#                                 month = 12)) %>% 
#       mutate(model = "Crude") %>% 
#       relocate(model)
# 
# # Partially adjusted 
# ae_part_adjusted <- bind_rows(
#       cumulative_visit_partially_adj_fn(visit = matched_data$ae_ac_visit_m1,
#                                         fu_time = matched_data$follow_up_m1,
#                                         data = matched_data,
#                                         month = 1),
#       cumulative_visit_partially_adj_fn(visit = matched_data$ae_ac_visit_m2,
#                                         fu_time = matched_data$follow_up_m2,
#                                         data = matched_data,
#                                         month = 2),
#       cumulative_visit_partially_adj_fn(visit = matched_data$ae_ac_visit_m3,
#                                         fu_time = matched_data$follow_up_m3,
#                                         data = matched_data,
#                                         month = 3),
#       cumulative_visit_partially_adj_fn(visit = matched_data$ae_ac_visit_m4,
#                                         fu_time = matched_data$follow_up_m4,
#                                         data = matched_data,
#                                         month = 4),
#       cumulative_visit_partially_adj_fn(visit = matched_data$ae_ac_visit_m5,
#                                         fu_time = matched_data$follow_up_m5,
#                                         data = matched_data,
#                                         month = 5),
#       cumulative_visit_partially_adj_fn(visit = matched_data$ae_ac_visit_m6,
#                                         fu_time = matched_data$follow_up_m6,
#                                         data = matched_data,
#                                         month = 6),
#       cumulative_visit_partially_adj_fn(visit = matched_data$ae_ac_visit_m7,
#                                         fu_time = matched_data$follow_up_m7,
#                                         data = matched_data,
#                                         month = 7),
#       cumulative_visit_partially_adj_fn(visit = matched_data$ae_ac_visit_m8,
#                                         fu_time = matched_data$follow_up_m8,
#                                         data = matched_data,
#                                         month = 8),
#       cumulative_visit_partially_adj_fn(visit = matched_data$ae_ac_visit_m9,
#                                         fu_time = matched_data$follow_up_m9,
#                                         data = matched_data,
#                                         month = 9),
#       cumulative_visit_partially_adj_fn(visit = matched_data$ae_ac_visit_m10,
#                                         fu_time = matched_data$follow_up_m10,
#                                         data = matched_data,
#                                         month = 10),
#       cumulative_visit_partially_adj_fn(visit = matched_data$ae_ac_visit_m11,
#                                         fu_time = matched_data$follow_up_m11,
#                                         data = matched_data,
#                                         month = 11),
#       cumulative_visit_partially_adj_fn(visit = matched_data$ae_ac_visit_m12,
#                                         fu_time = matched_data$follow_up_m12,
#                                         data = matched_data,
#                                         month = 12)) %>% 
#       mutate(model = "Partially adjusted") %>% 
#       relocate(model)
# 
# # Fully adjusted
# ae_fully_adjusted <- bind_rows(
#       cumulative_visit_fully_adj_fn(visit = matched_data$ae_ac_visit_m1,
#                                     fu_time = matched_data$follow_up_m1,
#                                     data = matched_data,
#                                     month = 1),
#       cumulative_visit_fully_adj_fn(visit = matched_data$ae_ac_visit_m2,
#                                     fu_time = matched_data$follow_up_m2,
#                                     data = matched_data,
#                                     month = 2),
#       cumulative_visit_fully_adj_fn(visit = matched_data$ae_ac_visit_m3,
#                                     fu_time = matched_data$follow_up_m3,
#                                     data = matched_data,
#                                     month = 3),
#       cumulative_visit_fully_adj_fn(visit = matched_data$ae_ac_visit_m4,
#                                     fu_time = matched_data$follow_up_m4,
#                                     data = matched_data,
#                                     month = 4),
#       cumulative_visit_fully_adj_fn(visit = matched_data$ae_ac_visit_m5,
#                                     fu_time = matched_data$follow_up_m5,
#                                     data = matched_data,
#                                     month = 5),
#       cumulative_visit_fully_adj_fn(visit = matched_data$ae_ac_visit_m6,
#                                     fu_time = matched_data$follow_up_m6,
#                                     data = matched_data,
#                                     month = 6),
#       cumulative_visit_fully_adj_fn(visit = matched_data$ae_ac_visit_m7,
#                                     fu_time = matched_data$follow_up_m7,
#                                     data = matched_data,
#                                     month = 7),
#       cumulative_visit_fully_adj_fn(visit = matched_data$ae_ac_visit_m8,
#                                     fu_time = matched_data$follow_up_m8,
#                                     data = matched_data,
#                                     month = 8),
#       cumulative_visit_fully_adj_fn(visit = matched_data$ae_ac_visit_m9,
#                                     fu_time = matched_data$follow_up_m9,
#                                     data = matched_data,
#                                     month = 9),
#       cumulative_visit_fully_adj_fn(visit = matched_data$ae_ac_visit_m10,
#                                     fu_time = matched_data$follow_up_m10,
#                                     data = matched_data,
#                                     month = 10),
#       cumulative_visit_fully_adj_fn(visit = matched_data$ae_ac_visit_m11,
#                                     fu_time = matched_data$follow_up_m11,
#                                     data = matched_data,
#                                     month = 11),
#       cumulative_visit_fully_adj_fn(visit = matched_data$ae_ac_visit_m12,
#                                     fu_time = matched_data$follow_up_m12,
#                                     data = matched_data,
#                                     month = 12)) %>% 
#       mutate(model = "Fully adjusted") %>% 
#       relocate(model)
# 
# 
# ae_results_hurdle <- bind_rows(ae_crude, ae_part_adjusted, ae_fully_adjusted)
# ae_results_hurdle %>% write.csv(here("output", "st_03_ae_result_cumulative_visit_hurdle.csv"), row.names = F)
