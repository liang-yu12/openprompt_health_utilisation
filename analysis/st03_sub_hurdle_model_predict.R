# Load previous data management
source("analysis/dm03_matched_define_monthly_follow_up_time.R")

# For organising the outputs
options(digits=2)


hospitalised <- matched_data %>% subset(previous_covid_hosp == "T")
no_hostpitalised <- matched_data %>% subset(previous_covid_hosp == "F")

# A. Crude hurdle model ------------
cumulative_visit_crude_fn <- function(visit, fu_time, data, month) {
      
      # Create a hurdle model
      model <- hurdle(
            visit ~ exposure + offset(log(fu_time)) | age_cat + sex+ region,
            data = data,
            zero.dist = "binomial",
            dist = "poisson"
      )
      
      # because original dataset has NA, need to first exclude them first
      # then add the predicted value back to the table. 
      no_na <- filter(data, !is.na(fu_time))
      no_na$predict <- predict(model, na.action = na.exclude)
      results <- no_na %>% # summarise the mean visit and sd.
            group_by(exposure) %>% 
            summarise(mean = mean(predict),
                      sd = sd(predict)) %>% 
            mutate(month = month) %>% relocate(month)
      
      return(results)
}

results_crude_hos <- bind_rows(
      cumulative_visit_crude_fn(visit = hospitalised$all_month1,
                                fu_time = hospitalised$follow_up_m1,
                                data = hospitalised,
                                month = 1),
      cumulative_visit_crude_fn(visit = hospitalised$all_month2,
                                fu_time = hospitalised$follow_up_m2,
                                data = hospitalised,
                                month = 2),
      cumulative_visit_crude_fn(visit = hospitalised$all_month3,
                                fu_time = hospitalised$follow_up_m3,
                                data = hospitalised,
                                month = 3),
      cumulative_visit_crude_fn(visit = hospitalised$all_month4,
                                fu_time = hospitalised$follow_up_m4,
                                data = hospitalised,
                                month = 4),
      cumulative_visit_crude_fn(visit = hospitalised$all_month5,
                                fu_time = hospitalised$follow_up_m5,
                                data = hospitalised,
                                month = 5),
      cumulative_visit_crude_fn(visit = hospitalised$all_month6,
                                fu_time = hospitalised$follow_up_m6,
                                data = hospitalised,
                                month = 6),
      cumulative_visit_crude_fn(visit = hospitalised$all_month7,
                                fu_time = hospitalised$follow_up_m7,
                                data = hospitalised,
                                month = 7),
      cumulative_visit_crude_fn(visit = hospitalised$all_month8,
                                fu_time = hospitalised$follow_up_m8,
                                data = hospitalised,
                                month = 8),
      cumulative_visit_crude_fn(visit = hospitalised$all_month9,
                                fu_time = hospitalised$follow_up_m9,
                                data = hospitalised,
                                month = 9),
      cumulative_visit_crude_fn(visit = hospitalised$all_month10,
                                fu_time = hospitalised$follow_up_m10,
                                data = hospitalised,
                                month = 10),
      cumulative_visit_crude_fn(visit = hospitalised$all_month11,
                                fu_time = hospitalised$follow_up_m11,
                                data = hospitalised,
                                month = 11),
      cumulative_visit_crude_fn(visit = hospitalised$all_month12,
                                fu_time = hospitalised$follow_up_m12,
                                data = hospitalised,
                                month = 12)) %>% 
      mutate(model = "Crude") %>% 
      relocate(model)


results_crude_no_hos <- bind_rows(
      cumulative_visit_crude_fn(visit = no_hostpitalised$all_month1,
                                fu_time = no_hostpitalised$follow_up_m1,
                                data = no_hostpitalised,
                                month = 1),
      cumulative_visit_crude_fn(visit = no_hostpitalised$all_month2,
                                fu_time = no_hostpitalised$follow_up_m2,
                                data = no_hostpitalised,
                                month = 2),
      cumulative_visit_crude_fn(visit = no_hostpitalised$all_month3,
                                fu_time = no_hostpitalised$follow_up_m3,
                                data = no_hostpitalised,
                                month = 3),
      cumulative_visit_crude_fn(visit = no_hostpitalised$all_month4,
                                fu_time = no_hostpitalised$follow_up_m4,
                                data = no_hostpitalised,
                                month = 4),
      cumulative_visit_crude_fn(visit = no_hostpitalised$all_month5,
                                fu_time = no_hostpitalised$follow_up_m5,
                                data = no_hostpitalised,
                                month = 5),
      cumulative_visit_crude_fn(visit = no_hostpitalised$all_month6,
                                fu_time = no_hostpitalised$follow_up_m6,
                                data = no_hostpitalised,
                                month = 6),
      cumulative_visit_crude_fn(visit = no_hostpitalised$all_month7,
                                fu_time = no_hostpitalised$follow_up_m7,
                                data = no_hostpitalised,
                                month = 7),
      cumulative_visit_crude_fn(visit = no_hostpitalised$all_month8,
                                fu_time = no_hostpitalised$follow_up_m8,
                                data = no_hostpitalised,
                                month = 8),
      cumulative_visit_crude_fn(visit = no_hostpitalised$all_month9,
                                fu_time = no_hostpitalised$follow_up_m9,
                                data = no_hostpitalised,
                                month = 9),
      cumulative_visit_crude_fn(visit = no_hostpitalised$all_month10,
                                fu_time = no_hostpitalised$follow_up_m10,
                                data = no_hostpitalised,
                                month = 10),
      cumulative_visit_crude_fn(visit = no_hostpitalised$all_month11,
                                fu_time = no_hostpitalised$follow_up_m11,
                                data = no_hostpitalised,
                                month = 11),
      cumulative_visit_crude_fn(visit = no_hostpitalised$all_month12,
                                fu_time = no_hostpitalised$follow_up_m12,
                                data = no_hostpitalised,
                                month = 12)) %>% 
      mutate(model = "Crude") %>% 
      relocate(model)


# B. Partially adjusted model ------
cumulative_visit_partially_adj_fn <- function(visit, fu_time, data, month) {
      
      # Create a hurdle model
      model <- hurdle(
            visit ~ exposure + sex + age_cat + offset(log(fu_time)) | age_cat + sex+ region,
            data = data,
            zero.dist = "binomial",
            dist = "poisson"
      )
      
      # because original dataset has NA, need to first exclude them first
      # then add the predicted value back to the table. 
      no_na <- filter(data, !is.na(fu_time))
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


results_partial_hos <- bind_rows(
      cumulative_visit_partially_adj_fn(visit = hospitalised$all_month1,
                                fu_time = hospitalised$follow_up_m1,
                                data = hospitalised,
                                month = 1),
      cumulative_visit_partially_adj_fn(visit = hospitalised$all_month2,
                                fu_time = hospitalised$follow_up_m2,
                                data = hospitalised,
                                month = 2),
      cumulative_visit_partially_adj_fn(visit = hospitalised$all_month3,
                                fu_time = hospitalised$follow_up_m3,
                                data = hospitalised,
                                month = 3),
      cumulative_visit_partially_adj_fn(visit = hospitalised$all_month4,
                                fu_time = hospitalised$follow_up_m4,
                                data = hospitalised,
                                month = 4),
      cumulative_visit_partially_adj_fn(visit = hospitalised$all_month5,
                                fu_time = hospitalised$follow_up_m5,
                                data = hospitalised,
                                month = 5),
      cumulative_visit_partially_adj_fn(visit = hospitalised$all_month6,
                                fu_time = hospitalised$follow_up_m6,
                                data = hospitalised,
                                month = 6),
      cumulative_visit_partially_adj_fn(visit = hospitalised$all_month7,
                                fu_time = hospitalised$follow_up_m7,
                                data = hospitalised,
                                month = 7),
      cumulative_visit_partially_adj_fn(visit = hospitalised$all_month8,
                                fu_time = hospitalised$follow_up_m8,
                                data = hospitalised,
                                month = 8),
      cumulative_visit_partially_adj_fn(visit = hospitalised$all_month9,
                                fu_time = hospitalised$follow_up_m9,
                                data = hospitalised,
                                month = 9),
      cumulative_visit_partially_adj_fn(visit = hospitalised$all_month10,
                                fu_time = hospitalised$follow_up_m10,
                                data = hospitalised,
                                month = 10),
      cumulative_visit_partially_adj_fn(visit = hospitalised$all_month11,
                                fu_time = hospitalised$follow_up_m11,
                                data = hospitalised,
                                month = 11),
      cumulative_visit_partially_adj_fn(visit = hospitalised$all_month12,
                                fu_time = hospitalised$follow_up_m12,
                                data = hospitalised,
                                month = 12)) %>% 
      mutate(model = "Partially adjusted") %>% 
      relocate(model)


results_partial_no_hos <- bind_rows(
      cumulative_visit_partially_adj_fn(visit = no_hostpitalised$all_month1,
                                fu_time = no_hostpitalised$follow_up_m1,
                                data = no_hostpitalised,
                                month = 1),
      cumulative_visit_partially_adj_fn(visit = no_hostpitalised$all_month2,
                                fu_time = no_hostpitalised$follow_up_m2,
                                data = no_hostpitalised,
                                month = 2),
      cumulative_visit_partially_adj_fn(visit = no_hostpitalised$all_month3,
                                fu_time = no_hostpitalised$follow_up_m3,
                                data = no_hostpitalised,
                                month = 3),
      cumulative_visit_partially_adj_fn(visit = no_hostpitalised$all_month4,
                                fu_time = no_hostpitalised$follow_up_m4,
                                data = no_hostpitalised,
                                month = 4),
      cumulative_visit_partially_adj_fn(visit = no_hostpitalised$all_month5,
                                fu_time = no_hostpitalised$follow_up_m5,
                                data = no_hostpitalised,
                                month = 5),
      cumulative_visit_partially_adj_fn(visit = no_hostpitalised$all_month6,
                                fu_time = no_hostpitalised$follow_up_m6,
                                data = no_hostpitalised,
                                month = 6),
      cumulative_visit_partially_adj_fn(visit = no_hostpitalised$all_month7,
                                fu_time = no_hostpitalised$follow_up_m7,
                                data = no_hostpitalised,
                                month = 7),
      cumulative_visit_partially_adj_fn(visit = no_hostpitalised$all_month8,
                                fu_time = no_hostpitalised$follow_up_m8,
                                data = no_hostpitalised,
                                month = 8),
      cumulative_visit_partially_adj_fn(visit = no_hostpitalised$all_month9,
                                fu_time = no_hostpitalised$follow_up_m9,
                                data = no_hostpitalised,
                                month = 9),
      cumulative_visit_partially_adj_fn(visit = no_hostpitalised$all_month10,
                                fu_time = no_hostpitalised$follow_up_m10,
                                data = no_hostpitalised,
                                month = 10),
      cumulative_visit_partially_adj_fn(visit = no_hostpitalised$all_month11,
                                fu_time = no_hostpitalised$follow_up_m11,
                                data = no_hostpitalised,
                                month = 11),
      cumulative_visit_partially_adj_fn(visit = no_hostpitalised$all_month12,
                                fu_time = no_hostpitalised$follow_up_m12,
                                data = no_hostpitalised,
                                month = 12)) %>% 
      mutate(model = "Partially adjusted") %>% 
      relocate(model)


# C. Fully adjusted model  ---------


cumulative_visit_fully_adj_fn <- function(visit, fu_time, data, month) {
      
      # Create a hurdle model
      model <- hurdle(
            visit ~ exposure + sex + age_cat + cov_covid_vax_n_cat + bmi_cat + imd_q5 + ethnicity_6 + region + 
                  number_comorbidities_cat + offset(log(fu_time)) | age_cat + sex+ region,
            data = data,
            zero.dist = "binomial",
            dist = "poisson"
      )
      
      # because original dataset has NA, need to first exclude them first
      # then add the predicted value back to the table. 
      no_na <- filter(data, !is.na(fu_time))
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


results_full_hos <- bind_rows(
      cumulative_visit_fully_adj_fn(visit = hospitalised$all_month1,
                                        fu_time = hospitalised$follow_up_m1,
                                        data = hospitalised,
                                        month = 1),
      cumulative_visit_fully_adj_fn(visit = hospitalised$all_month2,
                                        fu_time = hospitalised$follow_up_m2,
                                        data = hospitalised,
                                        month = 2),
      cumulative_visit_fully_adj_fn(visit = hospitalised$all_month3,
                                        fu_time = hospitalised$follow_up_m3,
                                        data = hospitalised,
                                        month = 3),
      cumulative_visit_fully_adj_fn(visit = hospitalised$all_month4,
                                        fu_time = hospitalised$follow_up_m4,
                                        data = hospitalised,
                                        month = 4),
      cumulative_visit_fully_adj_fn(visit = hospitalised$all_month5,
                                        fu_time = hospitalised$follow_up_m5,
                                        data = hospitalised,
                                        month = 5),
      cumulative_visit_fully_adj_fn(visit = hospitalised$all_month6,
                                        fu_time = hospitalised$follow_up_m6,
                                        data = hospitalised,
                                        month = 6),
      cumulative_visit_fully_adj_fn(visit = hospitalised$all_month7,
                                        fu_time = hospitalised$follow_up_m7,
                                        data = hospitalised,
                                        month = 7),
      cumulative_visit_fully_adj_fn(visit = hospitalised$all_month8,
                                        fu_time = hospitalised$follow_up_m8,
                                        data = hospitalised,
                                        month = 8),
      cumulative_visit_fully_adj_fn(visit = hospitalised$all_month9,
                                        fu_time = hospitalised$follow_up_m9,
                                        data = hospitalised,
                                        month = 9),
      cumulative_visit_fully_adj_fn(visit = hospitalised$all_month10,
                                        fu_time = hospitalised$follow_up_m10,
                                        data = hospitalised,
                                        month = 10),
      cumulative_visit_fully_adj_fn(visit = hospitalised$all_month11,
                                        fu_time = hospitalised$follow_up_m11,
                                        data = hospitalised,
                                        month = 11),
      cumulative_visit_fully_adj_fn(visit = hospitalised$all_month12,
                                        fu_time = hospitalised$follow_up_m12,
                                        data = hospitalised,
                                        month = 12)) %>% 
      mutate(model = "Fully adjusted") %>% 
      relocate(model)


results_full_no_hos <- bind_rows(
      cumulative_visit_fully_adj_fn(visit = no_hostpitalised$all_month1,
                                        fu_time = no_hostpitalised$follow_up_m1,
                                        data = no_hostpitalised,
                                        month = 1),
      cumulative_visit_fully_adj_fn(visit = no_hostpitalised$all_month2,
                                        fu_time = no_hostpitalised$follow_up_m2,
                                        data = no_hostpitalised,
                                        month = 2),
      cumulative_visit_fully_adj_fn(visit = no_hostpitalised$all_month3,
                                        fu_time = no_hostpitalised$follow_up_m3,
                                        data = no_hostpitalised,
                                        month = 3),
      cumulative_visit_fully_adj_fn(visit = no_hostpitalised$all_month4,
                                        fu_time = no_hostpitalised$follow_up_m4,
                                        data = no_hostpitalised,
                                        month = 4),
      cumulative_visit_fully_adj_fn(visit = no_hostpitalised$all_month5,
                                        fu_time = no_hostpitalised$follow_up_m5,
                                        data = no_hostpitalised,
                                        month = 5),
      cumulative_visit_fully_adj_fn(visit = no_hostpitalised$all_month6,
                                        fu_time = no_hostpitalised$follow_up_m6,
                                        data = no_hostpitalised,
                                        month = 6),
      cumulative_visit_fully_adj_fn(visit = no_hostpitalised$all_month7,
                                        fu_time = no_hostpitalised$follow_up_m7,
                                        data = no_hostpitalised,
                                        month = 7),
      cumulative_visit_fully_adj_fn(visit = no_hostpitalised$all_month8,
                                        fu_time = no_hostpitalised$follow_up_m8,
                                        data = no_hostpitalised,
                                        month = 8),
      cumulative_visit_fully_adj_fn(visit = no_hostpitalised$all_month9,
                                        fu_time = no_hostpitalised$follow_up_m9,
                                        data = no_hostpitalised,
                                        month = 9),
      cumulative_visit_fully_adj_fn(visit = no_hostpitalised$all_month10,
                                        fu_time = no_hostpitalised$follow_up_m10,
                                        data = no_hostpitalised,
                                        month = 10),
      cumulative_visit_fully_adj_fn(visit = no_hostpitalised$all_month11,
                                        fu_time = no_hostpitalised$follow_up_m11,
                                        data = no_hostpitalised,
                                        month = 11),
      cumulative_visit_fully_adj_fn(visit = no_hostpitalised$all_month12,
                                        fu_time = no_hostpitalised$follow_up_m12,
                                        data = no_hostpitalised,
                                        month = 12)) %>% 
      mutate(model = "Fully adjusted") %>% 
      relocate(model)

# Final output: ------
hos_results_hurdle <- bind_rows(results_crude_hos, results_partial_hos, results_full_hos)
no_hos_results_hurdle <- bind_rows(results_crude_no_hos, results_partial_no_hos, results_full_no_hos)

hos_results_hurdle %>% write.csv(here("output", "st_03_result_hos_cumulative_visit_hurdle.csv"), row.names = F)
no_hos_results_hurdle %>% write.csv(here("output", "st_03_result_no_hos_cumulative_visit_hurdle.csv"), row.names = F)