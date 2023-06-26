# Run difference in difference model:

# Load previous data management
source("analysis/dm02_matched_hx_data.R")


# 0. model selection: Use 6 month to test models -----
model_poisson <- glm(all_month1 ~ exposure*time + offset(log(fu_time_m1)),
       data = hx_matched_data, family = "poisson")
# Use negative binomial model: 
model_nb <- glm.nb(all_month1 ~ exposure*time + offset(log(fu_time_m1)),
       data = hx_matched_data) 

# compare models:
bind_rows(
      (model_nb %>% glance() %>% dplyr::select(AIC, BIC, deviance, df.residual) %>% 
            mutate(model = "Negative binomial - crude")),
      (model_poisson %>% glance()  %>% dplyr::select(AIC, BIC, deviance, df.residual) %>% 
            mutate(model = "Poisson - crude"))
      ) %>% relocate(model) %>% 
      mutate(dispersion = deviance/df.residual) %>% 
      write.csv(here("output", "sup_st01_model_compare.csv"), row.names = F)

# Poisson functions for all models: 

# 1. Crude poisson: -----
did_poisson_crude_fn <- function(all_vist, fu_time){
      glm(all_vist ~ exposure*time + offset(log(fu_time)),
          data = hx_matched_data, family = "poisson")
}

# Apply the function
crude_month_1 <- did_poisson_crude_fn(hx_matched_data$all_month1, hx_matched_data$fu_time_m1)
crude_month_2 <- did_poisson_crude_fn(hx_matched_data$all_month2, hx_matched_data$fu_time_m2)
crude_month_3 <- did_poisson_crude_fn(hx_matched_data$all_month3, hx_matched_data$fu_time_m3)
crude_month_4 <- did_poisson_crude_fn(hx_matched_data$all_month4, hx_matched_data$fu_time_m4)
crude_month_5 <- did_poisson_crude_fn(hx_matched_data$all_month5, hx_matched_data$fu_time_m5)
crude_month_6 <- did_poisson_crude_fn(hx_matched_data$all_month6, hx_matched_data$fu_time_m6)
crude_month_7 <- did_poisson_crude_fn(hx_matched_data$all_month7, hx_matched_data$fu_time_m7)
crude_month_8 <- did_poisson_crude_fn(hx_matched_data$all_month8, hx_matched_data$fu_time_m8)
crude_month_9 <- did_poisson_crude_fn(hx_matched_data$all_month9, hx_matched_data$fu_time_m9)
crude_month_10 <- did_poisson_crude_fn(hx_matched_data$all_month10, hx_matched_data$fu_time_m10)
crude_month_11 <- did_poisson_crude_fn(hx_matched_data$all_month11, hx_matched_data$fu_time_m11)
crude_month_12 <- did_poisson_crude_fn(hx_matched_data$all_month12, hx_matched_data$fu_time_m12)

## save stats output ----
stats_output <- bind_rows(
      crude_month_1 %>% tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
            mutate(model = "crude_month_1"),
      crude_month_2 %>% tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
            mutate(model = "crude_month_2"),
      crude_month_3 %>% tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
            mutate(model = "crude_month_3"),
      crude_month_4 %>% tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
            mutate(model = "crude_month_4"),
      crude_month_5 %>% tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
            mutate(model = "crude_month_5"),
      crude_month_6 %>% tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
            mutate(model = "crude_month_6"),
      crude_month_7 %>% tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
            mutate(model = "crude_month_7"),
      crude_month_8 %>% tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
            mutate(model = "crude_month_8"),
      crude_month_9 %>% tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
            mutate(model = "crude_month_9"),
      crude_month_10 %>% tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
            mutate(model = "crude_month_10"),
      crude_month_11 %>% tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
            mutate(model = "crude_month_11"),
      crude_month_12 %>% tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
            mutate(model = "crude_month_12")
)

## Obtain the fitted value: ----
# write a function for extracting fitted value
crude_fitted_fn<- function(visit, follow_up, reg_object){
      hx_matched_data %>% 
            filter(!is.na(visit) & !is.na(follow_up)) %>% 
            dplyr::select(patient_id,exposure, time) %>% 
            mutate(predict_value = reg_object$fitted.values)
}

crude_predicted_m1 <- crude_fitted_fn(hx_matched_data$all_month1, hx_matched_data$fu_time_m1, crude_month_1) %>% 
      rename(crude_predicted_m1 = predict_value)
crude_predicted_m2 <- crude_fitted_fn(hx_matched_data$all_month2, hx_matched_data$fu_time_m2, crude_month_2) %>% 
      rename(crude_predicted_m2 = predict_value)
crude_predicted_m3 <- crude_fitted_fn(hx_matched_data$all_month3, hx_matched_data$fu_time_m3, crude_month_3) %>% 
      rename(crude_predicted_m3 = predict_value)
crude_predicted_m4 <- crude_fitted_fn(hx_matched_data$all_month4, hx_matched_data$fu_time_m4, crude_month_4) %>% 
      rename(crude_predicted_m4 = predict_value)
crude_predicted_m5 <- crude_fitted_fn(hx_matched_data$all_month5, hx_matched_data$fu_time_m5, crude_month_5) %>% 
      rename(crude_predicted_m5 = predict_value)
crude_predicted_m6 <- crude_fitted_fn(hx_matched_data$all_month6, hx_matched_data$fu_time_m6, crude_month_6) %>% 
      rename(crude_predicted_m6 = predict_value)
crude_predicted_m7 <- crude_fitted_fn(hx_matched_data$all_month7, hx_matched_data$fu_time_m7, crude_month_7) %>% 
      rename(crude_predicted_m7 = predict_value)
crude_predicted_m8 <- crude_fitted_fn(hx_matched_data$all_month8, hx_matched_data$fu_time_m8, crude_month_8) %>% 
      rename(crude_predicted_m8 = predict_value)
crude_predicted_m9 <- crude_fitted_fn(hx_matched_data$all_month9, hx_matched_data$fu_time_m9, crude_month_9) %>% 
      rename(crude_predicted_m9 = predict_value)
crude_predicted_m10 <- crude_fitted_fn(hx_matched_data$all_month10, hx_matched_data$fu_time_m10, crude_month_10) %>% 
      rename(crude_predicted_m10 = predict_value)
crude_predicted_m11 <- crude_fitted_fn(hx_matched_data$all_month11, hx_matched_data$fu_time_m11, crude_month_11) %>% 
      rename(crude_predicted_m11 = predict_value)
crude_predicted_m12 <- crude_fitted_fn(hx_matched_data$all_month12, hx_matched_data$fu_time_m12, crude_month_12) %>% 
      rename(crude_predicted_m12 = predict_value)

# Added them back to the original data 
combined_predicted_value <- 
      hx_matched_data %>% dplyr::select(patient_id,exposure, time) %>% 
      left_join(crude_predicted_m1, by = c("patient_id","exposure", "time")) %>% 
      left_join(crude_predicted_m2, by = c("patient_id","exposure", "time")) %>% 
      left_join(crude_predicted_m3, by = c("patient_id","exposure", "time")) %>% 
      left_join(crude_predicted_m4, by = c("patient_id","exposure", "time")) %>% 
      left_join(crude_predicted_m5, by = c("patient_id","exposure", "time")) %>% 
      left_join(crude_predicted_m6, by = c("patient_id","exposure", "time")) %>% 
      left_join(crude_predicted_m7, by = c("patient_id","exposure", "time")) %>% 
      left_join(crude_predicted_m8, by = c("patient_id","exposure", "time")) %>% 
      left_join(crude_predicted_m9, by = c("patient_id","exposure", "time")) %>% 
      left_join(crude_predicted_m10, by = c("patient_id","exposure", "time")) %>% 
      left_join(crude_predicted_m11, by = c("patient_id","exposure", "time")) %>% 
      left_join(crude_predicted_m12, by = c("patient_id","exposure", "time"))


# Adjusted Poisson model: 


# 2. adjusted poisson: -----
did_poisson_adjusted_fn <- function(all_vist, fu_time){
      glm(all_vist ~ exposure*time + sex + age_cat + ethnicity_6+ bmi_cat + 
                number_comorbidities_cat + offset(log(fu_time)),
          data = hx_matched_data, family = "poisson")
}

# Apply the function
adjusted_month_1 <- did_poisson_adjusted_fn(hx_matched_data$all_month1, hx_matched_data$fu_time_m1)
adjusted_month_2 <- did_poisson_adjusted_fn(hx_matched_data$all_month2, hx_matched_data$fu_time_m2)
adjusted_month_3 <- did_poisson_adjusted_fn(hx_matched_data$all_month3, hx_matched_data$fu_time_m3)
adjusted_month_4 <- did_poisson_adjusted_fn(hx_matched_data$all_month4, hx_matched_data$fu_time_m4)
adjusted_month_5 <- did_poisson_adjusted_fn(hx_matched_data$all_month5, hx_matched_data$fu_time_m5)
adjusted_month_6 <- did_poisson_adjusted_fn(hx_matched_data$all_month6, hx_matched_data$fu_time_m6)
adjusted_month_7 <- did_poisson_adjusted_fn(hx_matched_data$all_month7, hx_matched_data$fu_time_m7)
adjusted_month_8 <- did_poisson_adjusted_fn(hx_matched_data$all_month8, hx_matched_data$fu_time_m8)
adjusted_month_9 <- did_poisson_adjusted_fn(hx_matched_data$all_month9, hx_matched_data$fu_time_m9)
adjusted_month_10 <- did_poisson_adjusted_fn(hx_matched_data$all_month10, hx_matched_data$fu_time_m10)
adjusted_month_11 <- did_poisson_adjusted_fn(hx_matched_data$all_month11, hx_matched_data$fu_time_m11)
adjusted_month_12 <- did_poisson_adjusted_fn(hx_matched_data$all_month12, hx_matched_data$fu_time_m12)


## save the stats output: ----

stats_output <- bind_rows(stats_output,
                          adjusted_month_1 %>% tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
                                mutate(model = "adjusted_month_1"),
                          adjusted_month_2 %>% tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
                                mutate(model = "adjusted_month_2"),
                          adjusted_month_3 %>% tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
                                mutate(model = "adjusted_month_3"),
                          adjusted_month_4 %>% tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
                                mutate(model = "adjusted_month_4"),
                          adjusted_month_5 %>% tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
                                mutate(model = "adjusted_month_5"),
                          adjusted_month_6 %>% tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
                                mutate(model = "adjusted_month_6"),
                          adjusted_month_7 %>% tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
                                mutate(model = "adjusted_month_7"),
                          adjusted_month_8 %>% tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
                                mutate(model = "adjusted_month_8"),
                          adjusted_month_9 %>% tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
                                mutate(model = "adjusted_month_9"),
                          adjusted_month_10 %>% tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
                                mutate(model = "adjusted_month_10"),
                          adjusted_month_11 %>% tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
                                mutate(model = "adjusted_month_11"),
                          adjusted_month_12 %>% tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
                                mutate(model = "adjusted_month_12")
)

# save the results:
stats_output %>% write_csv(here("output", "st05_did_stats.csv"))

## Obtain the fitted value: ----
# write a function for extracting fitted value
adjusted_fitted_fn<- function(visit, follow_up, reg_object){
      hx_matched_data %>% 
            filter(!is.na(visit) & !is.na(follow_up) &
                   !is.na(sex) & !is.na(age_cat) & !is.na(ethnicity_6) & 
                  !is.na(bmi_cat) & !is.na(number_comorbidities_cat)) %>% 
            dplyr::select(patient_id,exposure, time) %>% 
            mutate(predict_value = reg_object$fitted.values)
}

adjusted_predicted_m1 <- adjusted_fitted_fn(hx_matched_data$all_month1, hx_matched_data$fu_time_m1, adjusted_month_1) %>% 
      rename(adjusted_predicted_m1 = predict_value)
adjusted_predicted_m2 <- adjusted_fitted_fn(hx_matched_data$all_month2, hx_matched_data$fu_time_m2, adjusted_month_2) %>% 
      rename(adjusted_predicted_m2 = predict_value)
adjusted_predicted_m3 <- adjusted_fitted_fn(hx_matched_data$all_month3, hx_matched_data$fu_time_m3, adjusted_month_3) %>% 
      rename(adjusted_predicted_m3 = predict_value)
adjusted_predicted_m4 <- adjusted_fitted_fn(hx_matched_data$all_month4, hx_matched_data$fu_time_m4, adjusted_month_4) %>% 
      rename(adjusted_predicted_m4 = predict_value)
adjusted_predicted_m5 <- adjusted_fitted_fn(hx_matched_data$all_month5, hx_matched_data$fu_time_m5, adjusted_month_5) %>% 
      rename(adjusted_predicted_m5 = predict_value)
adjusted_predicted_m6 <- adjusted_fitted_fn(hx_matched_data$all_month6, hx_matched_data$fu_time_m6, adjusted_month_6) %>% 
      rename(adjusted_predicted_m6 = predict_value)
adjusted_predicted_m7 <- adjusted_fitted_fn(hx_matched_data$all_month7, hx_matched_data$fu_time_m7, adjusted_month_7) %>% 
      rename(adjusted_predicted_m7 = predict_value)
adjusted_predicted_m8 <- adjusted_fitted_fn(hx_matched_data$all_month8, hx_matched_data$fu_time_m8, adjusted_month_8) %>% 
      rename(adjusted_predicted_m8 = predict_value)
adjusted_predicted_m9 <- adjusted_fitted_fn(hx_matched_data$all_month9, hx_matched_data$fu_time_m9, adjusted_month_9) %>% 
      rename(adjusted_predicted_m9 = predict_value)
adjusted_predicted_m10 <- adjusted_fitted_fn(hx_matched_data$all_month10, hx_matched_data$fu_time_m10, adjusted_month_10) %>% 
      rename(adjusted_predicted_m10 = predict_value)
adjusted_predicted_m11 <- adjusted_fitted_fn(hx_matched_data$all_month11, hx_matched_data$fu_time_m11, adjusted_month_11) %>% 
      rename(adjusted_predicted_m11 = predict_value)
adjusted_predicted_m12 <- adjusted_fitted_fn(hx_matched_data$all_month12, hx_matched_data$fu_time_m12, adjusted_month_12) %>% 
      rename(adjusted_predicted_m12 = predict_value)

# Added them back to the original data 
combined_predicted_value <- combined_predicted_value %>% 
      left_join(adjusted_predicted_m1, by = c("patient_id","exposure", "time")) %>% 
      left_join(adjusted_predicted_m2, by = c("patient_id","exposure", "time")) %>% 
      left_join(adjusted_predicted_m3, by = c("patient_id","exposure", "time")) %>% 
      left_join(adjusted_predicted_m4, by = c("patient_id","exposure", "time")) %>% 
      left_join(adjusted_predicted_m5, by = c("patient_id","exposure", "time")) %>% 
      left_join(adjusted_predicted_m6, by = c("patient_id","exposure", "time")) %>% 
      left_join(adjusted_predicted_m7, by = c("patient_id","exposure", "time")) %>% 
      left_join(adjusted_predicted_m8, by = c("patient_id","exposure", "time")) %>% 
      left_join(adjusted_predicted_m9, by = c("patient_id","exposure", "time")) %>% 
      left_join(adjusted_predicted_m10, by = c("patient_id","exposure", "time")) %>% 
      left_join(adjusted_predicted_m11, by = c("patient_id","exposure", "time")) %>% 
      left_join(adjusted_predicted_m12, by = c("patient_id","exposure", "time"))

# save file for later plotting
combined_predicted_value %>% write_csv(here("output", "predicted_did_counts.csv.gz"))

# summarising the results
summarised_results <- 
      combined_predicted_value %>% 
      group_by(time, exposure) %>% 
      summarise(
            crude_visit_m1 = mean(crude_predicted_m1, na.rm = TRUE),
            crude_visit_m2 = mean(crude_predicted_m2, na.rm = TRUE),
            crude_visit_m3 = mean(crude_predicted_m3, na.rm = TRUE),
            crude_visit_m4 = mean(crude_predicted_m4, na.rm = TRUE),
            crude_visit_m5 = mean(crude_predicted_m5, na.rm = TRUE),
            crude_visit_m6 = mean(crude_predicted_m6, na.rm = TRUE),
            crude_visit_m7 = mean(crude_predicted_m7, na.rm = TRUE),
            crude_visit_m8 = mean(crude_predicted_m8, na.rm = TRUE),
            crude_visit_m9 = mean(crude_predicted_m9, na.rm = TRUE),
            crude_visit_m10 = mean(crude_predicted_m10, na.rm = TRUE),
            crude_visit_m11 = mean(crude_predicted_m11, na.rm = TRUE),
            crude_visit_m12 = mean(crude_predicted_m12, na.rm = TRUE),
            adjusted_visit_m1 = mean(adjusted_predicted_m1, na.rm = TRUE),
            adjusted_visit_m2 = mean(adjusted_predicted_m2, na.rm = TRUE),
            adjusted_visit_m3 = mean(adjusted_predicted_m3, na.rm = TRUE),
            adjusted_visit_m4 = mean(adjusted_predicted_m4, na.rm = TRUE),
            adjusted_visit_m5 = mean(adjusted_predicted_m5, na.rm = TRUE),
            adjusted_visit_m6 = mean(adjusted_predicted_m6, na.rm = TRUE),
            adjusted_visit_m7 = mean(adjusted_predicted_m7, na.rm = TRUE),
            adjusted_visit_m8 = mean(adjusted_predicted_m8, na.rm = TRUE),
            adjusted_visit_m9 = mean(adjusted_predicted_m9, na.rm = TRUE),
            adjusted_visit_m10 = mean(adjusted_predicted_m10, na.rm = TRUE),
            adjusted_visit_m11 = mean(adjusted_predicted_m11, na.rm = TRUE),
            adjusted_visit_m12 = mean(adjusted_predicted_m12, na.rm = TRUE)
      )
summarised_results %>% write_csv(here("output", "st05_summarised_did_predicted_results"))
