# Load previous data management
source("analysis/dm03_matched_define_monthly_follow_up_time.R")

# For organising the outputs
options(digits=2)


# Saperate data for subgroup analysis:
# previous hospitalisation due to covid
hospital <- matched_data %>% filter(previous_covid_hosp == "T") 
# Not hospitalised due to COVID
non_hospital <- matched_data %>% filter(previous_covid_hosp == "F")

# Crude Hurdle model function: 
month_hurdle_fn <- function(month, n, data) {
      # Fit a hurdle model to the data.
      model <- hurdle(
            month ~ exposure | exposure + age + sex,
            data = data,
            zero.dist = "binomial",
            dist = "poisson"
      )
      
      # Extract the coefficients and 95% confidence intervals from the model.
      # # Extract coefficients
      coef_results <- data.frame(exp(coef(model)))
      names(coef_results) <- "Coefficients"
      
      # # Extract CI
      results_ci <- data.frame(exp(confint(model))) %>% 
            setnames(old = c("X2.5..", "X97.5.."), new = c("Lower CI", "Upper CI"))
      
      # Combine the results.
      results <- cbind(coef_results, results_ci)
      
      results$month <- n
      
      # Return the results.
      return(results)
}

# A.
# 1. Crude results: overall ----
all_results <- bind_rows(
      month_hurdle_fn(matched_data$all_month1, 1, data = matched_data),
      month_hurdle_fn(matched_data$all_month2, 2, data = matched_data),
      month_hurdle_fn(matched_data$all_month3, 3, data = matched_data),
      month_hurdle_fn(matched_data$all_month4, 4, data = matched_data),
      month_hurdle_fn(matched_data$all_month5, 5, data = matched_data),
      month_hurdle_fn(matched_data$all_month6, 6, data = matched_data),
      month_hurdle_fn(matched_data$all_month7, 7, data = matched_data),
      month_hurdle_fn(matched_data$all_month8, 8, data = matched_data),
      month_hurdle_fn(matched_data$all_month9, 9, data = matched_data),
      month_hurdle_fn(matched_data$all_month10, 10, data = matched_data),
      month_hurdle_fn(matched_data$all_month11, 11, data = matched_data),
      month_hurdle_fn(matched_data$all_month12, 12, data = matched_data),
)

all_results %>% 
      filter(rownames(.) %>% startsWith("count_")) %>% 
      write.csv(here("output", "st03_monthly_visits_crude_hurdle.csv"), 
                row.names = T)

# 2. Crude results: stratified by previous hospitalisation -----
# COVID hospitalised people:
hospital_crude_results <- bind_rows(
      month_hurdle_fn(hospital$all_month1, 1, data = hospital),
      month_hurdle_fn(hospital$all_month2, 2, data = hospital),
      month_hurdle_fn(hospital$all_month3, 3, data = hospital),
      month_hurdle_fn(hospital$all_month4, 4, data = hospital),
      month_hurdle_fn(hospital$all_month5, 5, data = hospital),
      month_hurdle_fn(hospital$all_month6, 6, data = hospital),
      month_hurdle_fn(hospital$all_month7, 7, data = hospital),
      month_hurdle_fn(hospital$all_month8, 8, data = hospital),
      month_hurdle_fn(hospital$all_month9, 9, data = hospital),
      month_hurdle_fn(hospital$all_month10, 10, data = hospital),
      month_hurdle_fn(hospital$all_month11, 11, data = hospital),
      month_hurdle_fn(hospital$all_month12, 12, data = hospital),
)
hospital_crude_results %>% 
      filter(rownames(.) %>% startsWith("count_")) %>% 
      write.csv(here("output", "st03_monthly_visits_crude_hurdle_hospital.csv"), 
                row.names = T)

# No hospitalisation due to COVID:
no_hos_crude_results<- bind_rows(
      month_hurdle_fn(non_hospital$all_month1, 1, data = non_hospital),
      month_hurdle_fn(non_hospital$all_month2, 2, data = non_hospital),
      month_hurdle_fn(non_hospital$all_month3, 3, data = non_hospital),
      month_hurdle_fn(non_hospital$all_month4, 4, data = non_hospital),
      month_hurdle_fn(non_hospital$all_month5, 5, data = non_hospital),
      month_hurdle_fn(non_hospital$all_month6, 6, data = non_hospital),
      month_hurdle_fn(non_hospital$all_month7, 7, data = non_hospital),
      month_hurdle_fn(non_hospital$all_month8, 8, data = non_hospital),
      month_hurdle_fn(non_hospital$all_month9, 9, data = non_hospital),
      month_hurdle_fn(non_hospital$all_month10, 10, data = non_hospital),
      month_hurdle_fn(non_hospital$all_month11, 11, data = non_hospital),
      month_hurdle_fn(non_hospital$all_month12, 12, data = non_hospital),
)
no_hos_crude_results%>% 
      filter(rownames(.) %>% startsWith("count_")) %>% 
      write.csv(here("output", "st03_monthly_visits_crude_hurdle_non_hospital.csv"), 
                row.names = T)
