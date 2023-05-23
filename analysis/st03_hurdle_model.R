# Load previous data management
source("analysis/dm03_matched_define_monthly_follow_up_time.R")

# For organising the outputs
options(digits=2)

# Fit hurdle models: 
# The first part: counted data regression (truncated Poisson)
# The second part: the zero regression (logit)

# crude 
# Testing  codes:
# # month 1 
# crude_hc_m1_reg <- hurdle(
#       matched_data$all_month1 ~ exposure |  
#             exposure + age + sex,
#       data = matched_data,
#       zero.dist = "binomial",
#       dist = "poisson"
# )
# 
# # extract the output:
# # coefficients:
# coef_results <- data.frame(exp(coef((crude_hc_m1_reg))))
# names(coef_results) <- "Coeffiencts"
# # 95%CI:
# results_ci <- data.frame(exp(confint(crude_hc_m1_reg))) %>% 
#       setnames(old = c("X2.5..", "X97.5.."), new = c("Lower CI", "Upper CI"))
# # combine the results
# results_m1 <- cbind(coef_results, results_ci)


month_hurdle_fn <- function(month, n) {
      # Fit a hurdle model to the data.
      model <- hurdle(
            month ~ exposure | exposure + age + sex,
            data = matched_data,
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

all_results <- bind_rows(
      month_hurdle_fn(matched_data$all_month1, 1),
      month_hurdle_fn(matched_data$all_month2, 2),
      month_hurdle_fn(matched_data$all_month2, 3),
      month_hurdle_fn(matched_data$all_month2, 4),
      month_hurdle_fn(matched_data$all_month2, 5),
      month_hurdle_fn(matched_data$all_month2, 6),
      month_hurdle_fn(matched_data$all_month2, 7),
      month_hurdle_fn(matched_data$all_month2, 8),
      month_hurdle_fn(matched_data$all_month2, 9),
      month_hurdle_fn(matched_data$all_month2, 10),
      month_hurdle_fn(matched_data$all_month2, 11),
      month_hurdle_fn(matched_data$all_month2, 12),
)

all_results %>% 
      write.csv(here("output", "st03_monthly_visits_crude_hurdle.csv"), 
                row.names = T)