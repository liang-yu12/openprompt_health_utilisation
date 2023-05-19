# Load previous data management
source("analysis/dm03_matched_define_monthly_follow_up_time.R")

# For organising the outputs
options(digits=2)

# Fit hurdle models: 
# The first part: counted data regression (truncated Poisson)
# The second part: the zero regression (logit)

# Crude: only look at the association between exposure and outcome


crude_month_hurdle_fn <- function(month, n) {
      # Fit a hurdle model to the data.
      model <- hurdle(
            month ~ exposure | age + sex + region,
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
      crude_month_hurdle_fn(matched_data$all_month1, 1),
      crude_month_hurdle_fn(matched_data$all_month2, 2),
      crude_month_hurdle_fn(matched_data$all_month2, 3),
      crude_month_hurdle_fn(matched_data$all_month2, 4),
      crude_month_hurdle_fn(matched_data$all_month2, 5),
      crude_month_hurdle_fn(matched_data$all_month2, 6),
      crude_month_hurdle_fn(matched_data$all_month2, 7),
      crude_month_hurdle_fn(matched_data$all_month2, 8),
      crude_month_hurdle_fn(matched_data$all_month2, 9),
      crude_month_hurdle_fn(matched_data$all_month2, 10),
      crude_month_hurdle_fn(matched_data$all_month2, 11),
      crude_month_hurdle_fn(matched_data$all_month2, 12),
) %>% tibble::rownames_to_column(var = "Variables") %>% 
      filter(grepl("count_", Variables)) %>% 
      filter(grepl("exposure", Variables)) %>% 
      relocate(month, .after = "Variables")
