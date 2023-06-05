# Load previous data management
source("analysis/dm03_matched_define_monthly_follow_up_time.R")

# For organising the outputs
options(digits=2)

# Crude Hurdle model function: 
month_hurdle_fn <- function(month, n, data, fu_time) {
      # Fit a hurdle model to the data.
      model <- hurdle(
            month ~ exposure + offset(fu_time) | age_cat + sex+ region,
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


# A. Crude results: overall ----
all_results <- bind_rows(
      month_hurdle_fn(matched_data$all_month1, 1, data = matched_data, matched_data$follow_up_m1),
      month_hurdle_fn(matched_data$all_month2, 2, data = matched_data, matched_data$follow_up_m2),
      month_hurdle_fn(matched_data$all_month3, 3, data = matched_data, matched_data$follow_up_m3),
      month_hurdle_fn(matched_data$all_month4, 4, data = matched_data, matched_data$follow_up_m4),
      month_hurdle_fn(matched_data$all_month5, 5, data = matched_data, matched_data$follow_up_m5),
      month_hurdle_fn(matched_data$all_month6, 6, data = matched_data, matched_data$follow_up_m6),
      month_hurdle_fn(matched_data$all_month7, 7, data = matched_data, matched_data$follow_up_m7),
      month_hurdle_fn(matched_data$all_month8, 8, data = matched_data, matched_data$follow_up_m8),
      month_hurdle_fn(matched_data$all_month9, 9, data = matched_data, matched_data$follow_up_m9),
      month_hurdle_fn(matched_data$all_month10, 10, data = matched_data, matched_data$follow_up_m10),
      month_hurdle_fn(matched_data$all_month11, 11, data = matched_data, matched_data$follow_up_m11),
      month_hurdle_fn(matched_data$all_month12, 12, data = matched_data, matched_data$follow_up_m12),
)

# organise the output: keep only the exposure, and label the model.

crude <- all_results %>% 
      filter(rownames(.) %>% startsWith("count_exposure")) %>% 
      mutate(model = "Crude") %>% 
      relocate(model) %>% relocate(month, .after = model) %>% tibble::remove_rownames()
      

# B. Adjusted model

# Partially adjusted Hurdle model function: 
month_adj_hurdle_fn <- function(month, n, data, fu_time) {
      # Fit a hurdle model to the data.
      model <- hurdle(
            month ~ exposure + age_cat + sex + offset(fu_time)| age_cat + sex+ region,
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

# B. Adjusted results: overall ---------
all_par_adj_results <- bind_rows(
      month_adj_hurdle_fn(matched_data$all_month1, 1, data = matched_data, matched_data$follow_up_m1),
      month_adj_hurdle_fn(matched_data$all_month2, 2, data = matched_data, matched_data$follow_up_m2),
      month_adj_hurdle_fn(matched_data$all_month3, 3, data = matched_data, matched_data$follow_up_m3),
      month_adj_hurdle_fn(matched_data$all_month4, 4, data = matched_data, matched_data$follow_up_m4),
      month_adj_hurdle_fn(matched_data$all_month5, 5, data = matched_data, matched_data$follow_up_m5),
      month_adj_hurdle_fn(matched_data$all_month6, 6, data = matched_data, matched_data$follow_up_m6),
      month_adj_hurdle_fn(matched_data$all_month7, 7, data = matched_data, matched_data$follow_up_m7),
      month_adj_hurdle_fn(matched_data$all_month8, 8, data = matched_data, matched_data$follow_up_m8),
      month_adj_hurdle_fn(matched_data$all_month9, 9, data = matched_data, matched_data$follow_up_m9),
      month_adj_hurdle_fn(matched_data$all_month10, 10, data = matched_data, matched_data$follow_up_m10),
      month_adj_hurdle_fn(matched_data$all_month11, 11, data = matched_data, matched_data$follow_up_m11),
      month_adj_hurdle_fn(matched_data$all_month12, 12, data = matched_data, matched_data$follow_up_m12),
)


par_adj <- all_par_adj_results %>% 
      filter(rownames(.) %>% startsWith("count_exposure")) %>% 
      mutate(model = "Partially adjusted") %>% 
      relocate(model) %>% relocate(month, .after = model) %>% tibble::remove_rownames()

# C. Fully adjusted model  ---------
# Fully adjusted model Hurdle model function: 
month_full_hurdle_fn <- function(month, n, data, fu_time) {
      # Fit a hurdle model to the data.
      model <- hurdle(
            month ~ exposure + age_cat + sex  + cov_covid_vax_n_cat + bmi_cat + imd_q5 + ethnicity_6 + #region + 
                  number_comorbidities_cat + offset(fu_time) | age_cat + sex+ region,
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

# C-1 Fully adjusted model: all
all_full_adj_results <- bind_rows(
      month_full_hurdle_fn(matched_data$all_month1, 1, data = matched_data, matched_data$follow_up_m1),
      month_full_hurdle_fn(matched_data$all_month2, 2, data = matched_data, matched_data$follow_up_m2),
      month_full_hurdle_fn(matched_data$all_month3, 3, data = matched_data, matched_data$follow_up_m3),
      month_full_hurdle_fn(matched_data$all_month4, 4, data = matched_data, matched_data$follow_up_m4),
      month_full_hurdle_fn(matched_data$all_month5, 5, data = matched_data, matched_data$follow_up_m5),
      month_full_hurdle_fn(matched_data$all_month6, 6, data = matched_data, matched_data$follow_up_m6),
      month_full_hurdle_fn(matched_data$all_month7, 7, data = matched_data, matched_data$follow_up_m7),
      month_full_hurdle_fn(matched_data$all_month8, 8, data = matched_data, matched_data$follow_up_m8),
      month_full_hurdle_fn(matched_data$all_month9, 9, data = matched_data, matched_data$follow_up_m9),
      month_full_hurdle_fn(matched_data$all_month10, 10, data = matched_data, matched_data$follow_up_m10),
      month_full_hurdle_fn(matched_data$all_month11, 11, data = matched_data, matched_data$follow_up_m11),
      month_full_hurdle_fn(matched_data$all_month12, 12, data = matched_data, matched_data$follow_up_m12),
)

full_adj <- all_full_adj_results %>% 
      filter(rownames(.) %>% startsWith("count_exposure")) %>% 
      mutate(model = "Fully adjusted") %>% 
      relocate(model) %>% relocate(month, .after = model) %>% tibble::remove_rownames()


# Final output: ------
results_hurdle <- bind_rows(crude, par_adj, full_adj)
results_hurdle %>% write.csv(here("output", "st_03_result_monthly_visit_hurdle.csv"), row.names = F)
