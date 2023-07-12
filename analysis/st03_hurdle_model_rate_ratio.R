# Load previous data management
source("analysis/dm03_5_matched_pivot_long.R")
matched_data_ts %>% names

# For organising the outputs
options(digits=2)

# 0. Model comparison:
poisson_hurdle <- hurdle(
      monthly_visits ~ exposure + offset(log(follow_up_time)) | age_cat + sex+ region,
      data = matched_data_ts,
      zero.dist = "binomial",
      dist = "poisson"
)

neg_bionimal_hurdle <- hurdle(
      monthly_visits ~ exposure + offset(log(follow_up_time)) | age_cat + sex+ region,
      data = matched_data_ts,
      zero.dist = "binomial",
      dist = "negbin"
)


data.frame(
      aic_poisson = AIC(poisson_hurdle),
      aic_negbin = AIC(neg_bionimal_hurdle)
) %>% write.csv(here("output", "sup_st03_0_model_comparison.csv"))



# Crude Hurdle model function:
month_hurdle_fn <- function( n, data) {
      # Fit a hurdle model to the data.
      model <- hurdle(
            monthly_visits ~ exposure + offset(log(follow_up_time)) | age_cat + sex+ region,
            data = data,
            zero.dist = "binomial",
            dist = "negbin"
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
      results <- results %>% rownames_to_column("stat_name") %>% 
            filter(str_detect(stat_name, "count_exposure"))
      # Return the results.
      return(results)
}



matched_data_ts[matched_data_ts$month ==1,]


month_hurdle_fn(1,matched_data_ts[matched_data_ts$month ==1,]) 
month_hurdle_fn(1,matched_data_ts[matched_data_ts$month ==1:2,]) 

# A. Crude results: overall ----
all_results <- bind_rows(
      month_hurdle_fn(1, matched_data_ts[matched_data_ts$month == 1,]), 
      month_hurdle_fn(2, matched_data_ts[matched_data_ts$month == 1:2,]),
      month_hurdle_fn(3, matched_data_ts[matched_data_ts$month == 1:3,]), 
      month_hurdle_fn(4, matched_data_ts[matched_data_ts$month == 1:4,]),
      month_hurdle_fn(5, matched_data_ts[matched_data_ts$month == 1:5,]),
      month_hurdle_fn(6, matched_data_ts[matched_data_ts$month == 1:6,]),
      month_hurdle_fn(7, matched_data_ts[matched_data_ts$month == 1:7,]),
      month_hurdle_fn(8, matched_data_ts[matched_data_ts$month == 1:8,]),
      month_hurdle_fn(9, matched_data_ts[matched_data_ts$month == 1:9,]),
      month_hurdle_fn(10, matched_data_ts[matched_data_ts$month == 1:10,]),
      month_hurdle_fn(11, matched_data_ts[matched_data_ts$month == 1:11,]),
      month_hurdle_fn(12, matched_data_ts[matched_data_ts$month == 1:12,])
)


# organise the output: keep only the exposure, and label the model.

crude <- all_results %>%
      mutate(model = "Crude") %>%
      relocate(model) %>% relocate(month, .after = model) %>%
      tibble::remove_rownames() %>% 
      dplyr::select("model", "month", "Coefficients", "Lower CI","Upper CI")


# B. Adjusted model

# Partially adjusted Hurdle model function:
month_adj_hurdle_fn <- function(n, data) {
      # Fit a hurdle model to the data.
      model <- hurdle(
            monthly_visits ~ exposure + age_cat + sex + offset(log(follow_up_time))| age_cat + sex+ region,
            data = data,
            zero.dist = "binomial",
            dist = "negbin"
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
      results <- results %>% rownames_to_column("stat_name") %>% 
            filter(str_detect(stat_name, "count_exposure"))
      # Return the results.
      return(results)
}

# B. Adjusted results: overall ---------
all_par_adj_results <- bind_rows(
      month_adj_hurdle_fn(1, matched_data_ts[matched_data_ts$month == 1,]), 
      month_adj_hurdle_fn(2, matched_data_ts[matched_data_ts$month == 1:2,]),
      month_adj_hurdle_fn(3, matched_data_ts[matched_data_ts$month == 1:3,]), 
      month_adj_hurdle_fn(4, matched_data_ts[matched_data_ts$month == 1:4,]),
      month_adj_hurdle_fn(5, matched_data_ts[matched_data_ts$month == 1:5,]),
      month_adj_hurdle_fn(6, matched_data_ts[matched_data_ts$month == 1:6,]),
      month_adj_hurdle_fn(7, matched_data_ts[matched_data_ts$month == 1:7,]),
      month_adj_hurdle_fn(8, matched_data_ts[matched_data_ts$month == 1:8,]),
      month_adj_hurdle_fn(9, matched_data_ts[matched_data_ts$month == 1:9,]),
      month_adj_hurdle_fn(10, matched_data_ts[matched_data_ts$month == 1:10,]),
      month_adj_hurdle_fn(11, matched_data_ts[matched_data_ts$month == 1:11,]),
      month_adj_hurdle_fn(12, matched_data_ts[matched_data_ts$month == 1:12,])
)


par_adj <- all_par_adj_results %>%
      mutate(model = "Partially adjusted") %>%
      relocate(model) %>% relocate(month, .after = model) %>%
      tibble::remove_rownames() %>% 
      dplyr::select("model", "month", "Coefficients", "Lower CI","Upper CI")

# C. Fully adjusted model  ---------
# Fully adjusted model Hurdle model function:
month_full_hurdle_fn <- function(n, data) {
      # Fit a hurdle model to the data.
      model <- hurdle(
            monthly_visits ~ exposure + age_cat + sex  + cov_covid_vax_n_cat + bmi_cat + imd_q5 + ethnicity_6 + region +
                  number_comorbidities_cat + offset(log(follow_up_time)) | age_cat + sex+ region,
            data = data,
            zero.dist = "binomial",
            dist = "negbin"
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
      results <- results %>% rownames_to_column("stat_name") %>% 
            filter(str_detect(stat_name, "count_exposure"))
      # Return the results.
      return(results)
}

# C-1 Fully adjusted model: all
all_full_adj_results <- bind_rows(
      month_full_hurdle_fn(1, matched_data_ts[matched_data_ts$month == 1,]), 
      month_full_hurdle_fn(2, matched_data_ts[matched_data_ts$month == 1:2,]),
      month_full_hurdle_fn(3, matched_data_ts[matched_data_ts$month == 1:3,]), 
      month_full_hurdle_fn(4, matched_data_ts[matched_data_ts$month == 1:4,]),
      month_full_hurdle_fn(5, matched_data_ts[matched_data_ts$month == 1:5,]),
      month_full_hurdle_fn(6, matched_data_ts[matched_data_ts$month == 1:6,]),
      month_full_hurdle_fn(7, matched_data_ts[matched_data_ts$month == 1:7,]),
      month_full_hurdle_fn(8, matched_data_ts[matched_data_ts$month == 1:8,]),
      month_full_hurdle_fn(9, matched_data_ts[matched_data_ts$month == 1:9,]),
      month_full_hurdle_fn(10, matched_data_ts[matched_data_ts$month == 1:10,]),
      month_full_hurdle_fn(11, matched_data_ts[matched_data_ts$month == 1:11,]),
      month_full_hurdle_fn(12, matched_data_ts[matched_data_ts$month == 1:12,])
)

full_adj <- all_full_adj_results %>%
      mutate(model = "Fully adjusted") %>%
      relocate(model) %>% relocate(month, .after = model) %>%
      tibble::remove_rownames() %>% 
      dplyr::select("model", "month", "Coefficients", "Lower CI","Upper CI")



# Final output: ------
results_hurdle <- bind_rows(crude, par_adj, full_adj)
results_hurdle %>% write.csv(here("output", "st_03_result_monthly_visit_hurdle.csv"), row.names = F)
