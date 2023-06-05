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


# A.
# A-1. Crude results: overall ----
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
      

# # A-2. Crude results: stratified by previous hospitalisation -----
# # COVID hospitalised people:
# hospital_crude_results <- bind_rows(
#       month_hurdle_fn(hospital$all_month1, 1, data = hospital, matched_data$follow_up_m1),
#       month_hurdle_fn(hospital$all_month2, 2, data = hospital, matched_data$follow_up_m2),
#       month_hurdle_fn(hospital$all_month3, 3, data = hospital, matched_data$follow_up_m3),
#       month_hurdle_fn(hospital$all_month4, 4, data = hospital, matched_data$follow_up_m4),
#       month_hurdle_fn(hospital$all_month5, 5, data = hospital, matched_data$follow_up_m5),
#       month_hurdle_fn(hospital$all_month6, 6, data = hospital, matched_data$follow_up_m6),
#       month_hurdle_fn(hospital$all_month7, 7, data = hospital, matched_data$follow_up_m7),
#       month_hurdle_fn(hospital$all_month8, 8, data = hospital, matched_data$follow_up_m8),
#       month_hurdle_fn(hospital$all_month9, 9, data = hospital, matched_data$follow_up_m9),
#       month_hurdle_fn(hospital$all_month10, 10, data = hospital, matched_data$follow_up_m10),
#       month_hurdle_fn(hospital$all_month11, 11, data = hospital, matched_data$follow_up_m11),
#       month_hurdle_fn(hospital$all_month12, 12, data = hospital, matched_data$follow_up_m12),
# )
# hospital_crude_results %>% 
#       filter(rownames(.) %>% startsWith("count_")) %>% 
#       write.csv(here("output", "st03_monthly_visits_crude_hurdle_hospital.csv"), 
#                 row.names = T)
# 
# # No hospitalisation due to COVID:
# no_hos_crude_results<- bind_rows(
#       month_hurdle_fn(non_hospital$all_month1, 1, data = non_hospital),
#       month_hurdle_fn(non_hospital$all_month2, 2, data = non_hospital),
#       month_hurdle_fn(non_hospital$all_month3, 3, data = non_hospital),
#       month_hurdle_fn(non_hospital$all_month4, 4, data = non_hospital),
#       month_hurdle_fn(non_hospital$all_month5, 5, data = non_hospital),
#       month_hurdle_fn(non_hospital$all_month6, 6, data = non_hospital),
#       month_hurdle_fn(non_hospital$all_month7, 7, data = non_hospital),
#       month_hurdle_fn(non_hospital$all_month8, 8, data = non_hospital),
#       month_hurdle_fn(non_hospital$all_month9, 9, data = non_hospital),
#       month_hurdle_fn(non_hospital$all_month10, 10, data = non_hospital),
#       month_hurdle_fn(non_hospital$all_month11, 11, data = non_hospital),
#       month_hurdle_fn(non_hospital$all_month12, 12, data = non_hospital),
# )
# no_hos_crude_results%>% 
#       filter(rownames(.) %>% startsWith("count_")) %>% 
#       write.csv(here("output", "st03_monthly_visits_crude_hurdle_non_hospital.csv"), 
#                 row.names = T)

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

# B-1. Adjusted results: overall ---------
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
# # B-2 Hospitalisation: -----
# # COVID hospitalised people:
# hospital_par_adj_results <- bind_rows(
#       month_adj_hurdle_fn(hospital$all_month1, 1, data = hospital),
#       month_adj_hurdle_fn(hospital$all_month2, 2, data = hospital),
#       month_adj_hurdle_fn(hospital$all_month3, 3, data = hospital),
#       month_adj_hurdle_fn(hospital$all_month4, 4, data = hospital),
#       month_adj_hurdle_fn(hospital$all_month5, 5, data = hospital),
#       month_adj_hurdle_fn(hospital$all_month6, 6, data = hospital),
#       month_adj_hurdle_fn(hospital$all_month7, 7, data = hospital),
#       month_adj_hurdle_fn(hospital$all_month8, 8, data = hospital),
#       month_adj_hurdle_fn(hospital$all_month9, 9, data = hospital),
#       month_adj_hurdle_fn(hospital$all_month10, 10, data = hospital),
#       month_adj_hurdle_fn(hospital$all_month11, 11, data = hospital),
#       month_adj_hurdle_fn(hospital$all_month12, 12, data = hospital),
# )
# hospital_par_adj_results %>% 
#       filter(rownames(.) %>% startsWith("count_")) %>% 
#       write.csv(here("output", "st03_monthly_visits_par_adj_hurdle_hospital.csv"), 
#                 row.names = T)
# 
# 
# # No hospitalisation due to COVID:
# no_hos_par_adj_results<- bind_rows(
#       month_adj_hurdle_fn(non_hospital$all_month1, 1, data = non_hospital),
#       month_adj_hurdle_fn(non_hospital$all_month2, 2, data = non_hospital),
#       month_adj_hurdle_fn(non_hospital$all_month3, 3, data = non_hospital),
#       month_adj_hurdle_fn(non_hospital$all_month4, 4, data = non_hospital),
#       month_adj_hurdle_fn(non_hospital$all_month5, 5, data = non_hospital),
#       month_adj_hurdle_fn(non_hospital$all_month6, 6, data = non_hospital),
#       month_adj_hurdle_fn(non_hospital$all_month7, 7, data = non_hospital),
#       month_adj_hurdle_fn(non_hospital$all_month8, 8, data = non_hospital),
#       month_adj_hurdle_fn(non_hospital$all_month9, 9, data = non_hospital),
#       month_adj_hurdle_fn(non_hospital$all_month10, 10, data = non_hospital),
#       month_adj_hurdle_fn(non_hospital$all_month11, 11, data = non_hospital),
#       month_adj_hurdle_fn(non_hospital$all_month12, 12, data = non_hospital),
# )
# no_hos_par_adj_results%>% 
#       filter(rownames(.) %>% startsWith("count_")) %>% 
#       write.csv(here("output", "st03_monthly_visits_par_adj_hurdle_non_hospital.csv"), 
#                 row.names = T)
# 

# Fully adjusted model 
# Fully adjusted model Hurdle model function: 
month_full_hurdle_fn <- function(month, n, data, fu_time) {
      # Fit a hurdle model to the data.
      model <- hurdle(
            month ~ exposure + age_cat + sex + region + ethnicity_6 + 
                  imd_q5 + cov_covid_vax_n_cat + bmi_cat + 
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

independent <- c("exposure", "age_cat", "sex", "region", "ethnicity_6", "imd_q5",
                 "cov_covid_vax_n_cat", "bmi_cat", "number_comorbidities_cat")

lapply(matched_data[independent], function(x){table(is.na(x))})
lapply(matched_data[independent], function(x){table(x, useNA = "ifany")})
matched_data_no_na <- na.omit(matched_data, cols = independent)
matched_data_no_na$number_comorbidities_cat%>% table
# C-1 Fully adjusted model: all
all_full_adj_results <- bind_rows(
      month_full_hurdle_fn(matched_data_no_na$all_month1, 1, data = matched_data_no_na, matched_data_no_na$follow_up_m1),
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

all_full_adj_results %>% 
      filter(rownames(.) %>% startsWith("count_")) %>% 
      write.csv(here("output", "st03_monthly_visits_full_adj_hurdle.csv"), 
                row.names = T)
# 
# # C-2 Hospitalisation: -----
# # COVID hospitalised people:
# hospital_full_adj_results <- bind_rows(
#       month_full_hurdle_fn(hospital$all_month1, 1, data = hospital),
#       month_full_hurdle_fn(hospital$all_month2, 2, data = hospital),
#       month_full_hurdle_fn(hospital$all_month3, 3, data = hospital),
#       month_full_hurdle_fn(hospital$all_month4, 4, data = hospital),
#       month_full_hurdle_fn(hospital$all_month5, 5, data = hospital),
#       month_full_hurdle_fn(hospital$all_month6, 6, data = hospital),
#       month_full_hurdle_fn(hospital$all_month7, 7, data = hospital),
#       month_full_hurdle_fn(hospital$all_month8, 8, data = hospital),
#       month_full_hurdle_fn(hospital$all_month9, 9, data = hospital),
#       month_full_hurdle_fn(hospital$all_month10, 10, data = hospital),
#       month_full_hurdle_fn(hospital$all_month11, 11, data = hospital),
#       month_full_hurdle_fn(hospital$all_month12, 12, data = hospital),
# )
# hospital_full_adj_results %>% 
#       filter(rownames(.) %>% startsWith("count_")) %>% 
#       write.csv(here("output", "st03_monthly_visits_full_adj_hurdle_hospital.csv"), 
#                 row.names = T)
# 
# 
# # No hospitalisation due to COVID:
# no_hos_full_adj_results<- bind_rows(
#       month_full_hurdle_fn(non_hospital$all_month1, 1, data = non_hospital),
#       month_full_hurdle_fn(non_hospital$all_month2, 2, data = non_hospital),
#       month_full_hurdle_fn(non_hospital$all_month3, 3, data = non_hospital),
#       month_full_hurdle_fn(non_hospital$all_month4, 4, data = non_hospital),
#       month_full_hurdle_fn(non_hospital$all_month5, 5, data = non_hospital),
#       month_full_hurdle_fn(non_hospital$all_month6, 6, data = non_hospital),
#       month_full_hurdle_fn(non_hospital$all_month7, 7, data = non_hospital),
#       month_full_hurdle_fn(non_hospital$all_month8, 8, data = non_hospital),
#       month_full_hurdle_fn(non_hospital$all_month9, 9, data = non_hospital),
#       month_full_hurdle_fn(non_hospital$all_month10, 10, data = non_hospital),
#       month_full_hurdle_fn(non_hospital$all_month11, 11, data = non_hospital),
#       month_full_hurdle_fn(non_hospital$all_month12, 12, data = non_hospital),
# )
# no_hos_full_adj_results%>% 
#       filter(rownames(.) %>% startsWith("count_")) %>% 
#       write.csv(here("output", "st03_monthly_visits_full_adj_hurdle_non_hospital.csv"), 
#                 row.names = T)
