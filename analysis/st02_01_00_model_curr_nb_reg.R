# Load previous data management
source("analysis/dm03_5_matched_pivot_long.R")

# Data management for modeling:: --------
# Collapsing data by summarising the visits and follow-up time, and 
# generate three datasets for follow-up 3m, 6m, and 12m

# # 3 months
matched_data_3m <- matched_data_ts %>% 
      filter(month %in% c(1,2,3) & !is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_visits),
            follow_up = sum(follow_up_time)) %>% 
      ungroup()

# # 6 months
matched_data_6m <- matched_data_ts %>% 
      filter(month %in% c(1,2,3,4,5,6) & !is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_visits),
            follow_up = sum(follow_up_time)) %>% 
      ungroup()

# follow 12 months 
matched_data_12m <- matched_data_ts %>% 
      filter(!is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_visits),
            follow_up = sum(follow_up_time)) %>% 
      ungroup()


# # Add covariates for adjustment
for_covariates <- matched_data_ts %>% distinct(patient_id, exposure, .keep_all = T) %>% 
      dplyr::select("patient_id",     
                    "exposure",           
                    "age_cat",                 
                    "sex",                     
                    "bmi_cat",
                    "ethnicity_6",             
                    "imd_q5",                  
                    "region",      
                    "previous_covid_hosp",     
                    "cov_covid_vax_n_cat",     
                    "number_comorbidities_cat")

# # add covariates back to the summarised data frame
matched_data_3m <- left_join(matched_data_3m, for_covariates,
                               by = c("patient_id" = "patient_id", "exposure" = "exposure"))

matched_data_6m <- left_join(matched_data_6m, for_covariates,
                             by = c("patient_id" = "patient_id", "exposure" = "exposure"))

matched_data_12m <- left_join(matched_data_12m, for_covariates,
                             by = c("patient_id" = "patient_id", "exposure" = "exposure"))

# correct the level of exposure groups
matched_data_3m$exposure <- relevel(matched_data_3m$exposure, ref = "Comparator")
matched_data_6m$exposure <- relevel(matched_data_6m$exposure, ref = "Comparator")
matched_data_12m$exposure <- relevel(matched_data_12m$exposure, ref = "Comparator")

# Stats: one part model -----
# # Model 1: crude negative binomial model: -----
# 3m 
nb_crude_3m <- glm.nb(visits ~ exposure + offset(log(follow_up)), 
                   data = matched_data_3m,
                   link = log)

nb_crude_6m <- glm.nb(visits ~ exposure + offset(log(follow_up)), 
                      data = matched_data_6m,
                      link = log)

nb_crude_12m <- glm.nb(visits ~ exposure + offset(log(follow_up)), 
                      data = matched_data_12m,
                      link = log)

# Function to organise the regression outputs
org_reg_results_fn <- function(nb_reg, time) {
      # Calculate the coefficients and confidence intervals
      results <- bind_cols(
            (coef(nb_reg) %>% exp() %>% as.data.frame()),
            (confint(nb_reg) %>% exp() %>% as.data.frame()))
      
      # Set the row names as the variable names
      results$terms <- rownames(results)
      
      # Rename the "." column to "estimates"
      results <- rename(results, estimates = .)
      
      # Add a time frame column
      results <- mutate(results, time_frame =time)
      
      # Rearrange the columns
      results <- relocate(results, time_frame, terms, estimates)
      
      return(results)
}

# Organised and combine crude results
crude_reg_results <- bind_rows(
      org_reg_results_fn(nb_crude_3m, "3 months"),
      org_reg_results_fn(nb_crude_6m, "6 months"),
      org_reg_results_fn(nb_crude_12m, "12 months")
) %>% mutate(model = "Crude") %>% relocate(model)

# # Model 2: adjusted Negative binomial: ------
# 3 months
nb_adj_3m <- glm.nb(visits ~ exposure + offset(log(follow_up)) + 
                       sex + region + age_cat + imd_q5 + ethnicity_6 + bmi_cat +
                       number_comorbidities_cat + previous_covid_hosp + cov_covid_vax_n_cat, 
                 data = matched_data_3m,
                 link = log)

# 6 months
nb_adj_6m <- glm.nb(visits ~ exposure + offset(log(follow_up)) + 
                          sex + region + age_cat + imd_q5 + ethnicity_6 + bmi_cat +
                          number_comorbidities_cat + previous_covid_hosp + cov_covid_vax_n_cat, 
                    data = matched_data_6m,
                    link = log)

# 12 months
nb_adj_12m <- glm.nb(visits ~ exposure + offset(log(follow_up)) + 
                          sex + region + age_cat + imd_q5 + ethnicity_6 + bmi_cat +
                          number_comorbidities_cat + previous_covid_hosp + cov_covid_vax_n_cat, 
                    data = matched_data_12m,
                    link = log)

# Combine results -----
adj_reg_results <- bind_rows(
      org_reg_results_fn(nb_adj_3m, "3 months"),
      org_reg_results_fn(nb_adj_6m, "6 months"),
      org_reg_results_fn(nb_adj_12m, "12 months")
) %>% mutate(model = "Adjusted") %>% relocate(model)

# # write outputs:
bind_rows(crude_reg_results, adj_reg_results) %>% 
      write_csv(here("output", "st_02_non_cluster_model.csv"))
