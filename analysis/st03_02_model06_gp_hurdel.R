# Load previous data management
source("analysis/dm03_6_pivot_gp_long.R")

# Data management for modeling:: --------
# Collapsing data by summarising the visits and follow-up time, and 
# generate three datasets for follow-up 3m, 6m, and 12m
matched_data_gp_ts$month %>% table
# # 3 months
matched_data_gp_3m <- matched_data_gp_ts %>% 
  filter(month %in% c(1,2,3) & !is.na(follow_up_time)) %>% 
  group_by(patient_id, exposure) %>% 
  summarise(
    visits = sum(monthly_gp_visits),
    follow_up = sum(follow_up_time)) %>% 
  ungroup()

# # 6 months
matched_data_gp_6m <- matched_data_gp_ts %>% 
  filter(month %in% c(1,2,3,4,5,6) & !is.na(follow_up_time)) %>% 
  group_by(patient_id, exposure) %>% 
  summarise(
    visits = sum(monthly_gp_visits),
    follow_up = sum(follow_up_time)) %>% 
  ungroup()

# follow 12 months 
matched_data_gp_12m <- matched_data_gp_ts %>% 
  filter(!is.na(follow_up_time)) %>% 
  group_by(patient_id, exposure) %>% 
  summarise(
    visits = sum(monthly_gp_visits),
    follow_up = sum(follow_up_time)) %>% 
  ungroup()


# # Add covariates for adjustment
for_covariates <- matched_data_gp_ts %>% distinct(patient_id, exposure, .keep_all = T) %>% 
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
matched_data_gp_3m <- left_join(matched_data_gp_3m, for_covariates,
                                by = c("patient_id" = "patient_id", "exposure" = "exposure"))

matched_data_gp_6m <- left_join(matched_data_gp_6m, for_covariates,
                                by = c("patient_id" = "patient_id", "exposure" = "exposure"))

matched_data_gp_12m <- left_join(matched_data_gp_12m, for_covariates,
                                 by = c("patient_id" = "patient_id", "exposure" = "exposure"))

# correct the level of exposure groups
matched_data_gp_3m$exposure <- relevel(matched_data_gp_3m$exposure, ref = "Comparator")
matched_data_gp_6m$exposure <- relevel(matched_data_gp_6m$exposure, ref = "Comparator")
matched_data_gp_12m$exposure <- relevel(matched_data_gp_12m$exposure, ref = "Comparator")


# Stats: two part (Hurdle) model -----
# first need to exclude rows with NA and create 1/0 outcomes:
crude_vars <- c("visits", "exposure", "follow_up")#for crude anaylsis

crude_complete_gp_3m <- matched_data_gp_3m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))
crude_complete_gp_6m <- matched_data_gp_6m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))
crude_complete_gp_12m <- matched_data_gp_12m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Crude Hurdle model: ------

# Crude Hurdle model using pscl package: 
crude_gp_hurdle_3m<- hurdle(visits ~ exposure, 
                         offset = log(follow_up),
                         data = crude_complete_gp_3m,
                         zero.dist = "binomial",
                         dist = "negbin")

crude_gp_hurdle_6m<- hurdle(visits ~ exposure + offset(log(follow_up)), 
                         offset = log(follow_up),
                         data = crude_complete_gp_6m,
                         zero.dist = "binomial",
                         dist = "negbin")

crude_gp_hurdle_12m<- hurdle(visits ~ exposure + offset(log(follow_up)), 
                         offset = log(follow_up),
                         data = crude_complete_gp_12m,
                         zero.dist = "binomial",
                         dist = "negbin")

# Write a function to organise the regression output.
organise_reg_output_fn <- function(reg_model, model_time){
      estimate <- reg_model %>% coef() %>% exp() %>% as.data.frame()
      estimate$estimate <- rownames(estimate)  # get the coeficient
      
      e_ci <- reg_model %>% confint() %>% exp %>% as.data.frame()
      e_ci$estimate <- rownames(e_ci) # get the ci
      
      output <- inner_join(estimate, e_ci, by = c("estimate"="estimate")) %>% 
            rename("rr" = ".") %>% relocate(estimate) %>% 
            filter(estimate != "count_(Intercept)" & estimate != "zero_(Intercept)") %>% 
            mutate(time = model_time) %>% relocate(time)
      return(output)
}

crude_gp_output_hurdle <- bind_rows(
      organise_reg_output_fn(crude_gp_hurdle_3m, "3 month"),
      organise_reg_output_fn(crude_gp_hurdle_6m, "6 month"),
      organise_reg_output_fn(crude_gp_hurdle_12m, "12 month")
) %>% mutate(model = "crude GP")


# Adjusted hurdle model: 
# First need to clean the data by excluding obs with NA in variables:
adj_gp_complete_3m <- matched_data_gp_3m[complete.cases(matched_data_gp_3m),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))
adj_gp_complete_6m <- matched_data_gp_6m[complete.cases(matched_data_gp_6m),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))
adj_gp_complete_12m <- matched_data_gp_12m[complete.cases(matched_data_gp_12m),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Run the adjusted model using the complete data:

adj_gp_hurdle_3m <- hurdle(visits ~ exposure + age_cat + sex  + cov_covid_vax_n_cat + 
                        bmi_cat + imd_q5 + ethnicity_6 + region + number_comorbidities_cat,
                        offset = log(follow_up),
                        data = adj_gp_complete_3m,
                        zero.dist = "binomial",
                        dist = "negbin")

adj_gp_hurdle_6m <- hurdle(visits ~ exposure + age_cat + sex  + cov_covid_vax_n_cat + 
                             bmi_cat + imd_q5 + ethnicity_6 + region + number_comorbidities_cat,
                       offset = log(follow_up),
                       data = adj_gp_complete_6m,
                       zero.dist = "binomial",
                       dist = "negbin")


adj_gp_hurdle_12m <- hurdle(visits ~ exposure + age_cat + sex  + cov_covid_vax_n_cat + 
                              bmi_cat + imd_q5 + ethnicity_6 + region + number_comorbidities_cat,
                        offset = log(follow_up),
                        data = adj_gp_complete_12m,
                        zero.dist = "binomial",
                        dist = "negbin")

# Combine outputs
adj_gp_output_hurdle <- bind_rows(
      organise_reg_output_fn(adj_gp_hurdle_3m, "3 month"),
      organise_reg_output_fn(adj_gp_hurdle_6m, "6 month"),
      organise_reg_output_fn(adj_gp_hurdle_12m, "12 month")
) %>% mutate(model = "Adjusted GP")

# Save both outputs
bind_rows(crude_gp_output_hurdle,adj_gp_output_hurdle) %>% 
      write_csv(here("output", "st03_02_hurdle_gp_visits.csv"))


