# Load previous data management
source("analysis/dm03_5_matched_pivot_long.R")

# Data management for modeling:: --------
# Collapsing data by summarising the visits and follow-up time, and 
# generate three datasets for follow-up 3m, 6m, and 12m

# # 3 months
matched_data_3m <- matched_data_ts %>% 
      filter(month %in% c(1,2,3)& !is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_visits),
            follow_up = sum(follow_up_time)) %>% 
      ungroup()

# # 6 months
matched_data_6m <- matched_data_ts %>% 
      filter(month %in% c(1,2,3,4,5,6)& !is.na(follow_up_time)) %>% 
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

# Stats: two part (Hurdle) model -----
# first need to exclude rows with NA and create 1/0 outcomes:
crude_vars <- c("visits", "exposure", "follow_up")#for crude anaylsis

crude_complete_3m <- matched_data_3m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))
crude_complete_6m <- matched_data_6m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))
crude_complete_12m <- matched_data_12m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Crude Hurdle model: ------

# # Manually calculation using vglm
# # Binomial model: 
# crude_binomial_3m <- glm(visits_binary ~ exposure + offset(log(follow_up)), data = crude_complete_3m,
#                          family=binomial(link="logit")) 
# 
# # Truncated negative binomial model:
# crude_zt_nb_3m <- vglm(visits ~ exposure + offset(log(follow_up)), 
#                        family = posnegbinomial(),
#                        data = subset(crude_complete_3m, visits_binary > 0))

# Crude Hurdle model using pscl package: 
crude_hurdle_3m<- hurdle(visits ~ exposure, 
                         offset = log(follow_up),
                         data = crude_complete_3m,
                         zero.dist = "binomial",
                         dist = "negbin")

crude_hurdle_6m<- hurdle(visits ~ exposure + offset(log(follow_up)), 
                         offset = log(follow_up),
                         data = crude_complete_6m,
                         zero.dist = "binomial",
                         dist = "negbin")

crude_hurdle_12m<- hurdle(visits ~ exposure + offset(log(follow_up)), 
                         offset = log(follow_up),
                         data = crude_complete_12m,
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

crude_output_hurdle <- bind_rows(
      organise_reg_output_fn(crude_hurdle_3m, "3 month"),
      organise_reg_output_fn(crude_hurdle_6m, "6 month"),
      organise_reg_output_fn(crude_hurdle_12m, "12 month")
) %>% mutate(model = "crude")


# Adjusted hurdle model: 
# First need to clean the data by excluding obs with NA in variables:
adj_complete_3m <- matched_data_3m[complete.cases(matched_data_3m),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))
adj_complete_6m <- matched_data_6m[complete.cases(matched_data_6m),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))
adj_complete_12m <- matched_data_12m[complete.cases(matched_data_12m),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Run the adjusted model using the complete data:

adj_hurdle_3m <- hurdle(visits ~ exposure + age_cat + sex  + cov_covid_vax_n_cat + 
                        bmi_cat + imd_q5 + ethnicity_6 + region + number_comorbidities_cat,
                        offset = log(follow_up),
                        data = adj_complete_3m,
                        zero.dist = "binomial",
                        dist = "negbin")

adj_hurdle_6m <- hurdle(visits ~ exposure + age_cat + sex  + cov_covid_vax_n_cat + 
                             bmi_cat + imd_q5 + ethnicity_6 + region + number_comorbidities_cat,
                       offset = log(follow_up),
                       data = adj_complete_6m,
                       zero.dist = "binomial",
                       dist = "negbin")


adj_hurdle_12m <- hurdle(visits ~ exposure + age_cat + sex  + cov_covid_vax_n_cat + 
                              bmi_cat + imd_q5 + ethnicity_6 + region + number_comorbidities_cat,
                        offset = log(follow_up),
                        data = adj_complete_12m,
                        zero.dist = "binomial",
                        dist = "negbin")

# Combine outputs
adj_output_hurdle <- bind_rows(
      organise_reg_output_fn(adj_hurdle_3m, "3 month"),
      organise_reg_output_fn(adj_hurdle_6m, "6 month"),
      organise_reg_output_fn(adj_hurdle_12m, "12 month")
) %>% mutate(model = "Adjusted")

# Save both outputs
bind_rows(crude_output_hurdle,adj_output_hurdle) %>% 
      write_csv(here("output", "st03_01_hurdle_all_visits.csv"))


