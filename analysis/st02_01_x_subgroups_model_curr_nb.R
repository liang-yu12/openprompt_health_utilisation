# Load previous data management
source("analysis/dm03_5_matched_pivot_long.R")

# Goal: run the gubgroup analyses by 
# - sex: "female" "male"  
# - age groups: "18-29" "30-39" "40-49" "50-59" "60-69" "70+"  
# - previous hospitalisation:"FALSE" "TRUE" 

# outcome types: 3m, 6m, 12m,


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

# Assign covariate levels before adding them back:
for_covariates$sex <- relevel(for_covariates$sex, ref = "female")
for_covariates$age_cat <- relevel(for_covariates$age_cat, ref = "18-29")
for_covariates$previous_covid_hosp <- relevel(for_covariates$previous_covid_hosp, "FALSE")
for_covariates$number_comorbidities_cat <- as.factor(for_covariates$number_comorbidities_cat)

# # add covariates back to the summarised data frame
matched_data_3m <- left_join(matched_data_3m, for_covariates,
                               by = c("patient_id" = "patient_id", "exposure" = "exposure"))

matched_data_6m <- left_join(matched_data_6m, for_covariates,
                             by = c("patient_id" = "patient_id", "exposure" = "exposure"))

matched_data_12m <- left_join(matched_data_12m, for_covariates,
                             by = c("patient_id" = "patient_id", "exposure" = "exposure"))

# correct the levels of variables
matched_data_3m$exposure <- relevel(matched_data_3m$exposure, ref = "Comparator")
matched_data_6m$exposure <- relevel(matched_data_6m$exposure, ref = "Comparator")
matched_data_12m$exposure <- relevel(matched_data_12m$exposure, ref = "Comparator")

# Stats: Subgroup analyses by different covariates -----
# Use group_by then do(reg), then ungroup. The output from the fo part must be a dataframe


# # Sex: ------
# Function for crude negative binomial models and organise outputs:
crude_sex_subgroup_fn <- function(data, time){

      results <- data %>% group_by(sex) %>%   # by the variable
            do(tidy((glm.nb(visits ~ exposure + offset(log(follow_up)),
                            data = .,link = log)), 
                    exponentiate=TRUE, conf.int=TRUE)) %>% 
            ungroup() %>% mutate(model = time)
            
      return(results)
}

subgroup_crude_sex <- bind_rows(
      crude_sex_subgroup_fn(matched_data_3m, "3 months"),
      crude_sex_subgroup_fn(matched_data_6m, "6 months"),
      crude_sex_subgroup_fn(matched_data_12m, "12 months")
)


# # Age groups: ------


crude_age_subgroup_fn <- function(data, time){
      
      results <- data %>% group_by(age_cat) %>%   # by the variable
            do(tidy((glm.nb(visits ~ exposure + offset(log(follow_up)),
                            data = .,link = log)), 
                    exponentiate=TRUE, conf.int=TRUE)) %>% 
            ungroup() %>% mutate(model = time)
      
      return(results)
}

# # # age_cat "18-29" "30-39" "40-49" "50-59" "60-69" "70+"  
# 3 months

subgroup_crude_age <- bind_rows(
      crude_age_subgroup_fn(matched_data_3m, "3 months"),
      crude_age_subgroup_fn(matched_data_6m, "6 months"),
      crude_age_subgroup_fn(matched_data_12m, "12 months")
      )


# # COVID hospitalisations: ------
# # # previous_covid_hosp: "FALSE" "TRUE" 

crude_hos_subgroup_fn <- function(data, time){
      
      results <- data %>% group_by(previous_covid_hosp) %>%   # by the variable
            do(tidy((glm.nb(visits ~ exposure + offset(log(follow_up)),
                            data = .,link = log)), 
                    exponentiate=TRUE, conf.int=TRUE)) %>% 
            ungroup() %>% mutate(model = time)
      
      return(results)
}


subgroup_crude_hospital <- bind_rows(
      crude_hos_subgroup_fn(matched_data_3m, "3 months"),
      crude_hos_subgroup_fn(matched_data_6m, "6 months"),
      crude_hos_subgroup_fn(matched_data_12m, "12 months")
)

crude_all_sub <- bind_rows(
      subgroup_crude_sex,
      subgroup_crude_age,
      subgroup_crude_hospital) %>% relocate(model) %>% 
      mutate(nb_model = "Crude") %>% relocate(nb_model)

# Model 2: adjusted Negative binomial: ------
# function for sex

adj_sex_subgroup_fn <- function(data, time){
      results <- data %>% group_by(sex) %>%   # by the variable
            do(tidy((glm.nb(visits ~ exposure + offset(log(follow_up)) + 
                                  age_cat + previous_covid_hosp + region  + 
                                  imd_q5 + ethnicity_6 + bmi_cat +
                                  number_comorbidities_cat + cov_covid_vax_n_cat,
                            data = ., link = log)), 
                    exponentiate=TRUE, conf.int=TRUE)) %>% 
            ungroup() %>% mutate(model = time)
      return(results)
}

subgroup_adj_sex <- bind_rows(
      adj_sex_subgroup_fn(matched_data_3m, "3 months"),
      adj_sex_subgroup_fn(matched_data_6m, "6 months"),
      adj_sex_subgroup_fn(matched_data_12m, "12 months")
)


# Function for age_group
adj_age_subgroup_fn <- function(data, time){
      results <- data %>% group_by(age_cat) %>%   # by the variable
            do(tidy((glm.nb(visits ~ exposure + offset(log(follow_up)) + 
                                  sex + previous_covid_hosp + region  + 
                                  imd_q5 + ethnicity_6 + bmi_cat +
                                  number_comorbidities_cat + cov_covid_vax_n_cat,
                            data = ., link = log)), 
                    exponentiate=TRUE, conf.int=TRUE)) %>% 
            ungroup() %>% mutate(model = time)
      return(results)
}

subgroup_adj_age <- bind_rows(
      adj_age_subgroup_fn(matched_data_3m, "3 months"),
      adj_age_subgroup_fn(matched_data_6m, "6 months"),
      adj_age_subgroup_fn(matched_data_12m, "12 months")
)


# Function for hospitalisation
adj_hos_subgroup_fn <- function(data, time){
      results <- data %>% group_by(previous_covid_hosp) %>%   # by the variable
            do(tidy((glm.nb(visits ~ exposure + offset(log(follow_up)) + 
                                  sex + age_cat + previous_covid_hosp + region  + 
                                  imd_q5 + ethnicity_6 + bmi_cat +
                                  number_comorbidities_cat + cov_covid_vax_n_cat,
                            data = ., link = log)), 
                    exponentiate=TRUE, conf.int=TRUE)) %>% 
            ungroup() %>% mutate(model = time)
      return(results)
}

subgroup_adj_hos <- bind_rows(
      adj_hos_subgroup_fn(matched_data_3m, "3 months"),
      adj_hos_subgroup_fn(matched_data_6m, "6 months"),
      adj_hos_subgroup_fn(matched_data_12m, "12 months")
)

adj_all_sub <- bind_rows(
      subgroup_adj_sex,
      adj_age_subgroup_fn,
      subgroup_adj_hos) %>% relocate(model) %>% 
      mutate(nb_model = "Adjusted") %>% relocate(nb_model)

bind_rows(crude_all_sub, adj_all_sub) %>% 
      write_csv(here("output", "st_02_subgroup_by_cov.csv"))