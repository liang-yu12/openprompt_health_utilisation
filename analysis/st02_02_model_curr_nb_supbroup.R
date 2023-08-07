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
      filter(month %in% c(1,2,3)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_visits),
            follow_up = sum(follow_up_time)) %>% 
      ungroup()

# # 6 months
matched_data_6m <- matched_data_ts %>% 
      filter(month %in% c(1,2,3,4,5,6)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_visits),
            follow_up = sum(follow_up_time)) %>% 
      ungroup()

# follow 12 months 
matched_data_12m <- matched_data_ts %>% 
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
# Function for crude negative binomial models and organise outputs:
nb_crude_fn <- function(subet_data, time){
      nb_reg <- glm.nb(visits ~ exposure + offset(log(follow_up)), 
             data = subet_data,
             link = log)
      
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

# # Sex: ------
# # # female / male: 
# 3 months
sg_female_3m <- nb_crude_fn(matched_data_3m[matched_data_3m$sex == "female",],  "3 months") %>% 
      mutate(subgroup= "Sex: female") %>% relocate(subgroup)
sg_male_3m <- nb_crude_fn(matched_data_3m[matched_data_3m$sex != "female",],  "3 months") %>% 
      mutate(subgroup= "Sex: male") %>% relocate(subgroup)

# 6 months
sg_female_6m <- nb_crude_fn(matched_data_6m[matched_data_6m$sex == "female",],  "6 months") %>% 
      mutate(subgroup= "Sex: female") %>% relocate(subgroup)
sg_male_6m <- nb_crude_fn(matched_data_6m[matched_data_6m$sex != "female",],  "6 months") %>% 
      mutate(subgroup= "Sex: male") %>% relocate(subgroup)

# 12 months
sg_female_12m <- nb_crude_fn(matched_data_12m[matched_data_12m$sex == "female",],  "12 months") %>% 
      mutate(subgroup= "Sex: female") %>% relocate(subgroup)
sg_male_12m <- nb_crude_fn(matched_data_12m[matched_data_12m$sex != "female",],  "12 months") %>% 
      mutate(subgroup= "Sex: male") %>% relocate(subgroup)

subgroup_crude_sex <- bind_rows(
      sg_female_3m, sg_male_3m, 
      sg_female_6m, sg_male_6m, 
      sg_female_12m, sg_male_12m
)


# # Age groups: ------
# # # age_cat "18-29" "30-39" "40-49" "50-59" "60-69" "70+"  
# 3 months
sg_age18_3m <- nb_crude_fn(matched_data_3m[matched_data_3m$age_cat == "18-29",],  "3 months") %>% 
      mutate(subgroup= "Age: 18-29") %>% relocate(subgroup)
sg_age30_3m <- nb_crude_fn(matched_data_3m[matched_data_3m$age_cat == "30-39",],  "3 months") %>% 
      mutate(subgroup= "Age: 30-39") %>% relocate(subgroup)
sg_age40_3m <- nb_crude_fn(matched_data_3m[matched_data_3m$age_cat == "40-49",],  "3 months") %>% 
      mutate(subgroup= "Age: 40-49") %>% relocate(subgroup)
sg_age50_3m <- nb_crude_fn(matched_data_3m[matched_data_3m$age_cat == "50-59",],  "3 months") %>% 
      mutate(subgroup= "Age: 50-59") %>% relocate(subgroup)
sg_age60_3m <- nb_crude_fn(matched_data_3m[matched_data_3m$age_cat == "60-69",],  "3 months") %>% 
      mutate(subgroup= "Age: 60-69") %>% relocate(subgroup)
sg_age70_3m <- nb_crude_fn(matched_data_3m[matched_data_3m$age_cat == "70+",],  "3 months") %>% 
      mutate(subgroup= "Age: over 70") %>% relocate(subgroup)

subgroup_crude_age_3m <- bind_rows(
      sg_age18_3m, sg_age30_3m, sg_age40_3m, sg_age50_3m, sg_age60_3m, sg_age70_3m)

# 6 months
sg_age18_6m <- nb_crude_fn(matched_data_6m[matched_data_6m$age_cat == "18-29",],  "6 months") %>% 
      mutate(subgroup= "Age: 18-29") %>% relocate(subgroup)
sg_age30_6m <- nb_crude_fn(matched_data_6m[matched_data_6m$age_cat == "30-39",],  "6 months") %>% 
      mutate(subgroup= "Age: 30-39") %>% relocate(subgroup)
sg_age40_6m <- nb_crude_fn(matched_data_6m[matched_data_6m$age_cat == "40-49",],  "6 months") %>% 
      mutate(subgroup= "Age: 40-49") %>% relocate(subgroup)
sg_age50_6m <- nb_crude_fn(matched_data_6m[matched_data_6m$age_cat == "50-59",],  "6 months") %>% 
      mutate(subgroup= "Age: 50-59") %>% relocate(subgroup)
sg_age60_6m <- nb_crude_fn(matched_data_6m[matched_data_6m$age_cat == "60-69",],  "6 months") %>% 
      mutate(subgroup= "Age: 60-69") %>% relocate(subgroup)
sg_age70_6m <- nb_crude_fn(matched_data_6m[matched_data_6m$age_cat == "70+",],  "6 months") %>% 
      mutate(subgroup= "Age: over 70") %>% relocate(subgroup)

subgroup_crude_age_6m <- bind_rows(
      sg_age18_6m, sg_age30_6m, sg_age40_6m, sg_age50_6m, sg_age60_6m, sg_age70_6m)

# 12 months
sg_age18_12m <- nb_crude_fn(matched_data_12m[matched_data_12m$age_cat == "18-29",],  "12 months") %>% 
      mutate(subgroup= "Age: 18-29") %>% relocate(subgroup)
sg_age30_12m <- nb_crude_fn(matched_data_12m[matched_data_12m$age_cat == "30-39",],  "12 months") %>% 
      mutate(subgroup= "Age: 30-39") %>% relocate(subgroup)
sg_age40_12m <- nb_crude_fn(matched_data_12m[matched_data_12m$age_cat == "40-49",],  "12 months") %>% 
      mutate(subgroup= "Age: 40-49") %>% relocate(subgroup)
sg_age50_12m <- nb_crude_fn(matched_data_12m[matched_data_12m$age_cat == "50-59",],  "12 months") %>% 
      mutate(subgroup= "Age: 50-59") %>% relocate(subgroup)
sg_age60_12m <- nb_crude_fn(matched_data_12m[matched_data_12m$age_cat == "60-69",],  "12 months") %>% 
      mutate(subgroup= "Age: 60-69") %>% relocate(subgroup)
sg_age70_12m <- nb_crude_fn(matched_data_12m[matched_data_12m$age_cat == "70+",],  "12 months") %>% 
      mutate(subgroup= "Age: over 70") %>% relocate(subgroup)

subgroup_crude_age_12m <- bind_rows(
      sg_age18_12m, sg_age30_12m, sg_age40_12m, sg_age50_12m, sg_age60_12m, sg_age70_12m)


# # COVID hospitalisations: ------
# # # previous_covid_hosp: "FALSE" "TRUE" 
# 3 months
sg_nohos_3m <- nb_crude_fn(matched_data_3m[matched_data_3m$previous_covid_hosp == "FALSE",],  "3 months") %>% 
      mutate(subgroup= "Admission due to COVID: yes") %>% relocate(subgroup)
sg_hos_3m <- nb_crude_fn(matched_data_3m[matched_data_3m$previous_covid_hosp == "TRUE",],  "3 months") %>% 
      mutate(subgroup= "Admission due to COVID: yes") %>% relocate(subgroup)

# 6 months
sg_nohos_6m <- nb_crude_fn(matched_data_6m[matched_data_6m$previous_covid_hosp == "FALSE",],  "6 months") %>% 
      mutate(subgroup= "Admission due to COVID: yes") %>% relocate(subgroup)
sg_hos_6m <- nb_crude_fn(matched_data_6m[matched_data_6m$previous_covid_hosp == "TRUE",],  "6 months") %>% 
      mutate(subgroup= "Admission due to COVID: yes") %>% relocate(subgroup)

# 12 months
sg_nohos_12m <- nb_crude_fn(matched_data_12m[matched_data_12m$previous_covid_hosp == "FALSE",],  "12 months") %>% 
      mutate(subgroup= "Admission due to COVID: yes") %>% relocate(subgroup)
sg_hos_12m <- nb_crude_fn(matched_data_12m[matched_data_12m$previous_covid_hosp == "TRUE",],  "12 months") %>% 
      mutate(subgroup= "Admission due to COVID: yes") %>% relocate(subgroup)

subgroup_crude_hospital <- bind_rows(
      sg_nohos_3m, sg_hos_3m, 
      sg_nohos_6m, sg_hos_6m, 
      sg_nohos_12m, sg_hos_12m)

bind_rows(
      subgroup_crude_sex, subgroup_crude_hospital,
      subgroup_crude_age_3m, subgroup_crude_age_6m, subgroup_crude_age_12m) %>% 
      write_csv(here("output", "subgroup_by_cov_crude.csv"))

# Model 2: adjusted Negative binomial: ------
# function for sex
nb_adj_sex_fn <- function(subet_data, time){
      nb_reg <- glm.nb(visits ~ exposure + offset(log(follow_up)) + 
                  age_cat + previous_covid_hosp + region  + imd_q5 + ethnicity_6 + bmi_cat +
                  number_comorbidities_cat + cov_covid_vax_n_cat,
                       data = subet_data,
                       link = log)
      
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

sg_adj_female_3m <- nb_adj_sex_fn(matched_data_3m[matched_data_3m$sex == "female",],  "3 months") %>% 
      mutate(subgroup= "Sex: female") %>% relocate(subgroup)
sg_adj_male_3m <- nb_adj_sex_fn(matched_data_3m[matched_data_3m$sex != "female",],  "3 months") %>% 
      mutate(subgroup= "Sex: male") %>% relocate(subgroup)

# 6 months
sg_adj_female_6m <- nb_adj_sex_fn(matched_data_6m[matched_data_6m$sex == "female",],  "6 months") %>% 
      mutate(subgroup= "Sex: female") %>% relocate(subgroup)
sg_adj_male_6m <- nb_adj_sex_fn(matched_data_6m[matched_data_6m$sex != "female",],  "6 months") %>% 
      mutate(subgroup= "Sex: male") %>% relocate(subgroup)

# 12 months
sg_adj_female_12m <- nb_adj_sex_fn(matched_data_12m[matched_data_12m$sex == "female",],  "12 months") %>% 
      mutate(subgroup= "Sex: female") %>% relocate(subgroup)
sg_adj_male_12m <- nb_adj_sex_fn(matched_data_12m[matched_data_12m$sex != "female",],  "12 months") %>% 
      mutate(subgroup= "Sex: male") %>% relocate(subgroup)

subgroup_adj_sex <- bind_rows(
      sg_adj_female_3m, sg_adj_male_3m, 
      sg_adj_female_6m, sg_adj_male_6m, 
      sg_adj_female_12m, sg_adj_male_12m
)

subgroup_adj_sex %>% 
      write_csv(here("output", "subgroup_by_cov_adj_testing.csv"))

