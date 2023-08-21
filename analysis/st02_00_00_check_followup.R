# Load previous data management
source("analysis/dm03_5_matched_pivot_long.R")


# Investigating the Na/NaN/Inf in 'y' issue


# # 3 months
matched_data_3m <- matched_data_ts %>% 
      filter(month %in% c(1,2,3)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_visits, na.rm = T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup()

# # 6 months
matched_data_6m <- matched_data_ts %>% 
      filter(month %in% c(1,2,3,4,5,6)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_visits, na.rm = T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup()

# follow 12 months 
matched_data_12m <- matched_data_ts %>% 
      filter(!is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_visits, na.rm = T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
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

# correct the level of exposure groups
matched_data_3m$exposure <- relevel(matched_data_3m$exposure, ref = "Comparator")
matched_data_6m$exposure <- relevel(matched_data_6m$exposure, ref = "Comparator")
matched_data_12m$exposure <- relevel(matched_data_12m$exposure, ref = "Comparator")



bind_rows(data.frame(
      month = "3m",
      min=min(matched_data_3m$follow_up),
      na_count = sum(is.na(matched_data_3m$follow_up)),
      visit_min=min(matched_data_3m$visits),
      na_visit= sum(is.na(matched_data_12m$visits))  
),
data.frame(
      month = "6m",
      min=min(matched_data_6m$follow_up),
      na_count = sum(is.na(matched_data_6m$follow_up)),
                     visit_min=min(matched_data_6m$visits),
      na_visit= sum(is.na(matched_data_12m$visits))  
),
data.frame(
      month = "12m",
      min=min(matched_data_12m$follow_up),
      na_count = sum(is.na(matched_data_12m$follow_up)),
      visit_min=min(matched_data_12m$visits),
      na_visit= sum(is.na(matched_data_12m$visits))
      )
) %>% write_csv(here("output", "st02_check_min.csv"))


# test negative binomial
nb_crude_3m <- glm.nb(visits ~ exposure + offset(log(follow_up)), 
                      data = matched_data_3m %>% filter(!is.na(visits)&!is.na(follow_up)),
                      link = log)



