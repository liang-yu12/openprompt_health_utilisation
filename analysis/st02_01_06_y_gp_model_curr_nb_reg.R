# Load previous data management
source("analysis/dm03_6_pivot_gp_long.R")

# Data management for modeling:: --------
# Collapsing data by summarising the visits and follow-up time, and 
# generate three datasets for follow-up 3m, 6m, and 12m
matched_data_gp_ts$month %>% table
# # 3 months
matched_data_gp_3m <- matched_data_gp_ts %>% 
      filter(month %in% c(1,2,3)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_gp_visits),
            follow_up = sum(follow_up_time)) %>% 
      ungroup()

# # 6 months
matched_data_gp_6m <- matched_data_gp_ts %>% 
      filter(month %in% c(1,2,3,4,5,6)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_gp_visits),
            follow_up = sum(follow_up_time)) %>% 
      ungroup()

# follow 12 months 
matched_data_gp_12m <- matched_data_gp_ts %>% 
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


# function for outputs:

output_organise_fn <- function(reg_results, model_name){
      t <- reg_results %>% summary() %>% 
            .$coefficients %>% as.data.frame()
      t$terms <- rownames(t)
      t <- t %>% mutate(lci = Estimate - 1.96*`Std. Error`) %>% 
            mutate(hci = Estimate + 1.96*`Std. Error`) %>% 
            filter(terms == "exposureLong covid exposure") %>% 
            dplyr::select(terms, Estimate, lci, hci, `Pr(>|z|)`) %>% 
            mutate(model = model_name) %>% relocate(model) 
      return(t)
}


# Stats: one part model -----
# # Model 1: crude negative binomial model: -----
# 3m 
nb_crude_3m <- glm.nb(visits ~ exposure + offset(log(follow_up)), 
                   data = matched_data_gp_3m,
                   link = log)

nb_crude_6m <- glm.nb(visits ~ exposure + offset(log(follow_up)), 
                      data = matched_data_gp_6m,
                      link = log)

nb_crude_12m <- glm.nb(visits ~ exposure + offset(log(follow_up)), 
                      data = matched_data_gp_12m,
                      link = log)

# # Model 2: adjusted Negative binomial: ------
# 3 months
nb_adj_3m <- glm.nb(visits ~ exposure + offset(log(follow_up)) + 
                       sex + region + age_cat + imd_q5 + ethnicity_6 + bmi_cat +
                       number_comorbidities_cat + previous_covid_hosp + cov_covid_vax_n_cat, 
                 data = matched_data_gp_3m,
                 link = log)

# 6 months
nb_adj_6m <- glm.nb(visits ~ exposure + offset(log(follow_up)) + 
                          sex + region + age_cat + imd_q5 + ethnicity_6 + bmi_cat +
                          number_comorbidities_cat + previous_covid_hosp + cov_covid_vax_n_cat, 
                    data = matched_data_gp_6m,
                    link = log)

# 12 months
nb_adj_12m <- glm.nb(visits ~ exposure + offset(log(follow_up)) + 
                          sex + region + age_cat + imd_q5 + ethnicity_6 + bmi_cat +
                          number_comorbidities_cat + previous_covid_hosp + cov_covid_vax_n_cat, 
                    data = matched_data_gp_12m,
                    link = log)

# Combine all results -----
all_reg_results <- bind_rows(
      output_organise_fn(nb_crude_3m, "Crude 3 months"),
      output_organise_fn(nb_adj_3m, "Adjusted 3 months"),
      output_organise_fn(nb_crude_6m, "Crude 6 months"),
      output_organise_fn(nb_adj_6m, "Adjusted 6 months"),
      output_organise_fn(nb_crude_12m, "Crude 12 months"),
      output_organise_fn(nb_adj_12m, "Adjusted 12 months")
      )

# # write outputs:
all_reg_results %>% write_csv(here("output", "st_02_non_cluster_model.csv"))
