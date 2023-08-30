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
                    "age",
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
# for_covariates$age_cat <- relevel(for_covariates$age_cat, ref = "18-29")
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

# set a function for organising the regression output
nb_reg_crude_fn <- function(data, sub_level){
      reg <- glm.nb(visits ~ exposure + offset(log(follow_up)),
                    data = data,link = log)
      output <- bind_cols((coef(reg) %>% exp() %>% as.data.frame()),
                          (confint(reg) %>% exp() %>% as.data.frame()))
      
      output$terms <- rownames(output)
      output <- output %>% filter(terms == "exposureLong covid exposure")
      output <- rename(output, estimates = .) %>% relocate(terms) %>% 
            mutate(subgroup = sub_level) %>% relocate(subgroup)
      return(output)
}

## By hospitalisation: -----
# Customize another function for hospitalisatioin;
crude_hos_subgroup_fn <- function(data, hos_var){
      # split the data by covid hospitalisation
      hos <-split(data, hos_var)
      hos_t <- hos$`TRUE`
      hos_f <- hos$`FALSE`
      
      results <- bind_rows(
            nb_reg_crude_fn(hos_t, "Hospitalised"),
            nb_reg_crude_fn(hos_f, "Not hospitalised")
      ) 
      return(results)
}

crude_hos_subgroup <- bind_rows(
      crude_hos_subgroup_fn(matched_data_3m, matched_data_3m$previous_covid_hosp) %>% mutate(time="3m"),
      crude_hos_subgroup_fn(matched_data_6m, matched_data_6m$previous_covid_hosp) %>% mutate(time="6m"),
      crude_hos_subgroup_fn(matched_data_12m, matched_data_12m$previous_covid_hosp) %>% mutate(time="12m"))



## By sex:-----
# customize a function for sex:
crude_sex_subgroup_fn <- function(data, sex_var){
      # split the data by covid hospitalisation
      sex_g <-split(data, sex_var)
      sex_m <- sex_g$male
      sex_f <- sex_g$female
      
      results <- bind_rows(
            nb_reg_crude_fn(sex_m, "Male"),
            nb_reg_crude_fn(sex_f, "Female")
      ) 
      return(results)
}

crude_sex_subgroup <- bind_rows(
      crude_sex_subgroup_fn(matched_data_3m, matched_data_3m$sex) %>% mutate(time="3m"),
      crude_sex_subgroup_fn(matched_data_6m, matched_data_6m$sex) %>% mutate(time="6m"),
      crude_sex_subgroup_fn(matched_data_12m, matched_data_12m$sex) %>% mutate(time="12m")
)


### By agegroup
# customize a function for age group:
crude_age_subgroup_fn <- function(data, age_var){
      # split the data by covid hospitalisation
      age_c <-split(data, age_var)
      age_18_29 <- age_c$`18-29`
      age_30_39 <- age_c$`30-39`
      age_40_49 <- age_c$`40-49`
      age_50_59 <- age_c$`50-59`
      age_60_69 <- age_c$`60-69`
      age_over_70 <- age_c$`70+`
      
      results <- bind_rows(
            nb_reg_crude_fn(age_18_29, "Age 28-29"),
            nb_reg_crude_fn(age_30_39, "Age 30-39"),
            nb_reg_crude_fn(age_40_49, "Age 40-49"),
            nb_reg_crude_fn(age_50_59, "Age 50-59"),
            nb_reg_crude_fn(age_60_69, "Age 60-69"),
            nb_reg_crude_fn(age_over_70, "Age over 70")
      ) 
      return(results)
}

crude_age_subgroup <- bind_rows(
      crude_age_subgroup_fn(matched_data_3m, matched_data_3m$age_cat) %>% mutate(time="3m"),
      crude_age_subgroup_fn(matched_data_6m, matched_data_6m$age_cat) %>% mutate(time="6m"),
      crude_age_subgroup_fn(matched_data_12m, matched_data_12m$age_cat) %>% mutate(time="12m")
)


# Model 2: adjusted Negative binomial: ------

# By hospitalisation adjusted model ----
# Set up reg function output: need to customized for hospitalisation: 
hos_nb_reg_adj_fn <- function(data, sub_level){
      reg <- glm.nb(visits ~ exposure + offset(log(follow_up)) +
                    sex + age_cat + region  + imd_q5 + ethnicity_6 + bmi_cat +
                          number_comorbidities_cat + cov_covid_vax_n_cat,
                    data = data,link = log)
      output <- bind_cols((coef(reg) %>% exp() %>% as.data.frame()),
                          (confint(reg) %>% exp() %>% as.data.frame()))
      
      output$terms <- rownames(output)
      output <- output %>% filter(terms == "exposureLong covid exposure")
      output <- rename(output, estimates = .) %>% relocate(terms) %>% 
            mutate(subgroup = sub_level) %>% relocate(subgroup)
      return(output)
}


adj_hos_subgroup_fn <- function(data, hos_var, cov1, cov){
      # split the data by covid hospitalisation
      hos <-split(data, hos_var)
      hos_t <- hos$`TRUE`
      hos_f <- hos$`FALSE`
      
      results <- bind_rows(
            hos_nb_reg_adj_fn(hos_t, sub_level ="Hospitalised"),
            hos_nb_reg_adj_fn(hos_f, sub_level ="Not hospitalised")
      ) 
      return(results)
}

adj_hos_subgroup <- bind_rows(
      adj_hos_subgroup_fn(matched_data_3m, matched_data_3m$previous_covid_hosp) %>% mutate(time="3m"),
      adj_hos_subgroup_fn(matched_data_6m, matched_data_6m$previous_covid_hosp) %>% mutate(time="6m"),
      adj_hos_subgroup_fn(matched_data_12m, matched_data_12m$previous_covid_hosp) %>% mutate(time="12m"))






# Save outputs

bind_rows(crude_hos_subgroup, crude_sex_subgroup, crude_age_subgroup,
          adj_hos_subgroup) %>% 
      write_csv(here("output", "st_02_subgroup_by_cov.csv"))
