# 1. Overall visits dispersion exploration: ----------------
# Load previous data management
source("analysis/dm03_5_matched_pivot_long.R")

# follow 12 months 
matched_data_lc_12m <- matched_data_ts %>% 
      subset(exposure == "Long covid exposure") %>% 
      filter(!is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_visits),
            follow_up = sum(follow_up_time)) %>% 
      ungroup()

# correct the level of exposure groups
matched_data_lc_12m$exposure <- relevel(matched_data_lc_12m$exposure, ref = "Comparator")

# # Add covariates for adjustment
for_covariates <- matched_data_ts %>% distinct(patient_id, exposure, .keep_all = T) %>% 
      dplyr::select("patient_id",     
                    "exposure",           
                    "age", "age_cat",               
                    "sex",                     
                    "bmi_cat",
                    "ethnicity_6",             
                    "imd_q5",                  
                    "region",      
                    "cov_asthma",
                    "cov_mental_health",   
                    "previous_covid_hosp",     
                    "cov_covid_vax_n_cat",     
                    "number_comorbidities_cat")

matched_data_lc_12m <- left_join(matched_data_lc_12m, for_covariates,
                                 by = c("patient_id" = "patient_id", "exposure" = "exposure"))





# Exclude rows with NA and create 1/0 outcomes:
crude_vars <- c("visits", "exposure", "follow_up")#for crude anaylsis

lc_complete_12m <- matched_data_lc_12m[complete.cases(matched_data_lc_12m),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

## Check total visits over dispersion: ------
# Fit a poisson model to the data
mod <- glm(visits ~ offset(log(follow_up))+ age + sex + bmi_cat + 
                 ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                 previous_covid_hosp + cov_covid_vax_n_cat + number_comorbidities_cat,
           family = poisson,
           data = lc_complete_12m)

# Calculate the chisq and deviance / degree of freedom
with(mod, cbind(res.deviance = deviance, 
                df = df.residual,
                p = pchisq(deviance, df.residual, lower.tail=FALSE),
                dev_df_ratio = deviance/df.residual)) %>% as.data.frame() %>% 
      write_csv(here("output", "qc08_lc_only_overdispersion_test.csv"))

# house keeping
rm(list = ls())


