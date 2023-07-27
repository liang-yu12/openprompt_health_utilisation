# Load previous data management
source("analysis/dm03_5_matched_pivot_long.R")

# Explanation:
# Calculate the overall rate (across 12 months) by using Poisson and negative binomial models
# Compare the model AIC for selecting

 
# Data management: --------
## Collapsing data by summarising the visits and follow-up time -----
matched_data_year <- matched_data_ts %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_visits),
            follow_up = sum(follow_up_time)) %>% 
      ungroup()


## Add covariates 
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

matched_data_year <- left_join(matched_data_year, for_covariates,
                               by = c("patient_id" = "patient_id", "exposure" = "exposure"))

matched_data_year$exposure <- relevel(matched_data_year$exposure, ref = "Comparator")
matched_data_year$exposure %>% levels()

# Model: crude poisson model: ------
poisson_crude <- glm(visits ~ exposure + offset(log(follow_up)), 
                     data = matched_data_year, 
                     family = "poisson") %>% summary()

# organised output:
result_poisson_crude <- as.data.frame(poisson_crude$coefficients) 
result_poisson_crude$lci <- exp(result_poisson_crude$Estimate - 1.96*result_poisson_crude$`Std. Error`)
result_poisson_crude$uci <- exp(result_poisson_crude$Estimate + 1.96*result_poisson_crude$`Std. Error`)
result_poisson_crude$Estimate <- exp(result_poisson_crude$Estimate)
result_poisson_crude$terms <- rownames(result_poisson_crude)
result_poisson_crude <- result_poisson_crude %>% 
      filter(terms == "exposureLong covid exposure") %>% 
      mutate(aic = poisson_crude$aic) %>% 
      mutate(model = "Poisson crude") %>% relocate(model, terms, Estimate,lci,uci)

# Model: crude negative binomial model: -----
nb_crude <- glm.nb(visits ~ exposure + offset(log(follow_up)), 
                   data = matched_data_year,
                   link = log) %>% summary()

result_nb_crude <- as.data.frame(nb_crude$coefficients)
result_nb_crude$lci <- exp(result_nb_crude$Estimate - 1.96*result_nb_crude$`Std. Error`)
result_nb_crude$uci <- exp(result_nb_crude$Estimate + 1.96*result_nb_crude$`Std. Error`)
result_nb_crude$Estimate <- exp(result_nb_crude$Estimate)
result_nb_crude$terms <- rownames(result_nb_crude)
result_nb_crude <- result_nb_crude %>% 
      filter(terms == "exposureLong covid exposure") %>% 
      mutate(aic = nb_crude$aic) %>% 
      mutate(model = "Negative binomial crude") %>% relocate(model, terms, Estimate,lci,uci)


# Model: adjusted Poisson: ------
poisson_adjusted <- glm(visits ~ exposure + offset(log(follow_up)) + 
                        sex + region + age_cat + imd_q5 + ethnicity_6 + bmi_cat +
                        number_comorbidities_cat + previous_covid_hosp + cov_covid_vax_n_cat, 
                        data = matched_data_year, 
                        family = "poisson")  %>% summary()
# output organised 
result_poisson_adjusted <- as.data.frame(poisson_adjusted$coefficients) 
result_poisson_adjusted$lci <- exp(result_poisson_adjusted$Estimate - 1.96*result_poisson_adjusted$`Std. Error`)
result_poisson_adjusted$uci <- exp(result_poisson_adjusted$Estimate + 1.96*result_poisson_adjusted$`Std. Error`)
result_poisson_adjusted$Estimate <- exp(result_poisson_adjusted$Estimate)
result_poisson_adjusted$terms <- rownames(result_poisson_adjusted)
result_poisson_adjusted <- result_poisson_adjusted %>% 
      filter(terms == "exposureLong covid exposure") %>% 
      mutate(aic = poisson_adjusted$aic) %>% 
      mutate(model = "Poisson adjusted") %>% relocate(model, terms, Estimate,lci,uci)



# Model: adjusted Negative binomial: ------
nb_adj <- glm.nb(visits ~ exposure + offset(log(follow_up)) + 
                       sex + region + age_cat + imd_q5 + ethnicity_6 + bmi_cat +
                       number_comorbidities_cat + previous_covid_hosp + cov_covid_vax_n_cat, 
                   data = matched_data_year,
                   link = log) %>% summary()

# output organised 
result_nb_adj <- as.data.frame(nb_adj$coefficients)
result_nb_adj$lci <- exp(result_nb_adj$Estimate - 1.96*result_nb_adj$`Std. Error`)
result_nb_adj$uci <- exp(result_nb_adj$Estimate + 1.96*result_nb_adj$`Std. Error`)
result_nb_adj$Estimate <- exp(result_nb_adj$Estimate)
result_nb_adj$terms <- rownames(result_nb_adj)
result_nb_adj <- result_nb_adj %>% 
      filter(terms == "exposureLong covid exposure") %>% 
      mutate(aic = nb_adj$aic) %>% 
      mutate(model = "Negative binomial adjusted") %>% relocate(model, terms, Estimate,lci,uci)


# save outputs: ------
bind_rows(result_poisson_crude, result_poisson_adjusted,
          result_nb_crude, result_nb_adj) %>% 
      write_csv(here("output", "st03_model_01_poisson_and_nb.csv"))



