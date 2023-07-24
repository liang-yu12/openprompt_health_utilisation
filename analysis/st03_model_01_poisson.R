# Load previous data management
source("analysis/dm03_5_matched_pivot_long.R")

# Model: poisson regression

# Collapsing data by summarising the visits and follow-up time -----
matched_data_year <- matched_data_ts %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_visits),
            follow_up = sum(follow_up_time)) %>% 
      ungroup()


# Add covariates 
for_covariates <- matched_data_ts %>% distinct(patient_id, .keep_all = T) %>% 
      dplyr::select(1:43, "cov_covid_vaccine_number", "cov_covid_vax_n_cat",
                    "imd_q5","ethnicity_6"  ,"age_cat"  ,"bmi_cat" , "fu_total",
                    "number_comorbidities"  ,"number_comorbidities_cat")
for_covariates$exposure <- NULL

matched_data_year <- left_join(matched_data_year, for_covariates,
                               by = c("patient_id" = "patient_id"))
rm(for_covariates)


# Poisson model: 
# crude: ------
poisson_crude <- glm(visits ~ exposure + offset(log(follow_up)), 
                     data = matched_data_year, 
                     family = "poisson")

# organise output:
results_poisson_crude <- poisson_crude %>% 
      tidy(conf.int = T, conf.level = 0.95, exponentiate = T) %>% 
      filter(term != "(Intercept)") %>% 
      dplyr::select(term, estimate, conf.low, conf.high, p.value) %>% 
      mutate(model="Crude") %>% relocate(model)


# adjusted: ------
poisson_adjusted <- glm(visits ~ exposure + offset(log(follow_up)) + 
                        sex + region + age_cat + imd_q5 + ethnicity_6 + bmi_cat +
                        number_comorbidities_cat + previous_covid_hosp + cov_covid_vax_n_cat, 
                        data = matched_data_year, 
                        family = "poisson") 
# output organised 
results_poisson_adjusted <- poisson_adjusted %>% tidy(conf.int = T, conf.level = 0.95, exponentiate = T) %>% 
      filter(term != "(Intercept)") %>% 
      dplyr::select(term, estimate, conf.low, conf.high, p.value) %>% 
      mutate(model="Adjusted") %>% relocate(model)

# save outputs: ------
bind_rows(results_poisson_crude, poisson_adjusted) %>% 
      write_csv(here("output", "st03_model_01_poisson.csv"))



