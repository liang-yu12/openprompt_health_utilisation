# Load previous data management
source("analysis/dm03_5_matched_pivot_long.R")

# Fitting random-effects Poisson regression ----
# - Allowing clustering (by individual)

# Random intercept model: cluster: by individual:  "patient_id"      

# define a function  to organise the regression output
output_org_fn <- function(reg, m){
      output <- reg %>% summary() %>% coef() %>% as.data.frame() 
      output <- output %>% mutate(term = rownames(output)) %>% 
            mutate(or=exp(Estimate)) %>% 
            mutate(lci=exp((Estimate-1.96*`Std. Error`))) %>% 
            mutate(hci=exp((Estimate+1.96*`Std. Error`))) %>% 
            mutate(model=m) %>% 
            dplyr::select(model, term, or, lci, hci,`Pr(>|z|)`)
      return(output)
}


# Crude random intercept ------
crude_glmer <- glmer(
      formula = monthly_visits ~ 1 + exposure + offset(log(follow_up_time)) + (1|patient_id),
      data = matched_data_ts,
      family = poisson(link = "log") 
)

ri_poisson_crude <- output_org_fn(crude_glmer, "Random intercept Poisson crude")

# Adjusted random intercept ------
adj_glmer <- glmer(
      formula = monthly_visits ~ 1 + exposure + offset(log(follow_up_time)) + (1|patient_id) +
            sex + region + age_cat + imd_q5 + ethnicity_6 + bmi_cat +
            number_comorbidities_cat + previous_covid_hosp + cov_covid_vax_n_cat,
      data = matched_data_ts,
      family = poisson(link = "log") 
)

ri_poisson_adj <- output_org_fn(adj_glmer, "Random intercept Poisson adjusted")

