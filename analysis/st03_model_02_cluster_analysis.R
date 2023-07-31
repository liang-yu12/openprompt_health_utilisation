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
            dplyr::select(model, term, or, lci, hci,`Pr(>|z|)`) %>% 
            filter(term == "(Intercept)" | term == "exposureLong covid exposure")
      
      aic <- reg %>% summary %>% .$AICtab %>% as.data.frame()
      aic <- aic %>% mutate(model_compare = rownames(aic)) %>% 
            rename("value" = ".") %>% relocate(model_compare) %>% 
            filter(model_compare == "AIC" | model_compare == "BIC") # compare AIC BIC
      
      output <- cbind(output, aic)
      
      return(output)
}
# 
# ## Crude random intercept ------
# crude_glmer <- glmer(
#       formula = monthly_visits ~ 1 + exposure + offset(log(follow_up_time)) + (1|patient_id),
#       data = matched_data_ts,
#       family = poisson(link = "log") 
# )
# 
# ri_poisson_crude <- output_org_fn(crude_glmer, "Random intercept Poisson crude")
# 
# ## Adjusted random intercept ------
# adj_glmer <- glmer(
#       formula = monthly_visits ~ 1 + exposure + offset(log(follow_up_time)) + (1|patient_id) +
#             sex + region + age_cat + imd_q5 + ethnicity_6 + bmi_cat +
#             number_comorbidities_cat + previous_covid_hosp + cov_covid_vax_n_cat,
#       data = matched_data_ts,
#       family = poisson(link = "log") 
# )
# 
# ri_poisson_adj <- output_org_fn(adj_glmer, "Random intercept Poisson adjusted")
# 
# ## Crude random slope -------
# 
# crude_glmer_slope <- glmer(
#       formula = monthly_visits ~ 1 + exposure + offset(log(follow_up_time)) + (1 + exposure |patient_id),
#       data = matched_data_ts,
#       family = poisson(link = "log") 
# )
# 
# rs_poisson_crude <- output_org_fn(crude_glmer_slope, "Random slope Poisson crude")
# 
# ## Adjusted random slope ------
# adj_glmer_slope <- glmer(
#       formula = monthly_visits ~ 1 + exposure + offset(log(follow_up_time)) + (1+ exposure|patient_id) +
#             sex + region + age_cat + imd_q5 + ethnicity_6 + bmi_cat +
#             number_comorbidities_cat + previous_covid_hosp + cov_covid_vax_n_cat,
#       data = matched_data_ts,
#       family = poisson(link = "log") 
# )
# 
# rs_poisson_adj <- output_org_fn(adj_glmer_slope, "Random slope Poisson adjusted")

# GEE -----

# function for organising outputs
gee_output_org_fn <- function(reg, m){
      output <- reg %>% summary() %>% coef() %>% as.data.frame() 
      output <- output %>% mutate(term = rownames(output)) %>% 
            mutate(or=exp(Estimate)) %>% 
            mutate(lci=exp((Estimate-1.96*`Std.err`))) %>% 
            mutate(hci=exp((Estimate+1.96*`Std.err`))) %>% 
            mutate(model=m) %>% 
            dplyr::select(model, term, or, lci, hci,`Pr(>|W|)`) %>% 
            filter(term == "(Intercept)" | term == "exposureLong covid exposure")
      
      return(output)
}

# Crude GEE poisson
gee_crude <- geeglm(monthly_visits ~ exposure + offset(log(follow_up_time)),
                    data = drop_na(matched_data_ts, any_of(c("monthly_visits", "exposure", "follow_up_time"))),
                    id = patient_id,
                    family = poisson(link = "log") ,
                    corstr = "ar1"
)

results_gee_crude <- gee_crude %>% gee_output_org_fn("GEE Crude")


# Adjusted GEE poisson
all_var <- c("monthly_visits","exposure", "follow_up_time", "sex", "region",
             "age_cat", "imd_q5", "ethnicity_6", "bmi_cat", "number_comorbidities_cat",
             "previous_covid_hosp", "cov_covid_vax_n_cat") # variables for non-missing

gee_adj <- geeglm(monthly_visits ~ exposure + offset(log(follow_up_time)),
                  data = drop_na(matched_data_ts, any_of(all_var)),
                  id = patient_id,
                  family = poisson(link = "log") ,
                  corstr = "ar1"
)

results_gee_adj <- gee_adj %>% gee_output_org_fn("GEE Adjusted")


# organised and save output
# bind_rows(ri_poisson_crude, 
#           ri_poisson_adj, 
#           rs_poisson_crude, 
#           rs_poisson_adj) %>% write_csv(here("output", "st03_model_02_rm_models.csv"))


bind_rows(results_gee_crude, results_gee_adj) %>% write_csv(here("output", "st03_model_02_gee_models.csv"))



