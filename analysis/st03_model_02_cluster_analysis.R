# Load previous data management
source("analysis/dm03_5_matched_pivot_long.R")

# Data exploration: what are the percentage of NA in 
complete_vars <- c("patient_id", "exposure", "monthly_visits", "month", "follow_up_time")
gee_crude_data <- matched_data_ts %>% dplyr::select(all_of(complete_vars)) %>% 
      filter(!is.na(patient_id) & !is.na(monthly_visits) & 
                   !is.na(exposure) & !is.na(follow_up_time) & !is.na(month))

complete_fu_id <- gee_crude_data %>% group_by(exposure, patient_id) %>% 
      summarise(n=n()) %>% 
      as_tibble() %>% 
      filter(n == 12) %>% dplyr::select(patient_id, exposure) #Try keeping obs with full 12 records 
gee_crude_data <- gee_crude_data %>%
      right_join(complete_fu_id, by = c("patient_id" = "patient_id", "exposure" = "exposure")) # make the data balanced



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
# GEE -----
# lapply(gee_crude_data[,c("patient_id", "month", "monthly_visits", "exposure", "follow_up_time")], function(x){table(is.na(x))})

# Crude GEE poisson
gee_crude <- geeglm(monthly_visits ~ exposure + offset(log(follow_up_time)),
                    data = gee_crude_data,
                    id = patient_id,
                    waves = month,
                    family = poisson(link = "log") ,
                    corstr = "ar1"
)
summary(gee_crude)
results_gee_crude <- gee_crude %>% gee_output_org_fn("GEE Crude")


# # Adjusted GEE poisson
# all_var <- c("monthly_visits","exposure", "follow_up_time", "sex", "region",
#              "age_cat", "imd_q5", "ethnicity_6", "bmi_cat", "number_comorbidities_cat",
#              "previous_covid_hosp", "cov_covid_vax_n_cat") # variables for non-missing
# 
# gee_adj <- geeglm(monthly_visits ~ exposure + offset(log(follow_up_time)),
#                   data = drop_na(matched_data_ts, any_of(all_var)),
#                   id = patient_id,
#                   family = poisson(link = "log") ,
#                   corstr = "ar1"
# )
# 
# results_gee_adj <- gee_adj %>% gee_output_org_fn("GEE Adjusted")


# # organised and save output
# bind_rows(ri_poisson_crude,
#           # ri_poisson_adj, rs_poisson_adj,
#           # rs_poisson_crude
#           ) %>% write_csv(here("output", "st03_model_02_rm_models.csv"))


# bind_rows(results_gee_crude, results_gee_adj) %>% 
results_gee_crude %>% write_csv(here("output", "st03_model_02_gee_models.csv"))



