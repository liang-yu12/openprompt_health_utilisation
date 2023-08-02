# Load previous data management
source("analysis/dm03_5_matched_pivot_long.R")

summary(matched_data_ts$follow_up_time)



# Data management: GEE only runs using complete data
complete_vars <- c("patient_id", "exposure", "monthly_visits", "month", "follow_up_time") # only keep necessary variables
gee_crude_data <- matched_data_ts %>% dplyr::select(all_of(complete_vars)) %>% 
      filter(!is.na(patient_id) & !is.na(monthly_visits) & 
                   !is.na(exposure) & !is.na(follow_up_time) & !is.na(month))

summary(gee_crude_data$follow_up_time)
table(gee_crude_data$month, useNA = "ifany")
table(gee_crude_data$exposure, useNA = "ifany")
gee_crude_data$monthly_visits %>% summary


# # Testing: running model if the data is balanced:
# complete_fu_id <- gee_crude_data %>% group_by(exposure, patient_id) %>% 
#       summarise(n=n()) %>% 
#       as_tibble() %>% 
#       filter(n == 12) %>% dplyr::select(patient_id, exposure) #Try keeping obs with full 12 records 
# gee_crude_data <- gee_crude_data %>%
#       right_join(complete_fu_id, by = c("patient_id" = "patient_id", "exposure" = "exposure")) # make the data balanced

# Ordering data by cluster and time
gee_crude_data <- arrange(gee_crude_data, patient_id, month)

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
# Crude GEE poisson
gee_crude <- geeglm(monthly_visits ~ exposure + offset(log(follow_up_time)),
                    data = gee_crude_data,
                    id = patient_id,
                    waves = month,
                    family = poisson(link = "log") ,
                    corstr = "ar1"
)

summary(gee_crude)

results_gee_crude <- gee_crude %>% gee_output_org_fn("GEE Crude") # organise output
results_gee_crude %>% write_csv(here("output", "st03_model_02_gee_models.csv"))



