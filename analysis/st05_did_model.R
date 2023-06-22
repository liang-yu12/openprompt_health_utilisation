# Run difference in difference model:

# Load previous data management
source("analysis/dm02_matched_hx_data.R")


# 0. model selection: Use 6 month to test models
model_poisson <- glm(all_month6 ~ exposure*time + offset(log(fu_time_m6)),
       data = hx_matched_data, family = "poisson")
# Use negative binomial model: 
model_nb <- glm.nb(all_month6 ~ exposure*time + offset(log(fu_time_m6)),
       data = hx_matched_data) 

# compare models:
bind_rows(
      (model_nb %>% glance() %>% dplyr::select(AIC, BIC, deviance, df.residual) %>% 
            mutate(model = "Negative binomial - crude")),
      (model_poisson %>% glance()  %>% dplyr::select(AIC, BIC, deviance, df.residual) %>% 
            mutate(model = "Poisson - crude"))
      ) %>% relocate(model) %>% 
      mutate(dispersion = deviance/df.residual) %>% 
      write.csv(here("output", "sup_st01_model_compare.csv"), row.names = F)

# Poisson functions for all models: 

did_poisson_crude_fn <- function(all_vist, fu_time){
      glm(all_vist ~ exposure*time + offset(log(fu_time)),
          data = hx_matched_data, family = "poisson")
}

# Apply the function
month_1 <- did_poisson_fn(hx_matched_data$all_month1, hx_matched_data$fu_time_m1)
month_2 <- did_poisson_fn(hx_matched_data$all_month2, hx_matched_data$fu_time_m2)
month_3 <- did_poisson_fn(hx_matched_data$all_month3, hx_matched_data$fu_time_m3)
month_4 <- did_poisson_fn(hx_matched_data$all_month4, hx_matched_data$fu_time_m4)
month_5 <- did_poisson_fn(hx_matched_data$all_month5, hx_matched_data$fu_time_m5)
month_6 <- did_poisson_fn(hx_matched_data$all_month6, hx_matched_data$fu_time_m6)
month_7 <- did_poisson_fn(hx_matched_data$all_month7, hx_matched_data$fu_time_m7)
month_8 <- did_poisson_fn(hx_matched_data$all_month8, hx_matched_data$fu_time_m8)
month_9 <- did_poisson_fn(hx_matched_data$all_month9, hx_matched_data$fu_time_m9)
month_10 <- did_poisson_fn(hx_matched_data$all_month10, hx_matched_data$fu_time_m10)
month_11 <- did_poisson_fn(hx_matched_data$all_month11, hx_matched_data$fu_time_m11)
month_12 <- did_poisson_fn(hx_matched_data$all_month12, hx_matched_data$fu_time_m12)


# Obtain the fitted value:
