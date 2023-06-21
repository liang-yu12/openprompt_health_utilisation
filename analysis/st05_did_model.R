# Run difference in difference model:

# Load previous data management
source("analysis/dm02_matched_hx_data.R")

# For organising the outputs
options(digits=2)

# 0. model selection: 
model_nb <- glm.nb(all_month1 ~ exposure*time + offset(fu_time_m1), 
             data = hx_matched_data) 
model_poisson <- glm(all_month1 ~ exposure*time + offset(fu_time_m1), 
       data = hx_matched_data, family = "poisson") 


model_nb_adj <- glm.nb(all_month1 ~ exposure*time + age_cat+ sex+ bmi_cat+
                             ethnicity_6+ region+imd_q5+ number_comorbidities_cat+
                             offset(fu_time_m1), 
                   data = hx_matched_data) 
model_poisson_adj <- glm(all_month1 ~ exposure*time + age_cat+ sex+ bmi_cat+
                               ethnicity_6+ region+imd_q5+ number_comorbidities_cat+
                               offset(fu_time_m1), 
                     data = hx_matched_data, family = "poisson") 



aic_compare <- data.frame(
      nb_model = model_nb$aic,
      adjusted_nb_model = model_nb_adj$aic,
      poisson_model = model_poisson$aic,
      adjusted_poisson_model = model_poisson_adj$aic
)

aic_compare %>% write.csv(here("output", "model_compare_aic.csv"), row.names = F)
