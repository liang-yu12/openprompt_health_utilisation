# Run difference in difference model:

# Load previous data management
source("analysis/dm02_matched_hx_data.R")

# For organising the outputs
options(digits=2)

# 0. model selection: Use 6 month to test models 
model_nb <- glm.nb(all_month6 ~ exposure*time + offset(fu_time_m6), 
             data = hx_matched_data) 
model_poisson <- glm(all_month6 ~ exposure*time + offset(fu_time_m6), 
       data = hx_matched_data, family = "poisson") 


model_nb_adj <- glm.nb(all_month6 ~ exposure*time + age_cat+ sex+ bmi_cat+
                             ethnicity_6+ region+imd_q5+ number_comorbidities_cat+
                             offset(fu_time_m6), 
                   data = hx_matched_data) 
model_poisson_adj <- glm(all_month6 ~ exposure*time + age_cat+ sex+ bmi_cat+
                               ethnicity_6+ region+imd_q5+ number_comorbidities_cat+
                               offset(fu_time_m6), 
                     data = hx_matched_data, family = "poisson") 

# compare models:
bind_rows(
      model_nb %>% glance() %>% mutate(model = "Negative binomial - crude"),
      model_poisson %>% glance() %>% mutate(model = "Poisson - crude"),
      model_nb_adj %>% glance() %>% mutate(model = "Negative binomial - adjusted"),
      model_poisson_adj %>% glance() %>% mutate(model = "Poisson - adjusted")
      ) %>% 
      write.csv(here("output", "model_comparison.csv"), row.names = F)

# 1. print the output
bind_rows(
      (model_nb %>% 
             tidy(conf.int = T, conf.level = 0.95, exponentiate = T) %>% 
      mutate(model = "Negative binomial - crude")),
      (model_nb_adj %>% 
             tidy(conf.int = T, conf.level = 0.95, exponentiate = T) %>% 
      mutate(model = "Negative binomial - adjusted")),
      (model_poisson %>% 
            tidy(conf.int = T, conf.level = 0.95, exponentiate = T) %>% 
            mutate(model = "Poisson - crude")),
      (model_poisson_adj %>% 
             tidy(conf.int = T, conf.level = 0.95, exponentiate = T) %>% 
             mutate(model = "Poisson - adjusted"))
      ) %>% 
      write.csv(here("output", "results_models.csv"), row.names = F)