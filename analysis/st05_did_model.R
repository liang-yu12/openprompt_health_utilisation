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

# 
# # 1. print the output
# bind_rows(
#       (model_nb %>%
#              tidy(conf.int = T, conf.level = 0.95, exponentiate = T) %>%
#       mutate(model = "Negative binomial - crude")),
#       (model_nb_adj %>%
#              tidy(conf.int = T, conf.level = 0.95, exponentiate = T) %>%
#       mutate(model = "Negative binomial - adjusted")),
#       (model_poisson %>%
#             tidy(conf.int = T, conf.level = 0.95, exponentiate = T) %>%
#             mutate(model = "Poisson - crude")),
#       (model_poisson_adj %>%
#              tidy(conf.int = T, conf.level = 0.95, exponentiate = T) %>%
#              mutate(model = "Poisson - adjusted"))
#       ) %>%
#       write.csv(here("output", "results_models.csv"), row.names = F)