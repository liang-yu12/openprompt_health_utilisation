# Run difference in difference model:

# Load previous data management
source("analysis/dm02_matched_hx_data.R")

# For organising the outputs
options(digits=2)
check_dispersion <- hx_matched_data %>%
      group_by(exposure) %>%
      summarise(
            zero1 = sum(all_month1 == 0),
            zero2 = sum(all_month2 == 0),
            zero3 = sum(all_month3 == 0),
            zero4 = sum(all_month4 == 0),
            zero5 = sum(all_month5 == 0),
            zero6 = sum(all_month6 == 0),
            zero7 = sum(all_month7 == 0),
            zero8 = sum(all_month8 == 0),
            zero9 = sum(all_month9 == 0),
            zero10 = sum(all_month10 == 0),
            zero11 = sum(all_month11 == 0),
            zero12 = sum(all_month12 == 0)
      ) 

# 0. model selection: Use 6 month to test models
model_poisson <- glm(all_month6 ~ exposure*time + offset(fu_time_m6),
       data = hx_matched_data, family = "poisson")

check_dispersion$dispersion <- summary(model_poisson)$deviance / df.residual(model_poisson)

check_dispersion %>% write.csv(here("output", "sup_st01_check_dispersion_poisson.csv"), row.names = F)

# Use negative binomial model: 
glm.nb(all_month6 ~ exposure*time + offset(fu_time_m6),
       data = hx_matched_data) %>% tidy() %>% 
      write.csv(here("output", "results_models.csv"), row.names = F)

# # compare models:
# bind_rows(
#       model_nb %>% glance() %>% mutate(model = "Negative binomial - crude"),
#       model_poisson %>% glance() %>% mutate(model = "Poisson - crude")
#       ) %>%
#       write.csv(here("output", "model_comparison.csv"), row.names = F)
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