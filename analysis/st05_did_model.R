# Run difference in difference model:

# Load previous data management
source("analysis/dm02_matched_hx_data.R")

# For organising the outputs
options(digits=2)
hx_matched_data %>%
      group_by(exposure) %>%
      summarise(
            mean1 = mean(all_month1),
            zero1 = sum(all_month1 == 0),
            mean2 = mean(all_month2),
            zero2 = sum(all_month2 == 0),
            mean3 = mean(all_month3),
            zero3 = sum(all_month3 == 0),
            mean4 = mean(all_month4),
            zero4 = sum(all_month4 == 0),
            mean5 = mean(all_month5),
            zero5 = sum(all_month5 == 0),
            mean6 = mean(all_month6),
            zero6 = sum(all_month6 == 0),
            mean7 = mean(all_month7),
            zero7 = sum(all_month7 == 0),
            mean8 = mean(all_month8),
            zero8 = sum(all_month8 == 0),
            mean9 = mean(all_month9),
            zero9 = sum(all_month9 == 0),
            mean10 = mean(all_month10),
            zero10 = sum(all_month10 == 0),
            mean11 = mean(all_month11),
            zero11 = sum(all_month11 == 0),
            mean12 = mean(all_month12),
            zero12 = sum(all_month12 == 0)
      ) %>% write.csv(here("output", "model_comparison.csv"), row.names = F)
# 
# # 0. model selection: Use 6 month to test models 
# model_nb <- glm.nb(all_month6 ~ exposure*time + offset(fu_time_m6), 
#              data = hx_matched_data) 
# model_poisson <- glm(all_month6 ~ exposure*time + offset(fu_time_m6), 
#        data = hx_matched_data, family = "poisson") 
# 
# 
# model_nb_adj <- glm.nb(all_month6 ~ exposure*time + age_cat+ sex+ bmi_cat+
#                              ethnicity_6+ region+imd_q5+ number_comorbidities_cat+
#                              offset(fu_time_m6), 
#                    data = hx_matched_data) 
# model_poisson_adj <- glm(all_month6 ~ exposure*time + age_cat+ sex+ bmi_cat+
#                                ethnicity_6+ region+imd_q5+ number_comorbidities_cat+
#                                offset(fu_time_m6), 
#                      data = hx_matched_data, family = "poisson") 
# 
# # compare models:
# bind_rows(
#       model_nb %>% glance() %>% mutate(model = "Negative binomial - crude"),
#       model_poisson %>% glance() %>% mutate(model = "Poisson - crude"),
#       model_nb_adj %>% glance() %>% mutate(model = "Negative binomial - adjusted"),
#       model_poisson_adj %>% glance() %>% mutate(model = "Poisson - adjusted")
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