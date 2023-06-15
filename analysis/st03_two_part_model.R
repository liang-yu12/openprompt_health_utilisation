# Load previous data management
source("analysis/dm04_matched_estimale_costs.R")

 


month_1 <- tpm(
      gp_cost_m1 ~ sex ,
      data = matched_data,
      link_part1 = "probit", family_part2 = Gamma(link = "log")
)

estimated_cost <- predict(month_1, se.fit = TRUE) %>% 
      as.data.frame() %>% 
      dplyr::select(fit, se.fit) %>% 
      mutate(ll = fit - 1.96*se.fit) %>% 
      mutate(ul = fit + 1.96*se.fit) %>% 
      dplyr::select(fit, ll, ul)

complete_vars <- c("sex")
no_na <- matched_data[complete.cases(matched_data[,complete_vars]),]

no_na <- cbind(no_na, estimated_cost)
results <- no_na %>% 
      group_by(exposure) %>% 
      summarise(mean = mean(fit),
                sd = sd(fit)) %>% 
      mutate(month = 1) %>% relocate(month)

cumulative_cost_adj_fn <- function(cost, data, n){
      twopm.model <- tpm(
            cost ~ exposure + sex + age_cat + cov_covid_vax_n_cat + bmi_cat + imd_q5 + ethnicity_6 + region +
                  number_comorbidities_cat,
            data = matched_data,
            link_part1 = "probit", family_part2 = Gamma(link = "log")
      )
      
      cost <- predict(twopm.model, se.fit = TRUE) %>% 
            as.data.frame() %>% 
            dplyr::select(fit, se.fit) %>% 
            mutate(ll = fit - 1.96*se.fit) %>% 
            mutate(ul = fit + 1.96*se.fit) %>% 
            dplyr::select(fit, ll, ul)
      
      complete_vars <- c("exposure", "sex", "age_cat", "cov_covid_vax_n_cat", 
                         "bmi_cat", "imd_q5", "ethnicity_6", "region", "number_comorbidities_cat")
      no_na <- matched_data[complete.cases(matched_data[,complete_vars]),]
      
      no_na <- cbind(no_na, cost)
      results <- no_na %>% 
            group_by(exposure) %>% 
            summarise(mean = mean(fit),
                      sd = sd(fit)) %>% 
            mutate(month = n) %>% relocate(month)
}

results_costs_full <- bind_rows(
      cumulative_cost_adj_fn(cost = matched_data$gp_cost_m1, 
                             data = matched_data,
                             n = 1),
      cumulative_cost_adj_fn(cost = matched_data$gp_cost_m2, 
                             data = matched_data,
                             n = 2),
      cumulative_cost_adj_fn(cost = matched_data$gp_cost_m3, 
                             data = matched_data,
                             n = 3),
      cumulative_cost_adj_fn(cost = matched_data$gp_cost_m4, 
                             data = matched_data,
                             n = 4),
      cumulative_cost_adj_fn(cost = matched_data$gp_cost_m5, 
                             data = matched_data,
                             n = 5),
      cumulative_cost_adj_fn(cost = matched_data$gp_cost_m6, 
                             data = matched_data,
                             n = 6),
      cumulative_cost_adj_fn(cost = matched_data$gp_cost_m7, 
                             data = matched_data,
                             n = 7),
      cumulative_cost_adj_fn(cost = matched_data$gp_cost_m8, 
                             data = matched_data,
                             n = 8),
      cumulative_cost_adj_fn(cost = matched_data$gp_cost_m10, 
                             data = matched_data,
                             n = 9),
      cumulative_cost_adj_fn(cost = matched_data$gp_cost_m11, 
                             data = matched_data,
                             n = 11),
      cumulative_cost_adj_fn(cost = matched_data$gp_cost_m12, 
                             data = matched_data,
                             n = 12)
      ) %>% 
      mutate(model = "Fully adjusted") %>% 
      relocate(model)


# save
results_costs_full %>% 
      write.csv(here("output", "st_04_result_cumulative_cost_full_2pm.csv"), row.names = F)
