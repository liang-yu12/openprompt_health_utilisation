# Load previous data management
source("analysis/dm04_matched_estimale_costs.R")

# first manually exclude observations with missing variables

no_na <- matched_data %>% filter(
      !is.na(exposure) & !is.na(sex) &! is.na(age_cat) & !is.na(cov_covid_vax_n_cat) &
      !is.na(bmi_cat) & !is.na(imd_q5) & !is.na(ethnicity_6) & !is.na(region) &
            !is.na(number_comorbidities_cat))

cumulative_cost_adj_fn <- function(cost, n){
      
      # Run two-part model
      twopm.model <- tpm(
            cost ~ exposure + sex + age_cat + cov_covid_vax_n_cat + bmi_cat + imd_q5 + ethnicity_6 + region +
                  number_comorbidities_cat,
            data = no_na,
            link_part1 = "probit", family_part2 = Gamma(link = "log")
      )
      
      # Estimate the costs
      cost <- predict(twopm.model, se.fit = TRUE) %>% 
            as.data.frame() %>% 
            dplyr::select(fit, se.fit) %>% 
            mutate(ll = fit - 1.96*se.fit) %>% 
            mutate(ul = fit + 1.96*se.fit) %>% 
            dplyr::select(fit, ll, ul)

      # Add the predicted cost data bact to the original dataset
      no_na <- cbind(no_na, cost)
      results <- no_na %>% 
            group_by(exposure) %>% 
            summarise(mean = mean(fit),
                      sd = sd(fit)) %>% 
            mutate(month = n) %>% relocate(month)
}

results_costs_full <- bind_rows(
      cumulative_cost_adj_fn(cost = no_na$gp_cost_m1, 
                             n = 1),
      cumulative_cost_adj_fn(cost = no_na$gp_cost_m2, 
                             n = 2),
      cumulative_cost_adj_fn(cost = no_na$gp_cost_m3, 
                             n = 3),
      cumulative_cost_adj_fn(cost = no_na$gp_cost_m4, 
                             n = 4),
      cumulative_cost_adj_fn(cost = no_na$gp_cost_m5, 
                             n = 5),
      cumulative_cost_adj_fn(cost = no_na$gp_cost_m6, 
                             n = 6),
      cumulative_cost_adj_fn(cost = no_na$gp_cost_m7, 
                             n = 7),
      cumulative_cost_adj_fn(cost = no_na$gp_cost_m8, 
                             n = 8),
      cumulative_cost_adj_fn(cost = no_na$gp_cost_m10, 
                             n = 9),
      cumulative_cost_adj_fn(cost = no_na$gp_cost_m11, 
                             n = 11),
      cumulative_cost_adj_fn(cost = no_na$gp_cost_m12, 
                             n = 12)
      ) %>% 
      mutate(model = "Fully adjusted") %>% 
      relocate(model)


# save
results_costs_full %>% 
      write.csv(here("output", "st_04_result_cumulative_cost_full_2pm.csv"), row.names = F)
