# Load previous data management
source("analysis/dm04_matched_estimale_costs.R")

 
# exposure + sex + age_cat + cov_covid_vax_n_cat + bmi_cat + imd_q5 + ethnicity_6 + region + 
#       number_comorbidities_cat

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

matched_data