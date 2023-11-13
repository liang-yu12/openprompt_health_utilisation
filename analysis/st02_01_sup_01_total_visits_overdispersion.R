# Load previous data management
source("analysis/dm02_01_now_pivot_total_visits_long.R")

# Explanation:
# 1. Calculate the overall rate (across 12 months) by using Poisson and negative binomial models
# Compare the model AIC for selecting

matched_data_12m
# Model: crude poisson model: ------
poisson_crude <- glm(visits ~ exposure + offset(log(follow_up)), 
                     data = matched_data_12m, 
                     family = "poisson") %>% summary()




# Calculate the chisq and deviance / degree of freedom
with(poisson_crude, cbind(res.deviance = deviance, 
                df = df.residual,
                p = pchisq(deviance, df.residual, lower.tail=FALSE),
                dev_df_ratio = deviance/df.residual)) %>% as.data.frame() %>% 
      mutate(outcome = "Total healthcare visit") %>% 
      write_csv(here("output", "st02_01_sup_01_total_vist_overdispersion.csv"))

# house keeping
rm(list = ls())





