# Explanation: check the overdispersion by looking ad deviance and df

# 1. Total healthcare visits: -----
# Load previous data management
source("analysis/dm02_01_now_pivot_total_visits_long.R")


matched_data_12m
# Model: crude poisson model: ------
poisson_crude <- glm(visits ~ exposure + offset(log(follow_up)), 
                     data = matched_data_12m, 
                     family = "poisson") %>% summary()




# Calculate the chisq and deviance / degree of freedom
total_healthcare_visits <- with(poisson_crude, cbind(res.deviance = deviance, 
                                                     df = df.residual,
                                                     p = pchisq(deviance, df.residual, lower.tail=FALSE),
                                                     dev_df_ratio = deviance/df.residual)) %>% 
      as.data.frame() %>% 
      mutate(outcome = "Total healthcare visit") %>% 
      relocate(outcome)




# 2. Primary care visits------
source("analysis/dm02_02_now_pivot_gp_visits_long.R")

gp_crude <- glm(visits ~ exposure + offset(log(follow_up)), 
                     data = matched_data_gp_12m, 
                     family = "poisson") %>% summary()

# Calculate the chisq and deviance / degree of freedom
gp_visits <- with(gp_crude, cbind(res.deviance = deviance, 
                                  df = df.residual,
                                  p = pchisq(deviance, df.residual, lower.tail=FALSE),
                                  dev_df_ratio = deviance/df.residual)) %>% 
      as.data.frame() %>% 
      mutate(outcome = "GP visits")  %>% 
      relocate(outcome)

# 3. Hospitalisation: ---------
source("analysis/dm02_03_now_pivot_hos_long.R")

hos_crude <- glm(visits ~ exposure + offset(log(follow_up)), 
                data = matched_data_hos_12m, 
                family = "poisson") %>% summary()
hos_admin <-  with(hos_crude, cbind(res.deviance = deviance, 
                                   df = df.residual,
                                   p = pchisq(deviance, df.residual, lower.tail=FALSE),
                                   dev_df_ratio = deviance/df.residual)) %>% 
      as.data.frame() %>% 
      mutate(outcome = "Hospital admission")  %>% 
      relocate(outcome)


# 4. A&E visits: ----------
source("analysis/dm02_04_now_pivot_ane_long.R")
ane_crude <- glm(visits ~ exposure + offset(log(follow_up)), 
                 data = matched_data_ae_12m, 
                 family = "poisson") %>% summary()
ane_visit <-  with(ane_crude, cbind(res.deviance = deviance, 
                                    df = df.residual,
                                    p = pchisq(deviance, df.residual, lower.tail=FALSE),
                                    dev_df_ratio = deviance/df.residual)) %>% 
      as.data.frame() %>% 
      mutate(outcome = "A&E visits")  %>% 
      relocate(outcome)


# 5. OPA visits ----
source("analysis/dm02_05_now_pivot_opa_long.R")
opa_crude <- glm(visits ~ exposure + offset(log(follow_up)), 
                 data = matched_data_opa_12m, 
                 family = "poisson") %>% summary()
opa_visit <-  with(ane_crude, cbind(res.deviance = deviance, 
                                    df = df.residual,
                                    p = pchisq(deviance, df.residual, lower.tail=FALSE),
                                    dev_df_ratio = deviance/df.residual)) %>% 
      as.data.frame() %>% 
      mutate(outcome = "OPA visits")  %>% 
      relocate(outcome)

# Save outputs:

bind_rows(
      total_healthcare_visits, gp_visits, hos_admin, ane_visit, opa_visit
) %>% write_csv("output/st04_02_supp_02_checking_dispersion.csv")

