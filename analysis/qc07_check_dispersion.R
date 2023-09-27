# 1. Overall visits dispersion exploration: ----------------
# Load previous data management
source("analysis/dm03_5_matched_pivot_long.R")

# follow 12 months 
matched_data_12m <- matched_data_ts %>% 
      filter(!is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_visits),
            follow_up = sum(follow_up_time)) %>% 
      ungroup()

# correct the level of exposure groups
matched_data_12m$exposure <- relevel(matched_data_12m$exposure, ref = "Comparator")


# Exclude rows with NA and create 1/0 outcomes:
crude_vars <- c("visits", "exposure", "follow_up")#for crude anaylsis

crude_complete_12m <- matched_data_12m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

## Check total visits over dispersion: ------
# Fit a poisson model to the data
mod <- glm(visits ~ exposure + offset(log(follow_up)),
           family = poisson,
           data = crude_complete_12m)

# Calculate the chisq and deviance / degree of freedom
with(mod, cbind(res.deviance = deviance, 
                df = df.residual,
                p = pchisq(deviance, df.residual, lower.tail=FALSE),
                dev_df_ratio = deviance/df.residual)) %>% as.data.frame() %>% 
      write_csv(here("output", "qc07_total_vist_overdispersion_test.csv"))

# house keeping
rm(list = ls())

# 2. GP visit over dispersion -------------
# Load previous data management
source("analysis/dm03_6_pivot_gp_long.R")
# follow 12 months 
matched_data_gp_12m <- matched_data_gp_ts %>% 
      filter(!is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_gp_visits),
            follow_up = sum(follow_up_time)) %>% 
      ungroup()
matched_data_gp_12m$exposure <- relevel(matched_data_gp_12m$exposure, ref = "Comparator")
# Stats: two part (Hurdle) model -----
# first need to exclude rows with NA and create 1/0 outcomes:
crude_vars <- c("visits", "exposure", "follow_up")#for crude anaylsis
crude_complete_gp_12m <- matched_data_gp_12m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

## Check gp visits over dispersion: ------
# Fit a poisson model to the data
mod <- glm(visits ~ exposure + offset(log(follow_up)),
           family = poisson,
           data = crude_complete_gp_12m)

# Calculate the chisq and deviance / degree of freedom
with(mod, cbind(res.deviance = deviance, 
                df = df.residual,
                p = pchisq(deviance, df.residual, lower.tail=FALSE),
                dev_df_ratio = deviance/df.residual)) %>% as.data.frame() %>% 
      write_csv(here("output", "qc07_gp_vist_overdispersion_test.csv"))

# house keeping
rm(list = ls())

# 3. SUS OPA visits: -------------
# Load previous data management
source("analysis/dm03_9_pivot_opa_long.R")

# Data management for modeling:: --------
# Collapsing data by summarising the visits and follow-up time, and 
# generate three datasets for follow-up  12m
# follow 12 months 
matched_data_opa_12m <- matched_data_opa_ts %>% 
      filter(!is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_opa_visits),
            follow_up = sum(follow_up_time)) %>% 
      ungroup()
# correct the level of exposure groups
matched_data_opa_12m$exposure <- relevel(matched_data_opa_12m$exposure, ref = "Comparator")

# Stats: two part (Hurdle) model -----
# first need to exclude rows with NA and create 1/0 outcomes:
crude_vars <- c("visits", "exposure", "follow_up")#for crude anaylsis
crude_opa_complete_12m <- matched_data_opa_12m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

## Check OPA visits over dispersion: ------
# Fit a poisson model to the data
mod <- glm(visits ~ exposure + offset(log(follow_up)),
           family = poisson,
           data = crude_opa_complete_12m)

# Calculate the chisq and deviance / degree of freedom
with(mod, cbind(res.deviance = deviance, 
                df = df.residual,
                p = pchisq(deviance, df.residual, lower.tail=FALSE),
                dev_df_ratio = deviance/df.residual)) %>% as.data.frame() %>% 
      write_csv(here("output", "qc07_opa_vist_overdispersion_test.csv"))
