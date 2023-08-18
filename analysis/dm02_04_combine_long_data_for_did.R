# Load previous data management:
source("analysis/dm02_03_pivot_longer_now_data.R")

# Combine all datasets
did_data <- bind_rows(now_com_ts, now_exp_ts, hx_com_ts, hx_exp_ts)

# As factor
did_data$exposure <- as.factor(did_data$exposure)
levels(did_data$exposure) <- c("Comparator", "Long COVID exposure")

did_data$time <- as.factor(did_data$time)
levels(did_data$time) <- c("Historical", "Contemporary")

# Combine 12 month follow-up and visits: 

did_data_12m <- did_data %>% 
      filter(!is.na(fu_time)) %>% 
      group_by(patient_id, exposure, time) %>% 
      summarise(
            visits = sum(monthly_visits),
            follow_up = sum(fu_time)) %>% 
      ungroup()

# adding covariates back: 
did_data_12m <- did_data %>% distinct(patient_id, exposure, time, .keep_all = T) %>% 
      dplyr::select(all_of(common_vars),exposure, time) %>%
      right_join(did_data_12m, 
                 by = c("patient_id" = "patient_id",
                        "exposure" = "exposure",
                        "time" = "time"))

