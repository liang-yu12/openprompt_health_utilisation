source("analysis/dm03_9_pivot_opa_long.R")

# # For later data management: keep covariates
for_covariates <- matched_data_opa_ts %>% distinct(patient_id, exposure, .keep_all = T) %>% 
      dplyr::select("patient_id",     
                    "exposure",                
                    "region",      
                    "previous_covid_hosp")

# 1.check previous_covid_hosp and region -----

# # 1.1 Overall data ----
opa_couts_summary <- matched_data_opa_ts %>% group_by(region, exposure) %>% 
      summarise(opa_median = median(monthly_opa_visits),
                opa_mean = mean(monthly_opa_visits, na.rm = T),
                opa_max = max(monthly_opa_visits),
                high_use= sum(monthly_opa_visits>=4, na.rm = T),
                fu_time = mean(follow_up_time, na.rm =T)
      ) %>% as.data.frame() %>% mutate(data = "original")


# # 1.2 3 months-----
matched_data_opa_3m <- matched_data_opa_ts %>% 
      filter(month %in% c(1,2,3) & !is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_opa_visits, na.rm = T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup()

matched_data_opa_3m <- left_join(matched_data_opa_3m, for_covariates,
                                by = c("patient_id" = "patient_id", "exposure" = "exposure"))


# # check distribution: 
opa_summary_3m <- matched_data_opa_3m %>% group_by(region,exposure) %>% 
      summarise(opa_median = median(visits),
                opa_mean = mean(visits, na.rm =T),
                opa_max = max(visits),
                high_use= sum(visits>=4, na.rm = T),
                fu_time = mean(follow_up, na.rm =T)) %>% 
      as.data.frame() %>% mutate(data = "3 month")


# # 1.3 6 months-------
matched_data_opa_6m <- matched_data_opa_ts %>% 
      filter(month %in% c(1,2,3,4,5,6) & !is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_opa_visits, na.rm = T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup()


matched_data_opa_6m <- left_join(matched_data_opa_6m, for_covariates,
                                 by = c("patient_id" = "patient_id", "exposure" = "exposure"))

# check distribution
opa_summary_6m <- matched_data_opa_6m %>% group_by(region,exposure) %>% 
      summarise(opa_median = median(visits),
                opa_mean = mean(visits, na.rm =T),
                opa_max = max(visits),
                high_use= sum(visits>=4, na.rm = T),
                fu_time = mean(follow_up, na.rm =T)) %>% 
      as.data.frame() %>% mutate(data = "6 month")


# 1.4 follow 12 months  ------
matched_data_opa_12m <- matched_data_opa_ts %>% 
      filter(!is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_opa_visits, na.rm = T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup()

matched_data_opa_12m <- left_join(matched_data_opa_12m, for_covariates,
                                 by = c("patient_id" = "patient_id", "exposure" = "exposure"))

opa_summary_12m <- matched_data_opa_12m %>% group_by(region,exposure) %>% 
      summarise(opa_median = median(visits),
                opa_mean = mean(visits, na.rm =T),
                opa_max = max(visits),
                high_use= sum(visits>=4, na.rm = T),
                fu_time = mean(follow_up, na.rm =T)) %>% 
      as.data.frame() %>% mutate(data = "12 month")

bind_rows(
      opa_couts_summary,
      opa_summary_3m,
      opa_summary_6m,
      opa_summary_12m
) %>% write_csv(here("output", "opa_outcome_distribution.csv"))
