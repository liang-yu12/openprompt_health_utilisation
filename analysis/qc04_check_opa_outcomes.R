source("analysis/dm03_9_pivot_opa_long.R")

opa_couts_summary <- matched_data_opa_ts %>% group_by(exposure) %>% 
      summarise(opa_median = median(monthly_opa_visits),
                opa_mean = mean(monthly_opa_visits),
                opa_max = max(monthly_opa_visits),
                fu_time = mean(follow_up_time)
                ) %>% as.data.frame() %>% mutate(data = "original")


# # 3 months
matched_data_opa_3m <- matched_data_opa_ts %>% 
      filter(month %in% c(1,2,3) & !is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_opa_visits, na.rm = T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup()

# check distribution: 
opa_summary_3m <- matched_data_opa_3m %>% group_by(exposure) %>% 
      summarise(opa_median = median(visits),
                opa_mean = mean(visits),
                opa_max = max(visits),
                fu_time = mean(follow_up)
      ) %>% as.data.frame() %>% mutate(data = "3 month")


# # 6 months
matched_data_opa_6m <- matched_data_opa_ts %>% 
      filter(month %in% c(1,2,3,4,5,6) & !is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_opa_visits, na.rm = T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup()

# check distribution
opa_summary_6m <- matched_data_opa_6m %>% group_by(exposure) %>% 
      summarise(opa_median = median(visits),
                opa_mean = mean(visits),
                opa_max = max(visits),
                fu_time = mean(follow_up)
      ) %>% as.data.frame() %>% mutate(data = "6 month")


# follow 12 months 
matched_data_opa_12m <- matched_data_opa_ts %>% 
      filter(!is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_opa_visits, na.rm = T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup()

opa_summary_12m <- matched_data_opa_12m %>% group_by(exposure) %>% 
      summarise(opa_median = median(visits),
                opa_mean = mean(visits),
                opa_max = max(visits),
                fu_time = mean(follow_up)
      ) %>% as.data.frame() %>% mutate(data = "12 month")


bind_rows(
      opa_couts_summary,
      opa_summary_3m,
      opa_summary_6m,
      opa_summary_12m
) %>% write_csv(here("output", "opa_outcome_distribution.csv"))
