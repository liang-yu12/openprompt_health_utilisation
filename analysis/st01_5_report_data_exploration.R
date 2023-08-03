source("analysis/dm03_5_matched_pivot_long.R")

# Explore the data:
# - The percentage of zero
# - Outcome distribution by time 
# - Crude rate 


# Showing the percentage of zero ----

png(file=here("output", "st1_5_explore_zero_percentage.png"),
    width=1200, height=600)
# plots
matched_data_ts %>% mutate(month=as.numeric(month)) %>% 
      dplyr::select("exposure", "month", "monthly_visits") %>% 
      group_by(exposure, month) %>% 
      summarise(zero_percentage = mean(monthly_visits==0)*100) %>% 
      as_tibble() %>% 
ggplot(aes(x = month, fill = exposure, y = zero_percentage, color = exposure)) +
      geom_bar(stat = "identity", alpha = 0.5, position = "dodge") +
      labs(x = "Month", y = "Zero Percentage") + 
      ylim(0, 100) + scale_x_continuous(breaks = 1:12)

dev.off()


# Outcome distribution by time -----
matched_data_ts %>% 
      mutate(month=as.numeric(month)) %>% 
      dplyr::select("exposure", "month", "monthly_visits", 
                    "fu_total", "index_date","end_death", "end_deregist",
                    "end_lc_cure", "end_date", "follow_up_time") %>% 
      group_by(exposure, month) %>% 
      summarise(median_visit= median(monthly_visits, na.rm = T),
                mean_visit= mean(monthly_visits, na.rm = T),
                max_visit= max(monthly_visits, na.rm = T),
                max_fu_total = max(fu_total, na.rm = T),
                min_fu_total = min(fu_total, na.rm = T),
                min_fu_time = min(follow_up_time, na.rm = T),
                wrong_time = sum(follow_up_time <= 0),
                min_index = min(index_date, na.rm = T),
                min_death_date = min(end_death, na.rm = T), # investigating the end dates 
                min_deregister = min(end_deregist, na.rm = T),
                min_lc_cure = min(end_lc_cure, na.rm = T),
                min_enddate = min(end_date, na.rm = T)) %>% 
      write_csv(here("output", "st1_5_monthly_outcome_distribution.csv"))

rm(list = ls())
# check the outcomes extreme values by months:

source("analysis/dm03_matched_define_monthly_follow_up_time.R")

matched_data <- bind_rows(lc_exp_matched, com_matched)
matched_data %>% names()
visit_cols <- matched_data[grep("all_month_", names(matched_data))] %>% 
      names() %>% as.vector() 

matched_data[visit_cols] <- lapply(matched_data[visit_cols], function(x){
      cut(x,
          breaks = c(0,5,10,20, 9999999), 
          labels = c("Less than 5", "More than 5 but less than 10", "More than 10 but less than 20", "more than 20"),
          right = T)
})

dependent = "exposure"
explanatory = c(visit_cols)
test <- matched_data %>% summary_factorlist(dependent, explanatory, 
                                    p = F,
                                    add_row_totals = TRUE,
                                    row_totals_colname = "Total",
) 
test %>% write.csv(here("output", "st1_5_cat_visits_summary.csv"), row.names = F)

