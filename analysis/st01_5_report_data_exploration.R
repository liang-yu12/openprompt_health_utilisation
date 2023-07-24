source("analysis/dm03_5_matched_pivot_long.R")

# Explore the data:
# - The percentage of zero
# - Outcome distribution by time 
# - Crude rate 


# Showing the percentage of zero ----

png(file=here("output", "st1_5_explore_zero_percentage.png"),
    width=1200, height=600)
# plots
matched_data_ts %>% 
      dplyr::select("exposure", "month", "monthly_visits") %>% 
      group_by(exposure, month) %>% 
      summarise(zero_percentage = mean(monthly_visits==0)*100) %>% 
      as_tibble() %>% 
ggplot(aes(x = month, fill = exposure, y = zero_percentage)) +
      geom_bar(stat = "identity", alpha = 0.5, position = "dodge") +
      labs(x = "Month", y = "Zero Percentage") + 
      ylim(0, 100) + scale_x_continuous(breaks = 1:12)

dev.off()


# Outcome distribution by time -----
matched_data_ts[matched_data_ts$monthly_visits!=0,] %>% 
      dplyr::select("exposure", "month", "monthly_visits") %>% 
      group_by(exposure, month) %>% 
      summarise(median_visit= median(monthly_visits),
                mean_visit= mean(monthly_visits),
                max_visit= max(monthly_visits),
                min_visit= min(monthly_visits)) %>% 
      write_csv(here("output", "st1_5_monthly_outcome_distribution.csv"))

