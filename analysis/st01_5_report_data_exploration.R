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

# Crude rate -----
# Overall healthcare utilisation rate by month: 
crude_rate_fn <- function(data,m){
glm(monthly_visits ~ exposure + offset(log(follow_up_time)), 
    data = data,
    family = "poisson") %>%
      tidy(conf.int = T, conf.level = 0.95, exponentiate = T) %>% 
            mutate(month = m) %>% relocate(month)
}
monthly_crude_rates <- bind_rows(
      crude_rate_fn(matched_data_ts[matched_data_ts$month == 1,], 1),
      crude_rate_fn(matched_data_ts[matched_data_ts$month == 2,], 2),
      crude_rate_fn(matched_data_ts[matched_data_ts$month == 3,], 3),
      crude_rate_fn(matched_data_ts[matched_data_ts$month == 4,], 4),
      crude_rate_fn(matched_data_ts[matched_data_ts$month == 5,], 5),
      crude_rate_fn(matched_data_ts[matched_data_ts$month == 6,], 6),
      crude_rate_fn(matched_data_ts[matched_data_ts$month == 7,], 7),
      crude_rate_fn(matched_data_ts[matched_data_ts$month == 8,], 8),
      crude_rate_fn(matched_data_ts[matched_data_ts$month == 9,], 9),
      crude_rate_fn(matched_data_ts[matched_data_ts$month == 10,], 10),
      crude_rate_fn(matched_data_ts[matched_data_ts$month == 11,], 11),
      crude_rate_fn(matched_data_ts[matched_data_ts$month == 12,], 12)
)

monthly_crude_rates %>% write_csv(here("output", "st1_5_crude_monthly_rate.csv"))
