# Outcome distribution plots -----
# Healthcare visits:
source("analysis/dm02_01_now_pivot_total_visits_long.R")

matched_data_12m <- matched_data_12m %>% dplyr::select(-total_drug_visit)

visit_distribution_his <- matched_data_12m %>% 
      ggplot(aes(x=visits, fill = exposure)) + 
      geom_histogram(position = "dodge") + 
      ggtitle("Total healthcare utilisation distribution") + theme(legend.position = "none") + 
      xlab("Total healthcare visits") + ylab("Counts") + theme_bw()+ 
      guides(fill=guide_legend(title="Exposure group")) +
      scale_fill_manual(values = c("#FFC20A", "#0C7BDC"))


# Healthcare costs:
source("analysis/dm03_03_now_pivot_total_long.R")

cost_distribution_his <- matched_cost_12m %>% 
      ggplot(aes(x=total_cost, fill = exposure)) + 
      geom_histogram(position = "dodge") + 
      ggtitle("Total healthcare cost distribution") + theme(legend.position = "none") + 
      xlab("Total healthcare costs") + ylab("Counts") + theme_bw()+ 
      guides(fill=guide_legend(title="Exposure group")) +
      scale_fill_manual(values = c("#E66100", "#5D3A9B"))


# Combine two graphs and label
both_distribution_plot <- ggarrange(visit_distribution_his, cost_distribution_his, 
          ncol = 1, labels = c("a", "b")) 

ggsave(both_distribution_plot, file = "output/st04_01_total_outcome_distribution.png",
       width=9, height=12, units = "in", dpi = 300)

# Showing the percentage of zero ----

png(file=here("output", "st04_01_explore_zero_percentage.png"),
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


# Explore the data:
# - The percentage of zero
# - Outcome distribution by time 
# - Crude rate 

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
                wrong_death_date = sum(end_death <= index_date, na.rm = T), # investigating the end dates 
                wrong_deregister = sum(end_deregist <= index_date, na.rm = T),
                wrong_lc_cure = sum(end_lc_cure <= index_date, na.rm = T),
                min_enddate = sum(end_date <= index_date, na.rm = T)) %>% 
      write_csv(here("output", "st04_01_monthly_outcome_distribution.csv"))

# check the outcomes extreme values by months:

source("analysis/dm01_02_now_monthly_follow_up.R")

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
test %>% write.csv(here("output", "st04_01_cat_visits_summary.csv"), row.names = F)

