# Load data management files
source("analysis/dm02_04_combine_long_data_for_did.R")

# report numbers: -------------
did_data_12m %>% summary_factorlist(
      dependent = "exposure",
      explanatory = c("sex", "age_cat", "ethnicity_6", "bmi_cat", "imd_q5", 
                      "region", "cov_asthma", "cov_mental_health", "number_comorbidities_cat"),
      p = TRUE
) %>% write.csv(here("output", "st05_hx_matched_numbers_table.csv"), row.names = F)


did_data_12m %>% group_by(exposure, time) %>% 
      summarise(mean_visits = mean(visits), sd=sd(visits)) %>% 
      as.data.frame() %>% 
      write.csv(here("output", "st05_hx_crude_vistis_exp_time.csv"), row.names = F)

# Describing healthcare utilisation by time and exposure -------


# choose the pre-pandemic data: 
did_plot_interrupt <- did_data %>% dplyr::select(exposure, time, month, monthly_visits) %>% 
      filter(time == "Historical") %>% 
      mutate(month = case_when(
            time == "Historical" ~ as.numeric(month), 
            time == "Contemporary" ~ (as.numeric(month) + 19))
      ) %>% dplyr::select(exposure, time, month, monthly_visits) 


# Plot the trend before and after long COVID using LOESS smooth:
ggplot() + 
      geom_smooth(
            data = did_plot_interrupt, 
            aes(x = month, y = monthly_visits, color = exposure),
            method="gam", se=TRUE) + 
      scale_colour_hue(guide = "none") + 
      geom_vline(xintercept = 12) + 
      xlab("Time") + ylab("Mean healthcare visits") +
      scale_x_continuous(breaks = seq(1, 12),
                         labels = c("2019 March", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", "2020 March")) + 
      theme_bw() + 
      theme(axis.text.x = element_text(size=10)) +
      annotate("text", x = 5, y=0.35, label = "Historical records",hjust = 0.2) 


ggsave(file = "output/st05_historical_smooth.svg", width = 8, height = 4)


ggsave(file = "output/st05_smooth_comparison.svg", width = 12, height = 4)

