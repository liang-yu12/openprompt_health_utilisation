# Load data management files
source("analysis/dm05_04_hx_combine_long_data_for_did.R")

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
      mutate(month = as.numeric(month)) %>% dplyr::select(exposure, time, month, monthly_visits) 


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
      theme(axis.text.x = element_text(size=12)) +
      annotate("text", x = 5, y=0.35, label = "Historical healthcare utilisation",hjust = 0.2) 


ggsave(file = "output/st05_historical_smooth.jpg", width = 12, height = 6)



# Additional plots: plot observed counts: 
# data management: select complete cases:
adj_did_complete_12m <- did_data_12m[complete.cases(did_data_12m),]
adj_did_complete_12m$ethnicity_6 <- droplevels(adj_did_complete_12m$ethnicity_6)

adj_did_complete_12m %>% names
 
summary_crude_data <- adj_did_complete_12m %>% 
      filter(!is.na(follow_up)) %>% 
      group_by(time, exposure) %>% 
      summarise(
            mean_visits = mean(visits, na.rm = T),
            sd_visits = sd(visits, na.rm = T)
      ) %>% 
      mutate(lci = mean_visits - 1.96*sd_visits,
             hci = mean_visits + 1.96*sd_visits)
      

# Plot: 
ggplot(summary_crude_data, aes(x = time, y = mean_visits, color = exposure)) +
      geom_point() + geom_errorbar(aes(ymin=lci, ymax=hci), width=0.1) +
      geom_line(aes(group = exposure), linewidth = 1)  + theme_bw() +
      ylim(c(0, 30)) +
      xlab("Time period") + ylab("Average healthcare visits") +
      scale_color_manual(values=c("#1E88E5", "#D81B60")) +
      guides(color=guide_legend(title="Exposure group")) 

ggsave(file = "output/st05_observed_his_now_line.jpg", 
       width = 9, height = 4, units = "in")
