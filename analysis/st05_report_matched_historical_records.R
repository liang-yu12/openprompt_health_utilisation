# Load data management files
source("analysis/dm02_04_combine_long_data_for_did.R")

# report numbers: -------------
did_data_12m %>% summary_factorlist(
      dependent = "exposure",
      explanatory = c("sex", "age_cat", "ethnicity_6", "bmi_cat", "imd_q5", 
                      "region", "number_comorbidities_cat"),
      p = TRUE
) %>% write.csv(here("output", "st05_hx_matched_numbers_table.csv"), row.names = F)


did_data_12m %>% group_by(exposure, time) %>% 
      summarise(mean_visits = mean(visits), sd=sd(visits)) %>% 
      as.data.frame() %>% 
      write.csv(here("output", "st05_hx_crude_vistis_exp_time.csv"), row.names = F)

# Describing healthcare utilisation by time and exposure -------

did_data %>% names
did_data$month %>% table
table(did_data$time, did_data$month)

did_data <- did_data %>% mutate(month_x = case_when(
      time == "Historical" ~ as.numeric(month), 
      time == "Contemporary" ~ (as.numeric(month) +13))
      )

# collapsed the data by obtaining the mean of each month
plot_data <- did_data %>% group_by(exposure, month_x) %>% 
      summarise(visits =mean(monthly_visits, na.rm = TRUE))

line_plot <- ggplot(plot_data, aes(x = month_x, y = visits, color = exposure)) + 
      geom_line()+ geom_vline(xintercept = 13) +
      xlab("Time before and after long COVID") + ylab("Mean healthcare visits") +
      scale_x_continuous(breaks = seq(1, 25),
                         labels = c("Historical month 1", "Historical month 2", "Historical month 3", 
                                  "Historical month 4", "Historical month 5", "Historical month 6", 
                                  "Historical month 7", "Historical month 8", "Historical month 9",
                                  "Historical month 10", "Historical month 11", "Historical month 12",
                                  "Long COVID index date",
                                  "Contemporary month 1", "Contemporary month 2", "Contemporary month 3", 
                                  "Contemporary month 4", "Contemporary month 5", "Contemporary month 6", 
                                  "Contemporary month 7", "Contemporary month 8", "Contemporary month 9", 
                                  "Contemporary month 10", "Contemporary month 11", "Contemporary month 12")) + 
      theme_bw() +
      theme(axis.text.x = element_text(size=6, angle=20))

ggsave(file = "output/st05_hx_now_comparison.jpg", width = 12, height = 4)


# try loess smoth
ggplot(did_data, aes(x = month_x, y = monthly_visits, color = exposure)) + 
      scale_colour_hue(guide = "none") + geom_smooth(method="gam", se=TRUE)+ 
      geom_vline(xintercept = 13) +
      xlab("Time before and after long COVID (Month)") + ylab("Mean healthcare visits") +
      scale_x_continuous(breaks = seq(1, 25),
                         labels = c(" 1", "2", "Month 3", "Month 4", "Month 5", "Month 6", 
                                    "Month 7", "Month 8", "Month 9",
                                    "Month 10", "Month 11", "Month 12",
                                    "Index date",
                                    "Month 1", "Month 2", "Month 3", "Month 4", "Month 5", "Month 6", 
                                    "Month 7", "Month 8", "Month 9",
                                    "Month 10", "Month 11", "Month 12")) + 
      theme_bw() + 
      theme(axis.text.x = element_text(size=8, angle=20)) +
      annotate("text", x = 5, y=0.35, label = "Historical records",hjust = 0.2) +
      annotate("text", x = 18, y=0.35, label = "Contemporary records",hjust = 0.2)

