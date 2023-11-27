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

# Data management for including NA visits
# Separate comparators and exposure, and assign NA to the visits during COVID pandemic
# set the month %in% 1:7 because no long COVID diagnoses between Apr 2020 and Oct 2020
com_with_na <- did_data %>% dplyr::select(exposure, time, month, monthly_visits) %>% 
      filter(exposure == "Comparator" & time == "Historical" & month %in% 1:7) %>% 
      mutate(time = "COVID pandemic", monthly_visits = NA_real_) %>% 
      mutate(month = month + 12) # set up month as a cont. var

exp_with_na <- did_data %>% dplyr::select(exposure, time, month, monthly_visits) %>% 
      filter(exposure == "Long COVID exposure" & time == "Historical" & month %in% 1:7) %>% 
      mutate(time = "COVID pandemic", monthly_visits = NA_real_) %>% 
      mutate(month = month + 12)

# For the contemporary data, set it as a cont. var after the COVID pandemic period
did_plot_interrupt <- did_data %>% dplyr::select(exposure, time, month, monthly_visits) %>% 
      mutate(month = case_when(
            time == "Historical" ~ as.numeric(month), 
            time == "Contemporary" ~ (as.numeric(month) + 19))
      ) %>% dplyr::select(exposure, time, month, monthly_visits) 

# combine the data and reset the factor levels.
did_plot_interrupt <- bind_rows(did_plot_interrupt, exp_with_na, com_with_na)
did_plot_interrupt$time  <- factor(did_plot_interrupt$time, levels = c("Historical", "COVID pandemic", "Contemporary"))

# Plot the trend before and after long COVID using LOESS smooth:
ggplot() + 
      geom_smooth(
            data = did_plot_interrupt %>% filter(time == "Historical"), 
            aes(x = month, y = monthly_visits, color = exposure),
            method="gam", se=TRUE) + 
      geom_smooth(
            data = did_plot_interrupt %>% filter(time == "COVID pandemic"), 
            aes(x = month, y = monthly_visits, color = exposure),
            method="gam", se=TRUE) + 
      geom_smooth(
            data = did_plot_interrupt %>% filter(time == "Contemporary"), 
            aes(x = month, y = monthly_visits, color = exposure),
            method="gam", se=TRUE) + 
      scale_colour_hue(guide = "none") + 
      geom_vline(xintercept = 12) + 
      geom_vline(xintercept = 20) +
      xlab("Time before and after diagnosed with long COVID") + ylab("Mean healthcare visits") +
      scale_x_continuous(breaks = seq(1, 31),
                         labels = c("2019 March", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", "2020 March", 
                                    " ", " "," ", " "," ",  " ", " ", "Long COVID diagnoses",
                                    " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ")) + 
      theme_bw() + 
      theme(axis.text.x = element_text(size=10)) +
      annotate("text", x = 5, y=0.35, label = "Historical records",hjust = 0.2) +
      annotate("text", x = 15, y=0.35, label = "COVID pandemic",hjust = 0.2) +
      annotate("text", x = 25, y=0.35, label = "Contemporary records",hjust = 0.2)


ggsave(file = "output/st05_smooth_comparison.svg", width = 12, height = 4)

