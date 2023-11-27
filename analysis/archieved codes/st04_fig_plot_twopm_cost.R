# Visulise the healthcare utilisation:
source("analysis/settings_packages.R")
# read in data
all_cost <- read.csv(here("output", "st_04_result_cumulative_cost_full_2pm.csv"))
all_cost$month <- as.integer(all_cost$month)
      
# Full adjusted 
full <- all_cost %>% 
      ggplot(aes(x = month, y = mean, group = exposure)) +
      ggtitle("Cumulative costs") + theme(legend.title = element_blank()) + theme_bw() +
      geom_line(aes(color=exposure)) +
      geom_point(aes(color=exposure)) +
      geom_errorbar(aes(ymin = (mean-sd), ymax = (mean+sd)), width=0.2) +
      scale_x_continuous("Months after long COVID diagnosis", breaks = 1:12) +
      scale_y_continuous("Average healthcare cost (£)")

ggsave(full, 
       filename = "st_fig_04_cumulative_costs.svg",
       device = "svg", 
       path = here("output"),
       dpi = 400)

