# Visulise the healthcare utilisation:
source("analysis/settings_packages.R")
# read in data
all_visit <- read.csv(here("output", "st_03_result_cumulative_visit_hurdle.csv"))
all_visit$month <- as.integer(all_visit$month)
      
# crude
crude <- all_visit %>% filter(model == "Crude") %>% 
      ggplot(aes(x = month, y = mean, group = exposure)) +
      geom_line(aes(color=exposure)) +
      geom_point(aes(color=exposure)) +
      geom_errorbar(aes(ymin = (mean-sd), ymax = (mean+sd)), width=0.2) + 
      ggtitle("Crude") + theme(legend.title = element_blank()) +
      scale_x_continuous("Months after long COVID diagnosis", breaks = 1:12) +
      scale_y_continuous("Average healthcare utilisation")

# save
ggsave(crude, 
       filename = "st_04_crude_healthcare_visit.png",
       path = here("output"),
       dpi = 400)

# Partially adjusted
partially <- all_visit %>% filter(model == "Partially adjusted") %>% 
      ggplot(aes(x = month, y = mean, group = exposure)) +
      ggtitle("Partially adjuested") + theme(legend.title = element_blank()) +
      geom_line(aes(color=exposure))+
      geom_point(aes(color=exposure)) + 
      geom_errorbar(aes(ymin = (mean-sd), ymax = (mean+sd)), width=0.2)


ggsave(partially, 
       filename = "st_04_partial_adj_healthcare_visit.png",
       path = here("output"),
       dpi = 400)

# Full adjusted 
full <- all_visit %>% filter(model == "Fully adjusted") %>% 
      ggplot(aes(x = month, y = mean, group = exposure)) +
      ggtitle("Fully adjusted") + theme(legend.title = element_blank()) +
      geom_line(aes(color=exposure))+
      geom_point(aes(color=exposure))+ 
      geom_errorbar(aes(ymin = (mean-sd), ymax = (mean+sd)), width=0.2)

ggsave(full, 
       filename = "st_04_full_adj_healthcare_visit.png",
       path = here("output"),
       dpi = 400)

