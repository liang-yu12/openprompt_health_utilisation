# Visualise the outputs

options(digits = 2)

# Load all packages
source("analysis/settings_packages.R")

# 1. Main outcomes: total healthcare utilisation ----------
# read in datasets for plotting:
# Binomial models: ---------------
bi_total_visits <- read_csv("output/st02_01_total_binomial.csv") %>% 
      filter(time == "12 months" & Adjustment == "Adjusted") %>% 
      mutate(Group = "Long COVID group") %>% 
      dplyr::select(Group, estimate, lci, hci) %>% 
      add_row(
            Group = "Comparator group",
            estimate = 1,
            lci = 1,
            hci = 1) %>% 
      arrange(Group)
# Decrease decimal points

bi_total_visits$`OR (95% CI)` <- sprintf("%.2f (%.2f - %.2f)",
                                        bi_total_visits$estimate,  
                                        bi_total_visits$lci,  
                                        bi_total_visits$hci)
bi_total_visits$`   First part      `  <- " "
bi_total_visits <- relocate(bi_total_visits, `   First part      `  , .after = Group)
bi_total_visits <- rename(bi_total_visits, `Healthcare utilisation type` = Group)

# Create a forest plot object
(bi_forestplot <- forest(
      data = bi_total_visits[,c(1,2,6)],
      est = bi_total_visits$estimate,
      lower = bi_total_visits$lci,
      upper = bi_total_visits$hci,
      ci_column = 2,
      ref_line = 1
))

# Hurdle models: -------------
# Read in results: 
total_hurdle <- read_csv("output/st02_01_total_hurdle.csv") %>% 
      filter(time == "12 months" & Adjustment == "Adjusted") %>% 
      mutate(Group = "Long COVID group") %>% 
      dplyr::select(Group, estimate, lci, hci) %>% 
      add_row(
            Group = "Comparator group",
            estimate = 1,
            lci = 1,
            hci = 1) %>% 
      arrange(Group)

total_hurdle$`Rate ratio (95% CI)` <- sprintf("%.2f (%.2f - %.2f)",
                                                total_hurdle$estimate,  
                                                total_hurdle$lci,  
                                                total_hurdle$hci)
# Create an empty column for plots
total_hurdle$`   Second part      `  <- " "
total_hurdle <- relocate(total_hurdle, `   Second part      ` , .after = Group)
total_hurdle <- rename(total_hurdle, `Healthcare utilisation type` = Group)



# Create a forest plot object
(hurdle_forestplot <- forest(
      data = total_hurdle[,c(1,2,6)],
      est = total_hurdle$estimate,
      lower = total_hurdle$lci,
      upper = total_hurdle$hci,
      ci_column = 2,
      xlim = c(0, 4),
      ref_line = 1
))

# Try combining two data---------
part2 <- total_hurdle %>% rename(
      estimate2 = estimate, 
      lci2 = lci,
      hci2 = hci
)
part1 <- bi_total_visits
combine <- full_join(part1, part2)

combine[,c(1,2,6,7,11)] %>% names

tm <- forest_theme(core=list(bg_params=list(fill = c("#FFFFFF"))))

two_forest <- forest(
      data = combine[,c(1,2,6,7,11)],
      est = list(combine$estimate, combine$estimate2),
      lower = list(combine$lci, combine$lci2),
      upper = list(combine$hci,combine$hci2),
      ci_column = c(2, 4),
      xlim = list(c(1, 9), c(1, 1.7)),
      ref_line = 1,
      theme = tm)
plot(two_forest)



# Barplot: -----------
# Read in outcome data:
total_predicted_counts <- read_csv("output/st02_01_total_predicted_counts.csv") %>% 
            filter(adjustment == "Adjusted")

# change order by assigning the order of the factor
total_predicted_counts$exposure <- factor(total_predicted_counts$exposure, 
                                          levels=c("Long covid exposure","Comparator"))


# Colour blind friendly:
cbp1 <- c("#FFC20A", "#0C7BDC")


(visits_barplot <- ggplot(total_predicted_counts, aes(fill=exposure, y=visits, x=exposure)) + 
      geom_bar(position="dodge", stat="identity", width = 0.5) +  coord_flip() +
      geom_errorbar(position=position_dodge(0.5), aes(ymin = lci, ymax = hci), width = 0.2) + 
      guides(fill=guide_legend(title="   ")) +
      ylab("Average total healthcare utilisation frequency") + xlab(" ") + theme_bw() +
      scale_fill_manual(values = cbp1))

# Combine plots together : ----------

visits_all_plots <- ggarrange(two_forest, visits_barplot, ncol = 1, labels = c("a", "b"))
ggsave(visits_all_plots, file = "output/st02_06_total_healthcare_visits.svg",
       device = "svg",
       width=12, height=5, units = "in", dpi = 300)




