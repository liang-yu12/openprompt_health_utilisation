# Visualise the outputs

options(digits = 2)

# Load all packages
source("analysis/settings_packages.R")

# Wrap visualisation codes into a function:

# Forest plots function:
forest_plot_function <- function(binomial_file, hurdle_file, predicted_file){
      # Binomial models:
      bi_total_visits <- binomial_file
      
      bi_total_visits$`OR (95% CI)` <- sprintf("%.2f (%.2f - %.2f)",
                                               bi_total_visits$estimate,  
                                               bi_total_visits$lci,  
                                               bi_total_visits$hci)
      bi_total_visits$`   First part      `  <- " "
      bi_total_visits <- relocate(bi_total_visits, `   First part      `  , .after = Group)
      bi_total_visits <- rename(bi_total_visits, `Healthcare utilisation type` = Group)
      
      
      # Hurdle models:
      # Read in results: 
      total_hurdle <- hurdle_file
      
      total_hurdle$`Rate ratio (95% CI)` <- sprintf("%.2f (%.2f - %.2f)",
                                                    total_hurdle$estimate,  
                                                    total_hurdle$lci,  
                                                    total_hurdle$hci)
      # Create an empty column for plots
      total_hurdle$`   Second part      `  <- " "
      total_hurdle <- relocate(total_hurdle, `   Second part      ` , .after = Group)
      total_hurdle <- rename(total_hurdle, `Healthcare utilisation type` = Group)
      
      # Try combining two data
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
            xlim = list(c(0, 4), c(0, 1.8)),
            ref_line = 1,
            theme = tm)
      plot(two_forest)
      
      return(two_forest)
}

visit_bar_fc <- function(predicted_data, fig_title){
      
      # Barplot:
      # change order by assigning the order of the factor
      predicted_data$exposure <- factor(predicted_data$exposure, 
                                        levels=c("Long covid exposure","Comparator"))
      
      
      # Colour blind friendly:
      cbp1 <- c("#FFC20A", "#0C7BDC")
      
      
      (visits_barplot <- ggplot(predicted_data, aes(fill=exposure, y=visits, x=exposure)) + 
                  geom_bar(position="dodge", stat="identity", width = 0.5) +  coord_flip() +
                  geom_errorbar(position=position_dodge(0.5), aes(ymin = lci, ymax = hci), width = 0.2) + 
                  guides(fill=guide_legend(title="   ")) +
                  ylab(fig_title) + xlab(" ") + theme_bw() +
                  scale_fill_manual(values = cbp1))
      
      return(visits_barplot)
}


# 1. Visualise gp only results: ---------
gp_binomial <- read_csv("output/st02_02_02_gponly_binomial.csv")  %>% 
      filter(time == "12 months" & Adjustment == "GP Adjusted") %>% 
      mutate(Group = "Long COVID group") %>% 
      dplyr::select(Group, estimate, lci, hci) %>% 
      add_row(
            Group = "Comparator group",
            estimate = 1,
            lci = 1,
            hci = 1) %>% 
      arrange(Group)

gp_hurdle <- read_csv("output/st02_02_02_gponly_hurdle.csv") %>% 
      filter(time == "12 months" & Adjustment == "GP Adjusted") %>% 
      mutate(Group = "Long COVID group") %>% 
      dplyr::select(Group, estimate, lci, hci) %>% 
      add_row(
            Group = "Comparator group",
            estimate = 1,
            lci = 1,
            hci = 1) %>% 
      arrange(Group)

# GP forest plot:
gp_only_forest_plot <- forest_plot_function(gp_binomial, gp_hurdle)

# Read in predicted gp counts
predicted_gp_visits<- read_csv("output/st02_02_02_gponly_predicted_counts.csv") %>% 
      filter(model == "Adjusted")


# Bar plot: 
gp_bar <- visit_bar_fc(predicted_gp_visits, "Average GP consultation frequency")

# combine and save outputs
gp_plots <- ggarrange(gp_only_forest_plot, gp_bar, ncol = 1, labels = c("a", "b"))
ggsave(gp_plots, file = "output/st02_02_03_gp_only_visits.svg",
       device = "svg",
       width=14, height=5, units = "in", dpi = 300)





# 2. Visualise prescription visit results: ---------
drug_binomial <- read_csv("output/st02_02_01_drug_binomial.csv")  %>% 
      filter(time == "12 months" & Adjustment == "drug Adjusted") %>% 
      mutate(Group = "Long COVID group") %>% 
      dplyr::select(Group, estimate, lci, hci) %>% 
      add_row(
            Group = "Comparator group",
            estimate = 1,
            lci = 1,
            hci = 1) %>% 
      arrange(Group)

drug_hurdle <- read_csv("output/st02_02_01_drug_hurdle.csv") %>% 
      filter(time == "12 months" & Adjustment == "drug Adjusted") %>% 
      mutate(Group = "Long COVID group") %>% 
      dplyr::select(Group, estimate, lci, hci) %>% 
      add_row(
            Group = "Comparator group",
            estimate = 1,
            lci = 1,
            hci = 1) %>% 
      arrange(Group)


# drug visit forest plot:
drug_visit_forest_plot <- forest_plot_function(drug_binomial, drug_hurdle)

# Read in predicted gp counts
predicted_drug_visits<- read_csv("output/st02_02_01_drug_predicted_counts.csv") %>% 
      filter(model == "Adjusted")


# Bar plot: 
drug_bar <- visit_bar_fc(predicted_drug_visits, "Average prescription visit frequency")

# combine and save outputs
drug_visit_plots <- ggarrange(drug_visit_forest_plot, drug_bar, ncol = 1, labels = c("a", "b"))
ggsave(drug_visit_plots, file = "output/st02_02_03_prescription_visits.svg",
       device = "svg",
       width=14, height=5, units = "in", dpi = 300)
