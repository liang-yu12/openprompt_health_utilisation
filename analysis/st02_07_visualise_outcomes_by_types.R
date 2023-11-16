# Visualise the outputs

options(digits = 2)

# Load all packages
source("analysis/settings_packages.R")

# Wrap visualisation codes into a function:

# Forest plots function:
forest_plot_function <- function(binomial_file, hurdle_file,x_axis_setting){
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
            xlim = x_axis_setting,
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


# 2. Visualise primary care outcomes: ------------
primary_binomial <- read_csv("output/st02_02_parimarycare_binomial.csv")  %>% 
      filter(time == "12 months" & Adjustment == "GP Adjusted") %>% 
      mutate(Group = "Long COVID group") %>% 
      dplyr::select(Group, estimate, lci, hci) %>% 
      add_row(
            Group = "Comparator group",
            estimate = 1,
            lci = 1,
            hci = 1) %>% 
      arrange(Group)

primary_hurdle <- read_csv("output/st02_02_parimarycare_hurdle.csv") %>% 
      filter(time == "12 months" & Adjustment == "GP Adjusted") %>% 
      mutate(Group = "Long COVID group") %>% 
      dplyr::select(Group, estimate, lci, hci) %>% 
      add_row(
            Group = "Comparator group",
            estimate = 1,
            lci = 1,
            hci = 1) %>% 
      arrange(Group)

# Primary care forest plot:
primary_care_x_setting <- list(c(0, 8), c(0.8, 1.5))
primary_care_forest_plot <- forest_plot_function(primary_binomial, 
                                                 primary_hurdle,
                                                 x_axis_setting = primary_care_x_setting)


# Read in predicted counts
predicted_primary_visits<- read_csv("output/st02_02_parimarycare_predicted_counts.csv") %>% 
      filter(model == "Adjusted")

# Bar plot: 
primary_care_bar <- visit_bar_fc(predicted_primary_visits, "Average parimary care utilisation frequency")

# combine and save outputs
paimary_care_plots <- ggarrange(primary_care_forest_plot, primary_care_bar, ncol = 1)
ggsave(paimary_care_plots, file = "output/st02_07_parimary_care_visits.png",
       width=14, height=5, units = "in", dpi = 300)


# 3. Visualise hospitalisation outcomes: -----
hos_binomial <- read_csv("output/st02_03_hos_binomial.csv") %>% 
      filter(time == "12 months" & Adjustment == "Hospital Adjusted") %>% 
      mutate(Group = "Long COVID group") %>% 
      dplyr::select(Group, estimate, lci, hci) %>% 
      add_row(
            Group = "Comparator group",
            estimate = 1,
            lci = 1,
            hci = 1) %>% 
      arrange(Group)

hos_hurdle<- read_csv("output/st02_03_hos_hurdle.csv")%>% 
      filter(time == "12 months" & Adjustment == "Hospital Adjusted") %>% 
      mutate(Group = "Long COVID group") %>% 
      dplyr::select(Group, estimate, lci, hci) %>% 
      add_row(
            Group = "Comparator group",
            estimate = 1,
            lci = 1,
            hci = 1) %>% 
      arrange(Group)

predicted_hos_admin_counts<- read_csv("output/st02_03_hos_predicted_counts.csv")%>% 
      filter(model == "Adjusted")

# hospitalisation forest plot:
hos_axis_setting <- list(c(0.5, 2), c(0.8, 1.5))
hos_forest <- forest_plot_function(hos_binomial, 
                                   hos_hurdle,
                                   x_axis_setting = hos_axis_setting)

# Hospitalisation bar plot: 
hos_bar <- visit_bar_fc(predicted_hos_admin_counts, "Average hospitalisation frequency")


# combine and save outputs
hos_plots <- ggarrange(hos_forest, hos_bar, ncol = 1, labels = c("a", "b"))
ggsave(hos_plots, file = "output/st02_07_hospitalisations.png",
       width=12, height=5, units = "in", dpi = 300)



# A&E visits --------
# Read in data
ane_binomial<- read_csv("output/st02_04_ane_binomial.csv") %>% 
      filter(time == "12 months" & Adjustment == "Adjusted A&E visits") %>% 
      mutate(Group = "Long COVID group") %>% 
      dplyr::select(Group, estimate, lci, hci) %>% 
      add_row(
            Group = "Comparator group",
            estimate = 1,
            lci = 1,
            hci = 1) %>% 
      arrange(Group)

ane_hurdle<- read_csv("output/st02_04_ane_hurdle.csv") %>% 
      filter(time == "12 months" & Adjustment == "Adjusted A&E visits") %>% 
      mutate(Group = "Long COVID group") %>% 
      dplyr::select(Group, estimate, lci, hci) %>% 
      add_row(
            Group = "Comparator group",
            estimate = 1,
            lci = 1,
            hci = 1) %>% 
      arrange(Group)

predicted_ane_visits <- read_csv("output/st02_04_ane_predicted_counts.csv") %>% 
      filter(model == "Adjusted")

# Forest plot A&E: 
ane_x_setting <- list(c(0.5, 2), c(0.8, 1.5))
ane_forest <- forest_plot_function(ane_binomial, ane_hurdle,
                                   x_axis_setting = ane_x_setting)

# bar plot A&E:
ane_bar  <- visit_bar_fc(predicted_ane_visits, "Average A&E visit frequencies")


# combine and save outputs
ane_plots <- ggarrange(ane_forest, ane_bar, ncol = 1, labels = c("a", "b"))
ggsave(ane_plots, file = "output/st02_07_a_and_e_visits.png",
       width=12, height=5,  units = "in", dpi = 300)


# OPA clinic visits: ------
# read in data
opa_binomial <- read_csv("output/st02_05_opa_binomial.csv") %>% 
      filter(time == "12 months" & Adjustment == "Adjusted OPA visits") %>% 
      mutate(Group = "Long COVID group") %>% 
      dplyr::select(Group, estimate, lci, hci) %>% 
      add_row(
            Group = "Comparator group",
            estimate = 1,
            lci = 1,
            hci = 1) %>% 
      arrange(Group)

opa_hurdle<- read_csv("output/st02_05_opa_hurdle.csv") %>% 
      filter(time == "12 months" & Adjustment == "Adjusted OPA visits") %>% 
      mutate(Group = "Long COVID group") %>% 
      dplyr::select(Group, estimate, lci, hci) %>% 
      add_row(
            Group = "Comparator group",
            estimate = 1,
            lci = 1,
            hci = 1) %>% 
      arrange(Group)

predicted_opa_visits <- read_csv("output/st02_05_opa_predicted_counts.csv") %>% 
      filter(model == "Adjusted")

# Forest plot 

bi_total_visits <- opa_binomial

bi_total_visits$`OR (95% CI)` <- sprintf("%.2f (%.2f - %.2f)",
                                         bi_total_visits$estimate,  
                                         bi_total_visits$lci,  
                                         bi_total_visits$hci)
bi_total_visits$`   First part      `  <- " "
bi_total_visits <- relocate(bi_total_visits, `   First part      `  , .after = Group)
bi_total_visits <- rename(bi_total_visits, `Healthcare utilisation type` = Group)



# Hurdle models:
# Read in results: 
total_hurdle <- opa_hurdle

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

opa_forest <- forest(
      data = combine[,c(1,2,6,7,11)],
      est = list(combine$estimate, combine$estimate2),
      lower = list(combine$lci, combine$lci2),
      upper = list(combine$hci,combine$hci2),
      ci_column = c(2, 4),
      xlim = list(c(0.5, 3), c(0.8, 1.5)),
      ref_line = 1,
      theme = tm)
plot(opa_forest)

# barplot
opa_bar <- visit_bar_fc(predicted_opa_visits, "Average outpatient clinic visit frequencies")


# combine and save outputs
opa_plots <- ggarrange(opa_forest, opa_bar, ncol = 1, labels = c("a", "b"))
ggsave(opa_plots, file = "output/st02_07_opa_visits.png",
       width=12, height=5, units = "in", dpi = 300)

