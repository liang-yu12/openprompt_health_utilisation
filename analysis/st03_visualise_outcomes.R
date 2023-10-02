# Visualise the outputs

options(digits = 2)

# Load all packages
source("analysis/settings_packages.R")

# read in datasets for plotting:
# Binomial models: ---------------
bi_total_visits <- read_csv("output/st03_01_total_binomial.csv") %>% 
      filter(time == "12 months" & Adjustment == "Adjusted") %>% 
      mutate(Type = "Total healthcare visits")

bi_gp_visits <- read_csv("output/st03_02_gp_binomial.csv") %>% filter(Adjustment == "GP Adjusted") %>% 
      mutate(Type = "GP visits")

bi_opa_visits <- read_csv("output/st03_05_opa_binomial.csv") %>% filter(Adjustment == "Adjusted A&E visits") %>% 
      mutate(Type = "Outpatient clinic visits")

bi_hos_admin <- read_csv("output/st03_03_hos_binomial.csv") %>% filter(Adjustment == "Hospital Adjusted") %>% 
      mutate(Type = "Hospital admission")

bi_ane_visits <- read_csv("output/st03_04_ane_binomial.csv") %>% filter(Adjustment == "Adjusted A&E visits") %>% 
      mutate(Type = "A&E visits")

total_binomial <- bind_rows(
      bi_total_visits,
      bi_gp_visits,
      bi_opa_visits,
      bi_hos_admin,
      bi_ane_visits) %>% 
      dplyr::select(Type, estimate, lci, hci) 

# Decrease decimal points


total_binomial$`OR (95% CI)` <- sprintf("%.2f (%.2f - %.2f)",
                                        total_binomial$estimate,  
                                        total_binomial$lci,  
                                        total_binomial$hci)
total_binomial$`   First part      `  <- " "
total_binomial <- relocate(total_binomial, `   First part      `  , .after = Type)
total_binomial <- rename(total_binomial, `Healthcare utilisation type` = Type)

# Create a forest plot object
(bi_forestplot <- forest(
      data = total_binomial[,c(1,2,6)],
      est = total_binomial$estimate,
      lower = total_binomial$lci,
      upper = total_binomial$hci,
      ci_column = 2,
      xlim = c(0, 4),
      ref_line = 1
))

# Hurdle models: -------------
# Read in results: 
total_hurdle <- read_csv("output/st03_01_total_hurdle.csv") %>% 
      filter(time == "12 months" & Adjustment == "Adjusted") %>% 
      mutate(Type = "Total healthcare visits")

gp_hurdle <- read_csv("output/st03_02_gp_hurdle.csv") %>% filter(Adjustment == "GP Adjusted") %>% 
      mutate(Type = "GP visits")

opa_hurdle <- read_csv("output/st03_05_opa_hurdle.csv") %>% filter(Adjustment == "Adjusted A&E visits") %>% 
      mutate(Type = "Outpatient clinic visits")

ane_hurdle <- read_csv("output/st03_04_ane_hurdle.csv") %>% filter(Adjustment == "Adjusted A&E visits") %>% 
      mutate(Type = "A&E visits")

hos_hurdle <- read_csv("output/st03_03_hos_hurdle.csv") %>% filter(Adjustment == "Hospital Adjusted") %>% 
      mutate(Type = "Hospital admission")

# Combine results 
combine_hurdle <- bind_rows(
      total_hurdle, gp_hurdle, opa_hurdle, ane_hurdle, hos_hurdle) %>% 
      dplyr::select(Type, estimate, lci, hci) 

combine_hurdle$`Rate ratio (95% CI)` <- sprintf("%.2f (%.2f - %.2f)",
                                                combine_hurdle$estimate,  
                                                combine_hurdle$lci,  
                                                combine_hurdle$hci)
# Create an empty column for plots
combine_hurdle$`   Second part      `  <- " "
combine_hurdle <- relocate(combine_hurdle, `   Second part      ` , .after = Type)
combine_hurdle <- rename(combine_hurdle, `Healthcare utilisation type` = Type)



# Create a forest plot object
(hurdle_forestplot <- forest(
      data = combine_hurdle[,c(1,2,6)],
      est = combine_hurdle$estimate,
      lower = combine_hurdle$lci,
      upper = combine_hurdle$hci,
      ci_column = 2,
      xlim = c(0, 4),
      ref_line = 1
))

# Try combining two data---------
part2 <- combine_hurdle %>% rename(
      estimate2 = estimate, 
      lci2 = lci,
      hci2 = hci
)
part1 <- total_binomial
combine <- full_join(part1, part2)

combine[,c(1,2,6,7,11)] %>% names

two_forest <- forest(
      data = combine[,c(1,2,6,7,11)],
      est = list(combine$estimate, combine$estimate2),
      lower = list(combine$lci, combine$lci2),
      upper = list(combine$hci,combine$hci2),
      ci_column = c(2, 4),
      ref_line = 1)
plot(two_forest)



# Stacked barplot: -----------
# Read in outcome data:
gp_predicted_counts <- read_csv("output/st03_02_gp_predicted_counts.csv") %>% 
      filter(model == "Adjusted") %>% 
      mutate(Type = "GP visits")

opa_predicted_counts <- read_csv("output/st03_05_opa_predicted_counts.csv") %>% 
      filter(model == "Adjusted") %>% 
      mutate(Type = "Outpatient clinic visits")

ane_predicted_counts <- read_csv("output/st03_04_ane_predicted_counts.csv") %>% 
      filter(model == "Adjusted") %>% 
      mutate(Type = "A&E visits")

hos_predicted_counts <- read_csv("output/st03_03_hos_predicted_counts.csv") %>% 
      filter(model == "Adjusted") %>% 
      mutate(Type = "Hospital admission")

all_predicted_stacked <- bind_rows(
      gp_predicted_counts,
      opa_predicted_counts,
      ane_predicted_counts,
      hos_predicted_counts
)
all_predicted_stacked$model <- NULL

# change order by assigning the order of the factor
visit_type <- c("Outpatient clinic visits", "Hospital admission","A&E visits","GP visits")

all_predicted_stacked$Type <- factor(all_predicted_stacked$Type, levels=visit_type)
all_predicted_stacked <- all_predicted_stacked %>% arrange(factor(Type, levels=visit_type)) 

# Colour blind friendly:
cbp1 <- c("#E76F51", "#E9C46A", "#2A9D8F", "#264653")


(visits_barplot <- ggplot(all_predicted_stacked, aes(fill=Type, y=visits, x=exposure)) + 
      geom_bar(position="stack", stat="identity") +  coord_flip() +
      guides(fill=guide_legend(title="Healthcare sector type")) +
      ylab("Average healthcare visit frequency") + xlab(" ") +
      scale_fill_manual(values = cbp1))

# Combine plots together : ----------

visits_all_plots <- ggarrange(two_forest, visits_barplot, ncol = 1)
ggsave(visits_all_plots, file = "output/st03_healthcare_visits.png",width=12, height=4)
