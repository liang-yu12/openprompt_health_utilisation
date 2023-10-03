# Visualise the outputs

options(digits = 2)

# Load all packages
source("analysis/settings_packages.R")

# read in datasets for plotting:
# Binomial models: ---------------
bi_total_costs <- read_csv("output/st04_01_total_cost_binary.csv") %>% 
      filter(time == "12m" & model == "Adjusted") %>% 
      mutate(Type = "Total healthcare costs")

bi_gp_costs <- read_csv("output/st04_02_gp_cost_binomial_output.csv") %>% 
      filter(adjustment == "Adjusted" & term == "exposureLong covid exposure" & time == "12 months") %>% 
      mutate(Type = "GP costs")

bi_opa_costs <- read_csv("output/st04_05_opa_cost_binomial_output.csv") %>% 
      filter(adjustment == "Adjusted" & term == "exposureLong covid exposure" & time == "12 months") %>% 
      mutate(Type = "Outpatient clinic costs")

bi_hos_admin <- read_csv("output/st04_03_apc_cost_binomial_output.csv") %>% 
      filter(adjustment == "Adjusted" & term == "exposureLong covid exposure" & time == "12 months") %>% 
      mutate(Type = "Hospital admission")

bi_ane_costs <- read_csv("output/st04_04_ane_cost_binomial_output.csv") %>% 
      filter(adjustment == "Adjusted" & term == "exposureLong covid exposure" & time == "12 months") %>% 
      mutate(Type = "A&E costs")

total_binomial <- bind_rows(
      bi_total_costs,
      bi_gp_costs,
      bi_opa_costs,
      bi_hos_admin,
      bi_ane_costs) %>% 
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
      ref_line = 1
))

# tpm models: -------------
# Read in results: 
total_tpm <- read_csv("output/st04_02_gp_cost_twopm_output.csv") %>% 
      filter(time == "12 months" & adjustment == "Adjusted"& term == "exposureLong covid exposure") %>% 
      mutate(Type = "Total healthcare costs")

gp_tpm <- read_csv("output/st04_02_gp_cost_twopm_output.csv")  %>% 
      filter(adjustment == "Adjusted" & term == "exposureLong covid exposure" & time == "12 months") %>% 
      mutate(Type = "GP costs")


hos_tpm <- read_csv("output/st04_03_apc_cost_twopm_output.csv") %>% 
      filter(adjustment == "Adjusted" & term == "exposureLong covid exposure" & time == "12 months") %>% 
      mutate(Type = "Hospital admission")

opa_tpm <- read_csv("output/st04_05_opa_cost_twopm_output.csv") %>% 
      filter(adjustment == "Adjusted" & term == "exposureLong covid exposure" & time == "12 months") %>% 
      mutate(Type = "Outpatient clinic costs")

ane_tpm <- read_csv("output/st04_04_ane_cost_twopm_output.csv") %>% 
      filter(adjustment == "Adjusted" & term == "exposureLong covid exposure" & time == "12 months") %>% 
      mutate(Type = "A&E costs")


# Combine results 
combine_tpm <- bind_rows(
      total_tpm, gp_tpm, opa_tpm, ane_tpm, hos_tpm) %>% 
      dplyr::select(Type, estimate, lci, hci) 

combine_tpm$`Cost ratio (95% CI)` <- sprintf("%.2f (%.2f - %.2f)",
                                                combine_tpm$estimate,  
                                                combine_tpm$lci,  
                                                combine_tpm$hci)
# Create an empty column for plots
combine_tpm$`   Second part      `  <- " "
combine_tpm <- relocate(combine_tpm, `   Second part      ` , .after = Type)
combine_tpm <- rename(combine_tpm, `Healthcare utilisation type` = Type)



# Create a forest plot object
(tpm_forestplot <- forest(
      data = combine_tpm[,c(1,2,6)],
      est = combine_tpm$estimate,
      lower = combine_tpm$lci,
      upper = combine_tpm$hci,
      ci_column = 2,
      xlim = c(0, 4),
      ref_line = 1
))

# Try combining two data---------
part2 <- combine_tpm %>% rename(
      estimate2 = estimate, 
      lci2 = lci,
      hci2 = hci
)
part1 <- total_binomial
combine <- full_join(part1, part2)

combine[,c(1,2,6,7,11)] %>% names


tm <- forest_theme(core=list(bg_params=list(fill = c("#FFFFFF"))))

two_forest <- forest(
      data = combine[,c(1,2,6,7,11)],
      est = list(combine$estimate, combine$estimate2),
      lower = list(combine$lci, combine$lci2),
      upper = list(combine$hci,combine$hci2),
      ci_column = c(2, 4),
      ref_line = 1,
      theme = tm)
plot(two_forest)



# Stacked barplot: -----------
# Read in outcome data:
gp_predicted_cost <- read_csv("output/st04_02_predict_gp_cost_tpm.csv") %>% 
      filter(adjustment == "Adjusted" & time == "12 months") %>% 
      mutate(Type = "GP costs")

opa_predicted_cost <- read_csv("output/st04_05_predict_opa_cost_tpm.csv") %>% 
      filter(adjustment == "Adjusted" & time == "12 months") %>% 
      mutate(Type = "Outpatient clinic costs")

ane_predicted_cost <- read_csv("output/st04_04_predict_ane_cost_tpm.csv") %>% 
      filter(adjustment == "Adjusted" & time == "12 months") %>% 
      mutate(Type = "A&E costs")

hos_predicted_cost <- read_csv("output/st04_03_predict_apc_cost_tpm.csv") %>% 
      filter(adjustment == "Adjusted" & time == "12 months") %>% 
      mutate(Type = "Hospital admission")

all_predicted_stacked <- bind_rows(
      gp_predicted_cost,
      opa_predicted_cost,
      ane_predicted_cost,
      hos_predicted_cost
)
all_predicted_stacked$model <- NULL

# change order by assigning the order of the factor
visit_type <- c("Outpatient clinic costs", "Hospital admission","A&E costs","GP costs")

all_predicted_stacked$Type <- factor(all_predicted_stacked$Type, levels=visit_type)
all_predicted_stacked <- all_predicted_stacked %>% arrange(factor(Type, levels=visit_type)) 

# Colour blind friendly:
cbp1 <- c("#E76F51", "#E9C46A", "#2A9D8F", "#264653")


(costs_barplot <- ggplot(all_predicted_stacked, aes(fill=Type, y=cost, x=exposure)) + 
      geom_bar(position="stack", stat="identity") +  coord_flip() +
      guides(fill=guide_legend(title="Healthcare type")) +
      ylab("Average healthcare costs") + xlab(" ") + theme_bw() +
      scale_fill_manual(values = cbp1))

# Combine plots together : ----------

costs_all_plots <- ggarrange(two_forest, costs_barplot, ncol = 1)
ggsave(costs_all_plots, file = "output/st04_healthcare_costs.png",
       width=12, height=5, units = "in", dpi = 300)
