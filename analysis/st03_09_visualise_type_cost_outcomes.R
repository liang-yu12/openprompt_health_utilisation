# Visualise the outputs

options(digits = 2)

# Load all packages
source("analysis/settings_packages.R")

# Write functions to wrap the plotting codes:
# Function to add reference group:
add_ref_fn <- function(input){
      data <- input %>% 
            mutate(Group = "Long COVID group") %>% 
            dplyr::select(Group, estimate, lci, hci) %>% 
            add_row(
                  Group = "Comparator group",
                  estimate = 1,
                  lci = 1,
                  hci = 1) %>% 
            arrange(Group)
      return(data)
}

# Function to run a forest plot:
cost_forest_fn <- function(bi_data, tpm_data){
      # read in datasets for plotting:
      # Binomial models: ---------------
      total_binomial <- bi_data
      
      # Decrease decimal points
      
      
      total_binomial$`OR (95% CI)` <- sprintf("%.2f (%.2f - %.2f)",
                                              total_binomial$estimate,  
                                              total_binomial$lci,  
                                              total_binomial$hci)
      total_binomial$`   First part      `  <- " "
      total_binomial <- relocate(total_binomial, `   First part      `  , .after = Group)
      total_binomial <- rename(total_binomial, `Healthcare utilisation type` = Group)
      
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
      combine_tpm <- tpm_data
      
      combine_tpm$`Cost ratio (95% CI)` <- sprintf("%.2f (%.2f - %.2f)",
                                                   combine_tpm$estimate,  
                                                   combine_tpm$lci,  
                                                   combine_tpm$hci)
      # Create an empty column for plots
      combine_tpm$`   Second part      `  <- " "
      combine_tpm <- relocate(combine_tpm, `   Second part      ` , .after = Group)
      combine_tpm <- rename(combine_tpm, `Healthcare utilisation type` = Group)
      
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
      
      return(two_forest)
}



# read in datasets for plotting:
# Binomial models: ---------------

bi_gp_costs <- read_csv("output/st03_04_now_gp_cost_binomial_output.csv") %>% 
      filter(adjustment == "Adjusted" & term == "exposureLong covid exposure" & time == "12 months") %>% 
      add_ref_fn()

bi_apc_admin <- read_csv("output/st03_05_now_apc_cost_binomial_output.csv") %>% 
      filter(adjustment == "Adjusted" & term == "exposureLong covid exposure" & time == "12 months")  %>% 
      add_ref_fn()

bi_ane_costs <- read_csv("output/st03_06_ane_cost_binomial_output.csv") %>% 
      filter(adjustment == "Adjusted" & term == "exposureLong covid exposure" & time == "12 months") %>% 
      add_ref_fn()

bi_opa_costs <- read_csv("output/st03_07_opa_cost_binomial_output.csv") %>% 
      filter(adjustment == "Adjusted" & term == "exposureLong covid exposure" & time == "12 months")  %>% 
      add_ref_fn()


# tpm models: -------------
# Read in tpm results: 
gp_tpm <- read_csv("output/st03_04_now_gp_cost_twopm_output.csv")  %>% 
      filter(adjustment == "Adjusted" & term == "exposureLong covid exposure" & time == "12 months") %>% 
      add_ref_fn()

apc_tpm <- read_csv("output/st03_05_now_apc_cost_twopm_output.csv") %>% 
      filter(adjustment == "Adjusted" & term == "exposureLong covid exposure" & time == "12 months")  %>% 
      add_ref_fn()

ane_tpm <- read_csv("output/st03_06_ane_cost_twopm_output.csv") %>% 
      filter(adjustment == "Adjusted" & term == "exposureLong covid exposure" & time == "12 months") %>% 
      add_ref_fn()

opa_tpm <- read_csv("output/st03_07_opa_cost_twopm_output.csv") %>% 
      filter(adjustment == "Adjusted" & term == "exposureLong covid exposure" & time == "12 months") %>% 
      add_ref_fn()


# Run forest plots functions: 
# GP
gp_forest <- cost_forest_fn(bi_gp_costs, gp_tpm)

# Hospital APC:
apc_forest <- cost_forest_fn(bi_apc_admin, apc_tpm)

# A&E:
ane_forest <- cost_forest_fn(bi_ane_costs, ane_tpm)

# Outpatient clinic
opa_forest <- cost_forest_fn(bi_opa_costs, opa_tpm)




# Barplot: -----------
# Read in outcome data:
gp_predicted_cost <- read_csv("output/st03_04_predict_gp_cost_tpm.csv") %>% 
      filter(adjustment == "Adjusted" & time == "12 months")

opa_predicted_cost <- read_csv("output/st04_05_predict_opa_cost_tpm.csv") %>% 
      filter(adjustment == "Adjusted" & time == "12 months") %>% 
      mutate(Type = "Outpatient clinic costs")

ane_predicted_cost <- read_csv("output/st04_04_predict_ane_cost_tpm.csv") %>% 
      filter(adjustment == "Adjusted" & time == "12 months") %>% 
      mutate(Type = "A&E costs")

hos_predicted_cost <- read_csv("output/st04_03_predict_apc_cost_tpm.csv") %>% 
      filter(adjustment == "Adjusted" & time == "12 months") %>% 
      mutate(Type = "Hospital admission")


# Colour blind friendly:
cbp1 <- c("#E66100", "#5D3A9B")


(costs_barplot <- ggplot(gp_predicted_cost, aes(fill=exposure, y=cost, x=exposure)) + 
      geom_bar(position="stack", stat="identity") +  coord_flip() +
      guides(fill=guide_legend(title="Healthcare type")) +
      ylab("Average healthcare costs") + xlab(" ") + theme_bw() +
      scale_fill_manual(values = cbp1))

# Combine plots together : ----------

costs_all_plots <- ggarrange(two_forest, costs_barplot, ncol = 1)
ggsave(costs_all_plots, file = "output/st04_healthcare_costs.png",
       width=12, height=5, units = "in", dpi = 300)
