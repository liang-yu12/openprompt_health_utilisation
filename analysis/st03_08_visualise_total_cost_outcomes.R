# Visualise the outputs

options(digits = 2)

# Load all packages
source("analysis/settings_packages.R")

# read in datasets for plotting:
# Binomial models: ---------------
total_binomial <- read_csv("output/st03_03_total_cost_binary.csv") %>% 
      filter(time == "12m" & model == "Adjusted") %>% 
      mutate(Group = "Long COVID group") %>% 
      dplyr::select(Group, estimate, lci, hci) %>% 
      add_row(
            Group = "Comparator group",
            estimate = 1,
            lci = 1,
            hci = 1) %>% 
      arrange(Group)


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
combine_tpm <- read_csv("output/st03_03_total_cost_gammaglm.csv") %>% 
      filter(time == "12m" & model == "Adjusted"& term == "exposureLong covid exposure") %>% 
      mutate(Group = "Long COVID group") %>% 
      dplyr::select(Group, estimate, lci, hci) %>% 
      add_row(
            Group = "Comparator group",
            estimate = 1,
            lci = 1,
            hci = 1) %>% 
      arrange(Group)

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
plot(two_forest)


# Barplot: -----------
# Read in outcome data:
total_predicted_cost <- read_csv("output/st03_03_total_cost_predicted_costs.csv") 

# change order by assigning the order of the factor
total_predicted_cost$exposure <- factor(total_predicted_cost$exposure, 
                                          levels=c("Long covid exposure","Comparator"))


# Colour blind friendly:
cbp1 <- c("#E66100", "#5D3A9B")


(cost_barplot <- ggplot(total_predicted_cost, aes(fill=exposure, y=cost, x=exposure)) + 
            geom_bar(position="dodge", stat="identity", width = 0.5) +  coord_flip() +
            geom_errorbar(position=position_dodge(0.5), aes(ymin = lci, ymax = uci), width = 0.2) + 
            guides(fill=guide_legend(title="   ")) +
            ylab("Average total healthcare cost (£)") + xlab(" ") + theme_bw() +
            scale_fill_manual(values = cbp1))

# Combine plots together : ----------

all_cost_plots <- ggarrange(two_forest, cost_barplot, ncol = 1)
ggsave(all_cost_plots, file = "output/st03_08_total_cost.png",
       width=9, height=5, units = "in", dpi = 300)


