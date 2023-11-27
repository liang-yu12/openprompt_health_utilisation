# Load all packages
source("analysis/settings_packages.R")

# 1. Read in total N:

total_n <- read_csv("output/dataset_total_n.csv.gz") %>% 
      nrow()


# 2. How many GP use codes: 
gp_use <- read_csv("output/dataset_lc_gp_list.csv") %>% 
      nrow()

# 3. Unmatched comparators: 
# unmatched comparator
total_exclude_gp <- read_csv(here("output", "dataset_comparator_unmatched.csv")) %>% 
                                    nrow()


# Combine the results: 
flow_chart <- data.frame(
      total = total_n,
      gp_use_lc = gp_use,
      total_after_exclusion = total_exclude_gp,
      excluded = total_n - total_exclude_gp
)



flow_chart %>% write_csv(here("output", "qc00_check_exclusion_flow_chart.csv"))
