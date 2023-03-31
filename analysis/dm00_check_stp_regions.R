# check the STP regions names: 
# load packages:
source("analysis/settings_packages.R")

# gp practices using lc codes
lc_stp_list <- fread(here("output","dataset_exp_lc_unmatched.csv"), select = "region") 


# save the output
lc_stp_list %>% 
      count(region) %>% 
      fwrite(here("output", "stp_regions_counts.csv"))

