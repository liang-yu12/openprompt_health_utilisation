# check the STP regions names: 
# load packages:
source("analysis/settings_packages.R")

# gp practices using lc codes
lc_gp_list <- fread(here("output","dataset_lc_gp_list.csv")) 

# save the output
lc_gp_list %>% 
      count(gp_practice) %>% 
      fwrite(here("output", "gp_list.csv"))

