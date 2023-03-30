# Data management: splitting exposure groups and comparator groups by the 
# STP regions. 
# load packages
source(here::here("analysis","settings_packages.R"))

# 1. splitting exposure datasets: ----

# Step 1: Read in the csv file using fread
dataset_exp_lc_unmatched <- fread(here::here("output","dataset_exp_lc_unmatched.csv"))

# Step 2: Split the data.table by a variable level using data.table::split()
exp_split_list <- split(dataset_exp_lc_unmatched, by = "region")

# save the original titles for checking:
data.frame(
      stp_names = names(exp_split_list),
      numbers = seq_along(exp_split_list)
) %>% as.list() %>% 
      fwrite(here("output", "exp_stp_names_numbers.csv"))

names(exp_split_list) <- seq_along(exp_split_list) # rename the list for saving

# # Step 3: Save each split data.table to csv files using write.csv()
# lapply(names(exp_split_list), function(x) {
#       fwrite(as.list(exp_split_list[[x]]), file = here("output", paste0("exp_stp_", x, ".csv")), 
#              row.names = FALSE)
# })

# 2. Split comparator unmatched dataset ----

# Step 1: Read in the csv file using fread
dataset_comparator_unmatched <- fread(here::here("output", "dataset_comparator_unmatched.csv"))

# Step 2: Split the data.table by a variable level using data.table::split()
comp_split_list <- split(dataset_comparator_unmatched, by = "region")

# save the original titles for checking:
data.frame(
      stp_names = names(comp_split_list),
      numbers = seq_along(comp_split_list)
) %>% as.list() %>% 
      fwrite(here("output", "com_stp_names_numbers.csv"))

       
# names(comp_split_list) <- seq_along(comp_split_list)
# 
# # Step 3: Save each split data.table to csv files using write.csv()
# lapply(names(comp_split_list), function(x) {
#       fwrite(as.list(comp_split_list[[x]]), file = here("output", paste0("comp_stp_", x, ".csv")), 
#                 row.names = FALSE)
# })

