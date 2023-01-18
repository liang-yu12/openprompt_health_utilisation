library(data.table)
library(magrittr)

# make a list of GP using long COVID diagnosis
gp_list_data <- fread(here::here("output","dataset_lc_gp_list.csv"))



# read in the potential comparator groups 
potential_comparator <- fread("output/dataset_comparator_large.csv") %>% data.table()

# filter data using the list
filtered_potential_comparator <- potential_comparator[gp_practice %in% unlist(gp_list_data$gp_practice)]

# save the output
filtered_potential_comparator %>% fwrite("output/dataset_comparator_filtered_gp.csv")
