library(data.table)
library(magrittr)

# make a list of GP using long COVID diagnosis
gp_list <- fread("./output/dataset_lc_gp_list.csv", select = "gp_practice") %>% 
      unlist()

# read in the potential comparator groups 
potential_comparator <- fread("./output/dataset_comparator_large.csv") %>% data.table()
filtered_potential_comparator <- potential_comparator[gp_practice %in% gp_list]

# save the output
filtered_potential_comparator %>% fwrite("./output/dataset_comparator_filtered_gp.csv")
