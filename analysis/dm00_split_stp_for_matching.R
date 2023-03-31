# Data management: splitting exposure groups and comparator groups by the 
# STP regions. 
# load packages
source(here::here("analysis","settings_packages.R"))

# 1. splitting exposure datasets: ----

# Step 1: Read in the csv file using fread
dataset_exp_lc_unmatched <- fread(here::here("output","dataset_exp_lc_unmatched.csv"))


to_be_dates <- c("registration_date", "long_covid_dx_date" , "index_date" ,
                 "end_1y_after_index", "end_death", "end_lc_cure", "bmi_date")

dataset_exp_lc_unmatched[,(to_be_dates) := lapply(.SD, as.Date), .SDcols = to_be_dates]


# Step 2: Split the data.table by a variable level using data.table::split()
exp_split_list <- split(dataset_exp_lc_unmatched, by = "region")

# save the original titles for checking:
data.frame(
      stp_names = names(exp_split_list),
      numbers = str_sub(names(exp_split_list),-2, -1)
) %>% as.list() %>% 
      fwrite(here("output", "exp_stp_names_numbers.csv"))

names(exp_split_list) <- str_sub(names(exp_split_list),-2, -1) # rename the list for saving

# Step 3: Save each split data.table to csv files using write.csv()
lapply(names(exp_split_list), function(x) {
      fwrite(as.list(exp_split_list[[x]]), file = here("output", paste0("exp_stp_", x, ".csv")),
             row.names = FALSE)
})

# 2. Split comparator unmatched dataset ----

# Step 1: Read in the csv file using fread
dataset_comparator_unmatched <- fread(here::here("output", "dataset_comparator_unmatched.csv"))

to_be_dates2 <- c("registration_date", "long_covid_dx_date" , 
                 "end_death", "bmi_date")


dataset_comparator_unmatched[,(to_be_dates2) := lapply(.SD, as.Date), .SDcols = to_be_dates2]


# Step 2: Split the data.table by a variable level using data.table::split()
com_split_list <- split(dataset_comparator_unmatched, by = "region")

# save the original titles for checking:
data.frame(
      stp_names = names(com_split_list),
      numbers = str_sub(names(com_split_list),-2, -1)
) %>% as.list() %>% 
      fwrite(here("output", "com_stp_names_numbers.csv"))

       
names(com_split_list) <- str_sub(names(com_split_list),-2,-1)

# Step 3: Save each split data.table to csv files using write.csv()
lapply(names(com_split_list), function(x) {
      fwrite(as.list(com_split_list[[x]]), file = here("output", paste0("com_stp_", x, ".csv")),
                row.names = FALSE)
})

