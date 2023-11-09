# Visualise the outputs

options(digits = 2, scipen = 999)

# Load all packages
source("analysis/settings_packages.R")

# read in data
cost_data <- read_csv("output/qc_cost_by_year.csv.gz")

# write a function to summarise the results
description_fn <- function(var, cost_table, time){
      results <- cost_data %>% 
            summarise(
                  minimal = min(apc_cost_1y, na.rm = T),
                  median = median(apc_cost_1y, na.rm = T),
                  mean = mean(apc_cost_1y, na.rm = T),
                  max = max(apc_cost_1y, na.rm = T)
            ) %>% mutate(Table = cost_table,
                         Time = time) %>% 
            relocate(Table, Time)
      return(results)
}


# Summarise and combine the results
bind_rows(
      description_fn(cost_data$apc_cost_1y, "APC", "1 year"),
      description_fn(cost_data$apc_cost_2y, "APC", "2 year"),
      description_fn(cost_data$apc_cost_total, "APC", "total"),
      description_fn(cost_data$ec_cost_1y, "EC", "1 year"),
      description_fn(cost_data$ec_cost_2y, "EC", "2 year"),
      description_fn(cost_data$ec_cost_total, "EC", "total"),
      description_fn(cost_data$opa_cost_1y, "OPA", "1 year"),
      description_fn(cost_data$opa_cost_2y, "OPA", "2 year"),
      description_fn(cost_data$opa_cost_total, "OPA", "total")
) %>% write_csv("output/qc_03_00_cost_data_desc_stat.csv")
