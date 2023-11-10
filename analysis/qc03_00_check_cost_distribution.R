# Visualise the outputs

options(digits = 2, scipen = 999)

# Load all packages
source("analysis/settings_packages.R")

# read in data
cost_data <- read_csv("output/qc_cost_by_year.csv.gz")

# APC
apc_cost_1y <- cost_data %>% filter(apc_cost_1y> 0 & !is.na(apc_cost_1y)) %>% 
      summarise(
            minimal = min(apc_cost_1y, na.rm = T),
            median = median(apc_cost_1y, na.rm = T),
            mean = mean(apc_cost_1y, na.rm = T),
            max = max(apc_cost_1y, na.rm = T),
            N = sum(apc_cost_1y>0, na.rm = T)
      ) %>% mutate(Table = "APC",
                   Time = "1Y") %>% relocate(Table, Time) 
      
apc_cost_1y_count <- cost_data %>% 
      summarise(zero_count = sum(apc_cost_1y==0, na.rm = T),
                missing_count = sum(is.na(apc_cost_1y), na.rm = T),
                total_n = n())

apc_cost_2y <-  cost_data %>% filter(apc_cost_2y> 0 & !is.na(apc_cost_2y)) %>% 
      summarise(
            minimal = min(apc_cost_2y, na.rm = T),
            median = median(apc_cost_2y, na.rm = T),
            mean = mean(apc_cost_2y, na.rm = T),
            max = max(apc_cost_2y, na.rm = T),
            N = sum(apc_cost_2y>0, na.rm = T)
      ) %>% mutate(Table = "APC",
                   Time = "2Y") %>% relocate(Table, Time) 

apc_cost_2y_count <- cost_data %>% 
      summarise(zero_count = sum(apc_cost_2y==0, na.rm = T),
                missing_count = sum(is.na(apc_cost_2y), na.rm = T),
                total_n = n())

apc_cost_total <-  cost_data %>% filter(apc_cost_total> 0 & !is.na(apc_cost_total)) %>% 
      summarise(
            minimal = min(apc_cost_total, na.rm = T),
            median = median(apc_cost_total, na.rm = T),
            mean = mean(apc_cost_total, na.rm = T),
            max = max(apc_cost_total, na.rm = T),
            N = sum(apc_cost_total>0, na.rm = T)
      ) %>% mutate(Table = "APC",
                   Time = "Total") %>% relocate(Table, Time) 

apc_cost_total_count <- cost_data %>% 
      summarise(zero_count = sum(apc_cost_total==0, na.rm = T),
                missing_count = sum(is.na(apc_cost_total), na.rm = T),
                total_n = n())

# A&E
ec_cost_1y <- cost_data %>% filter(ec_cost_1y> 0 & !is.na(ec_cost_1y)) %>% 
      summarise(
            minimal = min(ec_cost_1y, na.rm = T),
            median = median(ec_cost_1y, na.rm = T),
            mean = mean(ec_cost_1y, na.rm = T),
            max = max(ec_cost_1y, na.rm = T),
            N = sum(ec_cost_1y>0, na.rm = T)
      ) %>% mutate(Table = "EC",
                   Time = "1Y") %>% relocate(Table, Time) 

ec_cost_1y_count <- cost_data %>% 
      summarise(zero_count = sum(ec_cost_1y==0, na.rm = T),
                missing_count = sum(is.na(ec_cost_1y), na.rm = T),
                total_n = n())

ec_cost_2y <-  cost_data %>% filter(ec_cost_2y> 0 & !is.na(ec_cost_2y)) %>% 
      summarise(
            minimal = min(ec_cost_2y, na.rm = T),
            median = median(ec_cost_2y, na.rm = T),
            mean = mean(ec_cost_2y, na.rm = T),
            max = max(ec_cost_2y, na.rm = T),
            N = sum(ec_cost_2y>0, na.rm = T)
      ) %>% mutate(Table = "EC",
                   Time = "2Y") %>% relocate(Table, Time) 

ec_cost_2y_count <- cost_data %>% 
      summarise(zero_count = sum(ec_cost_2y==0, na.rm = T),
                missing_count = sum(is.na(ec_cost_2y), na.rm = T),
                total_n = n())

ec_cost_total <-  cost_data %>% filter(ec_cost_total> 0 & !is.na(ec_cost_total)) %>% 
      summarise(
            minimal = min(ec_cost_total, na.rm = T),
            median = median(ec_cost_total, na.rm = T),
            mean = mean(ec_cost_total, na.rm = T),
            max = max(ec_cost_total, na.rm = T),
            N = sum(ec_cost_total>0, na.rm = T)
      ) %>% mutate(Table = "EC",
                   Time = "Total") %>% relocate(Table, Time) 

ec_cost_total_count <- cost_data %>% 
      summarise(zero_count = sum(ec_cost_total==0, na.rm = T),
                missing_count = sum(is.na(ec_cost_total), na.rm = T),
                total_n = n())

# OPA: 
opa_cost_1y <- cost_data %>% filter(opa_cost_1y> 0 & !is.na(opa_cost_1y)) %>% 
      summarise(
            minimal = min(opa_cost_1y, na.rm = T),
            median = median(opa_cost_1y, na.rm = T),
            mean = mean(opa_cost_1y, na.rm = T),
            max = max(opa_cost_1y, na.rm = T),
            N = sum(opa_cost_1y>0, na.rm = T)
      ) %>% mutate(Table = "OPA",
                   Time = "1Y") %>% relocate(Table, Time) 

opa_cost_1y_count <- cost_data %>% 
      summarise(zero_count = sum(opa_cost_1y==0, na.rm = T),
                missing_count = sum(is.na(opa_cost_1y), na.rm = T),
                total_n = n())

opa_cost_2y <-  cost_data %>% filter(opa_cost_2y> 0 & !is.na(opa_cost_2y)) %>% 
      summarise(
            minimal = min(opa_cost_2y, na.rm = T),
            median = median(opa_cost_2y, na.rm = T),
            mean = mean(opa_cost_2y, na.rm = T),
            max = max(opa_cost_2y, na.rm = T),
            N = sum(opa_cost_2y>0, na.rm = T)
      ) %>% mutate(Table = "OPA",
                   Time = "2Y") %>% relocate(Table, Time) 

opa_cost_2y_count <- cost_data %>% 
      summarise(zero_count = sum(opa_cost_2y==0, na.rm = T),
                missing_count = sum(is.na(opa_cost_2y), na.rm = T),
                total_n = n())

opa_cost_total <-  cost_data %>% filter(opa_cost_total> 0 & !is.na(opa_cost_total)) %>% 
      summarise(
            minimal = min(opa_cost_total, na.rm = T),
            median = median(opa_cost_total, na.rm = T),
            mean = mean(opa_cost_total, na.rm = T),
            max = max(opa_cost_total, na.rm = T),
            N = sum(opa_cost_total>0, na.rm = T)
      ) %>% mutate(Table = "OPA",
                   Time = "Total") %>% relocate(Table, Time) 

opa_cost_total_count <- cost_data %>% 
      summarise(zero_count = sum(opa_cost_total==0, na.rm = T),
                missing_count = sum(is.na(opa_cost_total), na.rm = T),
                total_n = n())

bind_rows(
      bind_cols(apc_cost_1y, apc_cost_1y_count),
      bind_cols(apc_cost_2y, apc_cost_2y_count),
      bind_cols(apc_cost_total, apc_cost_total_count),
      bind_cols(ec_cost_1y, ec_cost_1y_count),
      bind_cols(ec_cost_2y, ec_cost_2y_count),
      bind_cols(ec_cost_total, ec_cost_total_count),
      bind_cols(opa_cost_1y, opa_cost_1y_count),
      bind_cols(opa_cost_2y, opa_cost_2y_count),
      bind_cols(opa_cost_total, opa_cost_total_count)
) %>% write_csv("output/qc03_00_cost_data_desc_stat.csv")