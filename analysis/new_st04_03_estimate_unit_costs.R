# Data management of unit costs:
# 1. APC unit costs:
source("analysis/qc11_apc_unit_costs.R")

# 2. OPA unit costs:
source("analysis/qc12_opa_unit_costs.R")

# 3. A&E unit costs:
source("analysis/qc13_ane_unit_costs.R")


# new cost datasets:
# 4. new APC data:
source("analysis/qc11_apc__new_unit_costs.R")

# 5. new OPA data:
source("analysis/qc12_opa_new_unit_costs.R")

# 6. new A&E data: 
source("analysis/qc13_ane_new_unit_costs.R")


# Combine the unit costs data for comparison: 

tibble(
      version = c("Previous", "New"),
      apc_unit_costs = c(apc_new_unit_cost$unit_cost, apc_new_unit_cost$unit_cost),
      opa_unit_costs = c(opa_unit_cost$unit_cost, opa_new_unit_cost$unit_cost),
      ane_unit_costs = c(ane_unit_cost$unit_cost, ane_new_unit_cost$unit_cost)
) %>% write_csv(here("output","st04_compare_unit_costs.csv"))


