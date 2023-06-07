# Load previous data management
source("analysis/dm04_matched_estimale_costs.R")

# test running the model 
month_1 <- tpm(
      gp_cost_m1 ~ exposure,
      data = matched_data,
      link_part1 = "logit", family_part2 = Gamma(link = "log")
)

