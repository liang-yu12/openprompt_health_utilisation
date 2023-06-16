# Load previous data management
source("analysis/dm03_matched_define_monthly_follow_up_time.R")

# multiply the average costs:
# ref: https://doi.org/10.22024/UniKent%2F01.02.100519
# £42 per consultation
matched_data <- matched_data %>%
      mutate(gp_cost_m1 = gp_visit_m1 * 42,
             gp_cost_m2 = gp_visit_m2 * 42,
             gp_cost_m3 = gp_visit_m3 * 42,
             gp_cost_m4 = gp_visit_m4 * 42,
             gp_cost_m5 = gp_visit_m5 * 42,
             gp_cost_m6 = gp_visit_m6 * 42,
             gp_cost_m7 = gp_visit_m7 * 42,
             gp_cost_m8 = gp_visit_m8 * 42,
             gp_cost_m9 = gp_visit_m9 * 42,
             gp_cost_m10 = gp_visit_m10 * 42,
             gp_cost_m11 = gp_visit_m11 * 42,
             gp_cost_m12 = gp_visit_m12 * 42)

