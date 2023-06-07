# Load previous data management
source("analysis/dm03_matched_define_monthly_follow_up_time.R")

# data management: caclulate accumulated GP visits counts

matched_data$gp_visit_m2 <- matched_data$gp_visit_m1 + matched_data$gp_visit_m2
matched_data$gp_visit_m3 <- matched_data$gp_visit_m2 + matched_data$gp_visit_m3
matched_data$gp_visit_m4 <- matched_data$gp_visit_m3 + matched_data$gp_visit_m4
matched_data$gp_visit_m5 <- matched_data$gp_visit_m4 + matched_data$gp_visit_m5
matched_data$gp_visit_m6 <- matched_data$gp_visit_m5 + matched_data$gp_visit_m6
matched_data$gp_visit_m7 <- matched_data$gp_visit_m6 + matched_data$gp_visit_m7
matched_data$gp_visit_m8 <- matched_data$gp_visit_m7 + matched_data$gp_visit_m8
matched_data$gp_visit_m9 <- matched_data$gp_visit_m8 + matched_data$gp_visit_m9
matched_data$gp_visit_m10 <- matched_data$gp_visit_m9 + matched_data$gp_visit_m10
matched_data$gp_visit_m11 <- matched_data$gp_visit_m10 + matched_data$gp_visit_m11
matched_data$gp_visit_m12 <- matched_data$gp_visit_m11 + matched_data$gp_visit_m12

# multiply the average costs:
# ref: https://doi.org/10.22024/UniKent%2F01.02.100519
# £42

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

