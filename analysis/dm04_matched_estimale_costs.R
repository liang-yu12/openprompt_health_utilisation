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

matched_data <- matched_data %>% mutate(
  gi_cost_1 = gi_drug_1 * 5.71,
  gi_cost_2 = gi_drug_2 * 5.71,
  gi_cost_3 = gi_drug_3 * 5.71,
  gi_cost_4 = gi_drug_4 * 5.71,
  gi_cost_5 = gi_drug_5 * 5.71,
  gi_cost_6 = gi_drug_6 * 5.71,
  gi_cost_7 = gi_drug_7 * 5.71,
  gi_cost_8 = gi_drug_8 * 5.71,
  gi_cost_9 = gi_drug_9 * 5.71,
  gi_cost_10 = gi_drug_10 * 5.71,
  gi_cost_11 = gi_drug_11 * 5.71,
  gi_cost_12 = gi_drug_12 * 5.71,
  cv_cost_1 = cv_drug_1 * 4.84,
  cv_cost_2 = cv_drug_2 * 4.84,
  cv_cost_3 = cv_drug_3 * 4.84,
  cv_cost_4 = cv_drug_4 * 4.84,
  cv_cost_5 = cv_drug_5 * 4.84,
  cv_cost_6 = cv_drug_6 * 4.84,
  cv_cost_7 = cv_drug_7 * 4.84,
  cv_cost_8 = cv_drug_8 * 4.84,
  cv_cost_9 = cv_drug_9 * 4.84,
  cv_cost_10 = cv_drug_10 * 4.84,
  cv_cost_11 = cv_drug_11 * 4.84,
  cv_cost_12 = cv_drug_12 * 4.84,
  chest_cost_1 = chest_drug_1 * 14.47,
  chest_cost_2 = chest_drug_2 * 14.47,
  chest_cost_3 = chest_drug_3 * 14.47,
  chest_cost_4 = chest_drug_4 * 14.47,
  chest_cost_5 = chest_drug_5 * 14.47,
  chest_cost_6 = chest_drug_6 * 14.47,
  chest_cost_7 = chest_drug_7 * 14.47,
  chest_cost_8 = chest_drug_8 * 14.47,
  chest_cost_9 = chest_drug_9 * 14.47,
  chest_cost_10 = chest_drug_10 * 14.47,
  chest_cost_11 = chest_drug_11 * 14.47,
  chest_cost_12 = chest_drug_12 * 14.47,
  CNS_cost_1 = CNS_drug_1 * 6.93,
  CNS_cost_2 = CNS_drug_2 * 6.93,
  CNS_cost_3 = CNS_drug_3 * 6.93,
  CNS_cost_4 = CNS_drug_4 * 6.93,
  CNS_cost_5 = CNS_drug_5 * 6.93,
  CNS_cost_6 = CNS_drug_6 * 6.93,
  CNS_cost_7 = CNS_drug_7 * 6.93,
  CNS_cost_8 = CNS_drug_8 * 6.93,
  CNS_cost_9 = CNS_drug_9 * 6.93,
  CNS_cost_10 = CNS_drug_10 * 6.93,
  CNS_cost_11 = CNS_drug_11 * 6.93,
  CNS_cost_12 = CNS_drug_12 * 6.93,
  
  
  
)
