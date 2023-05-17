# Load previous data management
source("analysis/dm03_matched_define_followup_time.R")

# test running the model 
month_1 <- tpm(
      month_1 ~ sex + age_cat + ethnicity_6 + bmi_cat + imd_q5,
      data = matched_data,
      link_part1 = "logit",
      family_part2 = poisson
)

# # create a function for monthly healthcare utilisation
# two_part_model_hc_visit_month_fn <- function(month){
#       tpm(
#             month ~ sex + age_cat + ethnicity_6 + bmi_cat + imd_q5,
#             data = matched_data,
#             link_part1 = "logit",
#             family_part2 = poisson
#       )
# }