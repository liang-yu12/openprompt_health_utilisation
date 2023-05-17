# Load previous data management
source("analysis/dm03_matched_define_monthly_follow_up_time.R")

# test running the model 
month_1 <- tpm(
      all_month1 ~ exposure + sex + age_cat + ethnicity_6 + bmi_cat + imd_q5,
      data = matched_data,
      link_part1 = "logit",
      family_part2 = "poisson"
)

matched_data %<>% as_tibble()

t <- c("exposure", "sex", "age_cat", "ethnicity_6", "bmi_cat", "imd_q5","follow_up_m1","all_month1")
map(matched_data[,t], summary)

test_data <- matched_data[1:20,]


month_1 <- tpm(all_month1~exposure, 
               data = matched_data,
               link_part1 = "logit",
               family_part2 = poisson
)

# noted that tpm won't run if there is NA in the data; 
# will need to manually exclude NA