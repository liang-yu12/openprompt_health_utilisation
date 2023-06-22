# Load data management files
source("analysis/dm02_matched_hx_data.R")

fu_time <- c("fu_time_m1", "fu_time_m2","fu_time_m3", "fu_time_m4", 
             "fu_time_m5", "fu_time_m6", "fu_time_m7", "fu_time_m8",
             "fu_time_m9", "fu_time_m10", "fu_time_m11", "fu_time_m12")

all_visits <- c("all_month1", "all_month2", "all_month3", "all_month4", "all_month5", "all_month6", "all_month7", "all_month8", "all_month9", "all_month10", 
             "all_month11", "all_month12")

# report numbers: 
hx_matched_data %>% summary_factorlist(
      dependent = "exposure",
      explanatory = c("sex", "age_cat", "ethnicity_6", "bmi_cat", "imd_q5", 
                      "region",
                      "number_comorbidities_cat", fu_time, all_visits
                  ),
      p = TRUE
) %>% write.csv(here("output", "st02_hx_matched_numbers_table.csv"), row.names = F)


