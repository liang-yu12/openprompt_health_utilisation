# Load data management files
source("analysis/dm02_04_combine_long_data_for_did.R")

# report numbers: 
did_data_12m %>% summary_factorlist(
      dependent = "exposure",
      explanatory = c("sex", "age_cat", "ethnicity_6", "bmi_cat", "imd_q5", 
                      "region", "number_comorbidities_cat"),
      p = TRUE
) %>% write.csv(here("output", "st05_hx_matched_numbers_table.csv"), row.names = F)


did_data_12m %>% group_by(exposure, time) %>% 
      summarise(mean_visits = mean(visits), sd=sd(visits)) %>% 
      as.data.frame() %>% 
      write.csv(here("output", "st05_hx_crude_vistis_exp_time.csv"), row.names = F)







