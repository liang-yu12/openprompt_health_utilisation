# Source data management
source("analysis/new_dm02_cont_comparison_monthly_follow_up_time_new_cost_data.R")

# manually combine data
matched_data <- bind_rows(lc_exp_matched, com_matched)

costs <- matched_data[grep("_cost_m", names(matched_data))] %>%  names

# summarise follow-up period
fu_cols <- matched_data[grep("follow_up_m", names(matched_data))] %>% 
      names %>% as.vector()  # summarise follow-up time

# define variables for table 1
dependent = "exposure"
explanatory = c("sex", "age", "age_cat", "ethnicity_6", "bmi_cat", "imd_q5", "region",
                "cov_asthma",
                "cov_mental_health",   
                "number_comorbidities_cat",
                "previous_covid_hosp",
                "cov_c19_vaccine_number", 
                "cov_covid_vaccine_number", "cov_covid_vax_n_cat", 
                "long_covid_dx", "covid_positive",
                "reg_and_visit_gp_1y")

# Table 1 reporting numbers:  -----
# basic demographic data
basic_demographic <- matched_data %>% summary_factorlist(dependent, explanatory, 
                                    p = TRUE,
                                    add_row_totals = TRUE,
                                    row_totals_colname = "Total",
                                    cont_cut = 5) 

# Summarising the costs:
outcomes = costs

outcome_summary <- matched_data %>% summary_factorlist(dependent, outcomes,
                                                       p = TRUE, p_cont_para = "aov",
                                                       add_row_totals = TRUE,
                                                       row_totals_colname = "Total",
                                                       cont = "mean",  cont_cut = 0)

bind_rows(basic_demographic, outcome_summary) %>% 
      write.csv(here("output", "new_st01_matched_numbers_table.csv"), row.names = F)
