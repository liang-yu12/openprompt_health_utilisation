# Source data management
source("analysis/new_dm02_cont_comparison_monthly_follow_up_time_new_cost_data.R")

# manually combine data
matched_data <- bind_rows(lc_exp_matched, com_matched)

# define LC outpatient visits variables
opa_lc_visit <- matched_data[92:103] %>% names() %>% as.vector()
# define total visits variables
visit_cols <- matched_data[grep("all_month_", names(matched_data))] %>% 
      names() %>% as.vector() # summarise the healthcare visit counts
# define gp visits vectors
gp_visits <- matched_data[grep("gp_visit_m", names(matched_data))] %>% 
      names %>% as.vector()
# hostpital visits
hos_visits <- matched_data[grep("hos_visit_m", names(matched_data))] %>% 
      names %>% as.vector()
# A&E visits
a_e_visits <- matched_data[grep("ae_visit_m", names(matched_data))] %>% 
      names %>% as.vector()
# OPD visits
opd_visits <- matched_data[grep("opa_visit_m", names(matched_data))] %>% 
      names %>% as.vector()

opa_lc <- c()
for (i in 1:12){
      opa_lc <- c(opa_lc, paste0("opa_lc_visit_m", i))
}

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
                "long_covid_dx", "covid_positive"
                )

# Table 1 reporting numbers:  -----
# basic demographic data
basic_demographic <- matched_data %>% summary_factorlist(dependent, explanatory, 
                                    p = TRUE,
                                    add_row_totals = TRUE,
                                    row_totals_colname = "Total",
                                    cont_cut = 5) 


# Summarising the outcomes by types:
outcomes <- c(visit_cols, gp_visits, hos_visits, a_e_visits, opd_visits, opa_lc, fu_cols, costs)

outcome_summary <- matched_data %>% summary_factorlist(dependent, outcomes,
                                                       p = TRUE, p_cont_para = "aov",
                                                       add_row_totals = TRUE,
                                                       row_totals_colname = "Total",
                                                       cont = "mean",  cont_cut = 0)

bind_rows(basic_demographic, outcome_summary) %>% 
      write.csv(here("output", "st01_matched_numbers_table.csv"), row.names = F)


matched_data %>% group_by(exposure) %>% 
      summarise(
            zero_count_m1 = sum(follow_up_m1==0, na.rm=T),
            zero_count_m2 = sum(follow_up_m2 == 0, na.rm=T),
            zero_count_m3 = sum(follow_up_m3 == 0, na.rm=T),
            zero_count_m4 = sum(follow_up_m4 == 0, na.rm=T),
            zero_count_m5 = sum(follow_up_m5 == 0, na.rm=T),
            zero_count_m6 = sum(follow_up_m6 == 0, na.rm=T),
            zero_count_m7 = sum(follow_up_m7 == 0, na.rm=T),
            zero_count_m8 = sum(follow_up_m8 == 0, na.rm=T),
            zero_count_m9 = sum(follow_up_m9 == 0, na.rm=T),
            zero_count_m10 = sum(follow_up_m10 == 0, na.rm=T),
            zero_count_m11 = sum(follow_up_m11 == 0, na.rm=T),
            zero_count_m12 = sum(follow_up_m12 == 0, na.rm=T)
      ) %>% 
write.csv(here("output", "st01_matched_numbers_check_fu.csv"), row.names = F)



# Exploring the distribution of vaccine number and index dates
plot_vaccine_distribution <- function(x) {
      # Filter the data by exposure.
      bind_rows((matched_data %>% 
            filter(cov_covid_vax_n_cat == x) %>% 
            filter(exposure == "Comparator") %>% 
            group_by(index_date) %>% count %>% 
            mutate(exposure = "Comparator")),
            (matched_data %>% 
            filter(cov_covid_vax_n_cat == x) %>% 
            filter(exposure == "Long covid exposure") %>% 
            group_by(index_date) %>% count %>% 
            mutate(exposure = "Long COVID exposure"))) %>% 
            ggplot(aes(x=index_date, y=n, color = exposure)) +
            geom_histogram(stat = "identity") +
            ggtitle(x) +
            xlab("Index dates") + ylab("Number") 
}

a_0dose <- plot_vaccine_distribution("0 dose")
b_1dose <- plot_vaccine_distribution("1 dose")
c_2dose <- plot_vaccine_distribution("2 doses")
d_3dose <- plot_vaccine_distribution("3 or more doses")

# combine and save the outputs in a picture
png(file=here("output", "st1_exporing_vax_index_date.png"),
    width=800, height=1200)
ggarrange(a_0dose,b_1dose,c_2dose, d_3dose,
          ncol = 1, nrow = 4) 
dev.off()


# check missing value distribution:------
matched_data %>% 
      missing_glimpse(dependent, explanatory) %>% 
      write.csv(here("output", "missing_distribution_table.csv"), row.names = F)


# check missing pattern:
png(file=here("output", "missing_pattern_current.png"),
    width=600, height=600)
matched_data %>% 
      missing_pattern(dependent, explanatory)
dev.off()
