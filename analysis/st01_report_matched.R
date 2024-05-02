# Source data management
source("analysis/dm01_02_now_monthly_follow_up.R")

# manually combine data
matched_data <- bind_rows(lc_exp_matched, com_matched)


# Additional data management of cost data: 
# set cost = 0 if visit == 0; cost = existing cost if visit !=0
# APC costs:
matched_data <- matched_data %>% 
      mutate(apc_cost_m1 = ifelse(hos_visit_m1!=0, apc_cost_m1, 0)) %>% 
      mutate(apc_cost_m2 = ifelse(hos_visit_m2!=0, apc_cost_m2, 0)) %>%
      mutate(apc_cost_m3 = ifelse(hos_visit_m3!=0, apc_cost_m3, 0)) %>%
      mutate(apc_cost_m4 = ifelse(hos_visit_m4!=0, apc_cost_m4, 0)) %>%
      mutate(apc_cost_m5 = ifelse(hos_visit_m5!=0, apc_cost_m5, 0)) %>%
      mutate(apc_cost_m6 = ifelse(hos_visit_m6!=0, apc_cost_m6, 0)) %>%
      mutate(apc_cost_m7 = ifelse(hos_visit_m7!=0, apc_cost_m7, 0)) %>%
      mutate(apc_cost_m8 = ifelse(hos_visit_m8!=0, apc_cost_m8, 0)) %>%
      mutate(apc_cost_m9 = ifelse(hos_visit_m9!=0, apc_cost_m9, 0)) %>%
      mutate(apc_cost_m10 = ifelse(hos_visit_m10!=0, apc_cost_m10, 0)) %>%
      mutate(apc_cost_m11 = ifelse(hos_visit_m11!=0, apc_cost_m11, 0)) %>%
      mutate(apc_cost_m12 = ifelse(hos_visit_m12!=0, apc_cost_m12, 0))

# OPA costs: 
matched_data <- matched_data %>% 
      mutate(opd_cost_m1 = ifelse(opa_visit_m1!=0, opd_cost_m1, 0)) %>% 
      mutate(opd_cost_m2 = ifelse(opa_visit_m2!=0, opd_cost_m2, 0)) %>%
      mutate(opd_cost_m3 = ifelse(opa_visit_m3!=0, opd_cost_m3, 0)) %>%
      mutate(opd_cost_m4 = ifelse(opa_visit_m4!=0, opd_cost_m4, 0)) %>%
      mutate(opd_cost_m5 = ifelse(opa_visit_m5!=0, opd_cost_m5, 0)) %>%
      mutate(opd_cost_m6 = ifelse(opa_visit_m6!=0, opd_cost_m6, 0)) %>%
      mutate(opd_cost_m7 = ifelse(opa_visit_m7!=0, opd_cost_m7, 0)) %>%
      mutate(opd_cost_m8 = ifelse(opa_visit_m8!=0, opd_cost_m8, 0)) %>%
      mutate(opd_cost_m9 = ifelse(opa_visit_m9!=0, opd_cost_m9, 0)) %>%
      mutate(opd_cost_m10 = ifelse(opa_visit_m10!=0, opd_cost_m10, 0)) %>%
      mutate(opd_cost_m11 = ifelse(opa_visit_m11!=0, opd_cost_m11, 0)) %>%
      mutate(opd_cost_m12 = ifelse(opa_visit_m12!=0, opd_cost_m12, 0))

# A&E cost:
matched_data <- matched_data %>% 
      mutate(er_cost_m1 = ifelse(ae_visit_m1!=0, er_cost_m1, 0)) %>% 
      mutate(er_cost_m2 = ifelse(ae_visit_m2!=0, er_cost_m2, 0)) %>%
      mutate(er_cost_m3 = ifelse(ae_visit_m3!=0, er_cost_m3, 0)) %>%
      mutate(er_cost_m4 = ifelse(ae_visit_m4!=0, er_cost_m4, 0)) %>%
      mutate(er_cost_m5 = ifelse(ae_visit_m5!=0, er_cost_m5, 0)) %>%
      mutate(er_cost_m6 = ifelse(ae_visit_m6!=0, er_cost_m6, 0)) %>%
      mutate(er_cost_m7 = ifelse(ae_visit_m7!=0, er_cost_m7, 0)) %>%
      mutate(er_cost_m8 = ifelse(ae_visit_m8!=0, er_cost_m8, 0)) %>%
      mutate(er_cost_m9 = ifelse(ae_visit_m9!=0, er_cost_m9, 0)) %>%
      mutate(er_cost_m10 = ifelse(ae_visit_m10!=0, er_cost_m10, 0)) %>%
      mutate(er_cost_m11 = ifelse(ae_visit_m11!=0, er_cost_m11, 0)) %>%
      mutate(er_cost_m12 = ifelse(ae_visit_m12!=0, er_cost_m12, 0))

# define total visits variables
visit_cols <- c()
for (i in 1:12) {
      visit_cols <- c(visit_cols, paste0("all_month_m", i))
}
      
# define gp visits vectors
gp_visits <- c()
for (i in 1:12){
      gp_visits <- c(gp_visits, paste0("gp_visit_m", i))
}

# Prescription counts
drug_visits <- c()
for (i in 1:12){
      drug_visits <- c(drug_visits, paste0("drug_visit_m", i))
}

# hostpital visits
hos_visits <- c()
for (i in 1:12) {
      hos_visits <- c(hos_visits, paste0("hos_visit_m", i))
}

# A&E visits
a_e_visits <- c()
for (i in 1:12) {
      a_e_visits <- c(a_e_visits, paste0("ae_visit_m", i))
}

# OPD visits
opd_visits <- c()
for (i in 1:12) {
      opd_visits <- c(opd_visits, paste0("opa_visit_m", i))
}      

costs <- matched_data[grep("_cost_m", names(matched_data))] %>%  names

# summarise follow-up period
fu_cols <- c()
for (i in 1:12) {
      fu_cols <- c(fu_cols, paste0("follow_up_m", i))
}      
      
# define variables for table 1 ----
dependent = "exposure"
explanatory = c("sex", "age", "age_cat", "ethnicity_6", "bmi_cat", "imd_q5", "region",
                "cov_asthma",
                "cov_mental_health",   
                "number_comorbidities_cat",
                "previous_covid_hosp",
                "cov_c19_vaccine_number", 
                "cov_covid_vaccine_number", "cov_covid_vax_n_cat", 
                "long_covid_dx", "covid_positive", "reg_and_visit_gp_1y"
                )

# Table 1 reporting numbers:  -----
# basic demographic data
basic_demographic <- matched_data %>% summary_factorlist(dependent, explanatory, 
                                    p = TRUE,
                                    add_row_totals = TRUE,
                                    row_totals_colname = "Total",
                                    cont_cut = 5) 


# Summarising the outcomes by types:
outcomes <- c(visit_cols, gp_visits, drug_visits, hos_visits, a_e_visits, opd_visits, costs, fu_cols)

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
svg(file=here("output", "st1_exporing_vax_index_date.svg"),
    width=800, height=1200)
ggarrange(a_0dose,b_1dose,c_2dose, d_3dose,
          ncol = 1, nrow = 4) 
dev.off()


# check missing value distribution:------
matched_data %>% 
      missing_glimpse(dependent, explanatory) %>% 
      write.csv(here("output", "missing_distribution_table.csv"), row.names = F)


# check missing pattern:
svg(file=here("output", "missing_pattern_current.svg"),
    width=600, height=600)
matched_data %>% 
      missing_pattern(dependent, explanatory)
dev.off()
