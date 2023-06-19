# Source data management
source("analysis/dm03_matched_define_monthly_follow_up_time.R")


dependent = "exposure"
explanatory = c("sex", "age","age_cat", "ethnicity_6", "bmi_cat", "imd_q5", "region",
                "long_covid_dx", "covid_positive","previous_covid_hosp",
                "cov_c19_vaccine_number", "cov_covid_vaccine_number", 
                "cov_covid_vax_n_cat", "number_comorbidities_cat","admit_over_1m_count")

# Table 1 reporting numbers:  -----
matched_data %>% summary_factorlist(dependent, explanatory, 
                                    p = TRUE,
                                    add_row_totals = TRUE,
                                    row_totals_colname = "Total",
                                    cont_cut = 7
                                    ) %>% 
      write.csv(here("output", "st01_matched_numbers_table.csv"), row.names = F)

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