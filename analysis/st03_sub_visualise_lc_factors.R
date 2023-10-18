# Visualise the outputs

options(digits = 2, scipen = 999)

# Load all packages
source("analysis/settings_packages.R")

# Data management for the forest plot:

lc_binomial <- read_csv(here("output", "st03_05_sub_lc_only_binomial.csv")) %>% 
      filter(time =="12 months" & term != "(Intercept)") %>% 
      dplyr::select(term, estimate, lci, hci, p.value) 

lc_binomial_s1 <- lc_binomial %>% mutate(
      Factors = case_when(
            term == "age" ~ "Age",
            term == "sexfemale" ~ "Female",
            term == "bmi_catUnderweight" ~ "Underweight",
            term == "bmi_catOverweight" ~ "Overweight",
            term == "bmi_catObese" ~ "Obese",
            term == "ethnicity_6Mixed" ~ "Mixed",
            term == "ethnicity_6South Asian" ~ "South Asian",
            term == "ethnicity_6Black" ~ "Black",
            term == "ethnicity_6Other" ~ "Other",
            term == "imd_q52_deprived" ~ "2nd deprived",
            term == "imd_q53_deprived" ~ "3rd deprived",
            term == "imd_q54_deprived" ~ "4th deprived",
            term == "imd_q5most_deprived" ~ "Most deprived",
            term == "regionEast" ~ "East",
            term == "regionEast Midlands" ~ "East Midlands",
            term == "regionNorth East" ~ "North East",
            term == "regionNorth West" ~ "North West",
            term == "regionSouth East" ~ "South East",
            term == "regionSouth West" ~ "South West",
            term == "regionWest Midlands"~ "West Midlands",
            term == "regionYorkshire and The Humber" ~ "Yorkshire and The Humber",
            term == "cov_asthmaTRUE" ~ "Had asthma",
            term == "cov_mental_healthTRUE" ~ "Had mental health isses",
            term == "previous_covid_hospTRUE" ~ "Had previous COVID hospital admission",
            term == "cov_covid_vax_n_cat1 dose"~ "Received one dose",
            term == "cov_covid_vax_n_cat2 doses"~ "Received two doses",
            term == "cov_covid_vax_n_cat3 or more doses"~ "Received three or more doses",
            term == "number_comorbidities_cat1" ~ "One comorbidities",
            term == "number_comorbidities_cat2" ~ "Two comorbidities",
            term == "number_comorbidities_cat3" ~ "Three or more comorbidities")) %>% 
      mutate(Categories = case_when(
            str_detect(term, "age") ~ "Age",
            str_detect(term, "sex") ~ "Sex",
            str_detect(term, "bmi_") ~ "BMI categories",
            str_detect(term, "ethnicity_") ~ "Ethnicity",
            str_detect(term, "imd_q") ~ "Index for Multiple Deprivation",
            str_detect(term, "region") ~ "Region",
            str_detect(term, "previous_covid_") ~ "Hospitalisation",
            str_detect(term, "cov_covid_vax_n_") ~ "COVID vaccine doses",
            (str_detect(term, "cov_asthma") | str_detect(term, "cov_mental_health")) ~ "Underlying diseases",
            str_detect(term, "number_comorbidities_cat") ~ "Comorbidities")) %>% 
      dplyr::select(Categories, Factors, estimate, lci, hci) 
      
lc_bi_plot_data <- lc_binomial_s1 %>%
      group_by(Categories) %>% 
      mutate(Order = row_number()) %>% 
      ungroup %>% 
      add_row(Categories = "Sex",
              Factors = "Male",
              estimate = 1, lci =1, hci =1,
              Order = 0) %>% 
      add_row(Categories = "BMI categories", 
              Factors = "Normal BMI",
              estimate = 1, lci =1, hci =1,
              Order = 0) %>% 
      add_row(Categories = "Ethnicity", 
              Factors = "White ethnicity",
              estimate = 1, lci =1, hci =1,
              Order = 0) %>% 
      add_row(Categories = "Index for Multiple Deprivation", 
              Factors = "Least deprived",
              estimate = 1, lci =1, hci =1,
              Order = 0) %>% 
      add_row(Categories = "Region", 
              Factors = "London",
              estimate = 1, lci =1, hci =1,
              Order = 0) %>% 
      add_row(Categories = "Underlying diseases", 
              Factors = "No asthma",
              estimate = 1, lci =1, hci =1,
              Order = 0) %>% 
      add_row(Categories = "Underlying diseases", 
              Factors = "No mental health isses",
              estimate = 1, lci =1, hci =1,
              Order = 0) %>% 
      add_row(Categories = "COVID vaccine doses", 
              Factors = "Did not receive COVID vaccines",
              estimate = 1, lci =1, hci =1,
              Order = 0) %>% 
      add_row(Categories = "Comorbidities", 
              Factors = "Did not have comorbidities",
              estimate = 1, lci =1, hci =1,
              Order = 0) %>% 
      mutate(cat_order = case_when(
            Categories == "Age" ~ 1,
            Categories == "Sex" ~ 2,
            Categories == "Ethnicity" ~ 3,
            Categories == "Region" ~ 4,
            Categories == "Index for Multiple Deprivation" ~ 5,
            Categories == "BMI categories"~ 6,
            Categories == "Underlying diseases"~ 7,
            Categories == "Comorbidities" ~ 8,
            Categories == "Hospitalisation"~ 9,
            Categories == "COVID vaccine doses" ~ 10)) %>% 
      arrange(cat_order, Order) %>% 
      mutate(Categories = ifelse(Order == 0, Categories, " ")) %>% 
      mutate(Categories = ifelse(Factors == "Age", "Age", Categories)) %>% 
      mutate(Categories = ifelse(Factors == "Had previous COVID hospital admission", 
                                 "Hospitalisation", Categories)) %>% 
      dplyr::select(Categories, Factors, estimate, lci, hci)
      

lc_bi_plot_data$`   First part      `  <- " "
lc_bi_plot_data <- relocate(lc_bi_plot_data, `   First part      `  , .after = Factors)



lc_bi_plot_data$`OR (95% CI)` <- sprintf("%.2f (%.2f - %.2f)",
                                         lc_bi_plot_data$estimate,  
                                         lc_bi_plot_data$lci,  
                                         lc_bi_plot_data$hci)
lc_bi_plot_data <- relocate(lc_bi_plot_data, `OR (95% CI)` , .after = `   First part      `)


# Forest plot: 

(bi_forestplot <- forest(
      data = lc_bi_plot_data[,1:4],
      est = lc_bi_plot_data$estimate,
      lower = lc_bi_plot_data$lci,
      upper = lc_bi_plot_data$hci,
      ci_column = 3,
      xlim = c(0, 4),
      ref_line = 1
))


# Second part Hurdle ------


lc_hurdle <- read_csv("output/st03_05_sub_lc_only_hurdle.csv") %>% 
      filter(time =="12 months" & term != "(Intercept):2" & term != "(Intercept):1") %>% 
      dplyr::select(term, estimate, lci, hci, p.value) %>% 
      mutate(
            Factors = case_when(
                  term == "age" ~ "Age",
                  term == "sexfemale" ~ "Female",
                  term == "bmi_catUnderweight" ~ "Underweight",
                  term == "bmi_catOverweight" ~ "Overweight",
                  term == "bmi_catObese" ~ "Obese",
                  term == "ethnicity_6Mixed" ~ "Mixed",
                  term == "ethnicity_6South Asian" ~ "South Asian",
                  term == "ethnicity_6Black" ~ "Black",
                  term == "ethnicity_6Other" ~ "Other",
                  term == "imd_q52_deprived" ~ "2nd deprived",
                  term == "imd_q53_deprived" ~ "3rd deprived",
                  term == "imd_q54_deprived" ~ "4th deprived",
                  term == "imd_q5most_deprived" ~ "Most deprived",
                  term == "regionEast" ~ "East",
                  term == "regionEast Midlands" ~ "East Midlands",
                  term == "regionNorth East" ~ "North East",
                  term == "regionNorth West" ~ "North West",
                  term == "regionSouth East" ~ "South East",
                  term == "regionSouth West" ~ "South West",
                  term == "regionWest Midlands"~ "West Midlands",
                  term == "regionYorkshire and The Humber" ~ "Yorkshire and The Humber",
                  term == "cov_asthmaTRUE" ~ "Had asthma",
                  term == "cov_mental_healthTRUE" ~ "Had mental health isses",
                  term == "previous_covid_hospTRUE" ~ "Had previous COVID hospital admission",
                  term == "cov_covid_vax_n_cat1 dose"~ "Received one dose",
                  term == "cov_covid_vax_n_cat2 doses"~ "Received two doses",
                  term == "cov_covid_vax_n_cat3 or more doses"~ "Received three or more doses",
                  term == "number_comorbidities_cat1" ~ "One comorbidities",
                  term == "number_comorbidities_cat2" ~ "Two comorbidities",
                  term == "number_comorbidities_cat3" ~ "Three or more comorbidities")) %>% 
      mutate(Categories = case_when(
            str_detect(term, "age") ~ "Age",
            str_detect(term, "sex") ~ "Sex",
            str_detect(term, "bmi_") ~ "BMI categories",
            str_detect(term, "ethnicity_") ~ "Ethnicity",
            str_detect(term, "imd_q") ~ "Index for Multiple Deprivation",
            str_detect(term, "region") ~ "Region",
            str_detect(term, "previous_covid_") ~ "Hospitalisation",
            str_detect(term, "cov_covid_vax_n_") ~ "COVID vaccine doses",
            (str_detect(term, "cov_asthma") | str_detect(term, "cov_mental_health")) ~ "Underlying diseases",
            str_detect(term, "number_comorbidities_cat") ~ "Comorbidities")) %>% 
      dplyr::select(Categories, Factors, estimate, lci, hci) 

lc_hurdle_plot_data <- lc_hurdle  %>%
      group_by(Categories) %>% 
      mutate(Order = row_number()) %>% 
      ungroup %>% 
      add_row(Categories = "Sex",
              Factors = "Male",
              estimate = 1, lci =1, hci =1,
              Order = 0) %>% 
      add_row(Categories = "BMI categories", 
              Factors = "Normal BMI",
              estimate = 1, lci =1, hci =1,
              Order = 0) %>% 
      add_row(Categories = "Ethnicity", 
              Factors = "White ethnicity",
              estimate = 1, lci =1, hci =1,
              Order = 0) %>% 
      add_row(Categories = "Index for Multiple Deprivation", 
              Factors = "Least deprived",
              estimate = 1, lci =1, hci =1,
              Order = 0) %>% 
      add_row(Categories = "Region", 
              Factors = "London",
              estimate = 1, lci =1, hci =1,
              Order = 0) %>% 
      add_row(Categories = "Underlying diseases", 
              Factors = "No asthma",
              estimate = 1, lci =1, hci =1,
              Order = 0) %>% 
      add_row(Categories = "Underlying diseases", 
              Factors = "No mental health isses",
              estimate = 1, lci =1, hci =1,
              Order = 0) %>% 
      add_row(Categories = "COVID vaccine doses", 
              Factors = "Did not receive COVID vaccines",
              estimate = 1, lci =1, hci =1,
              Order = 0) %>% 
      add_row(Categories = "Comorbidities", 
              Factors = "Did not have comorbidities",
              estimate = 1, lci =1, hci =1,
              Order = 0) %>% 
      mutate(cat_order = case_when(
            Categories == "Age" ~ 1,
            Categories == "Sex" ~ 2,
            Categories == "Ethnicity" ~ 3,
            Categories == "Region" ~ 4,
            Categories == "Index for Multiple Deprivation" ~ 5,
            Categories == "BMI categories"~ 6,
            Categories == "Underlying diseases"~ 7,
            Categories == "Comorbidities" ~ 8,
            Categories == "Hospitalisation"~ 9,
            Categories == "COVID vaccine doses" ~ 10)) %>% 
      arrange(cat_order, Order) %>% 
      mutate(Categories = ifelse(Order == 0, Categories, " ")) %>% 
      mutate(Categories = ifelse(Factors == "Age", "Age", Categories)) %>% 
      mutate(Categories = ifelse(Factors == "Had previous COVID hospital admission", 
                                 "Hospitalisation", Categories)) %>% 
      dplyr::select(Categories, Factors, estimate, lci, hci)


lc_hurdle_plot_data$`   Second part      `  <- " "
lc_hurdle_plot_data <- relocate(lc_hurdle_plot_data, `   Second part      `  , .after = Factors)



lc_hurdle_plot_data$`RR (95% CI)` <- sprintf("%.2f (%.2f - %.2f)",
                                             lc_hurdle_plot_data$estimate,  
                                             lc_hurdle_plot_data$lci,  
                                             lc_hurdle_plot_data$hci)
lc_hurdle_plot_data <- relocate(lc_hurdle_plot_data, `RR (95% CI)` , .after = `   Second part      `)

(hurdle_forestplot <- forest(
      data = lc_hurdle_plot_data[,1:4],
      est = lc_hurdle_plot_data$estimate,
      lower = lc_hurdle_plot_data$lci,
      upper = lc_hurdle_plot_data$hci,
      ci_column = 3,
      xlim = c(0, 4),
      ref_line = 1
))

# Plot two parts together:
part_1 <- lc_bi_plot_data
part_2 <- lc_hurdle_plot_data %>% 
      rename(      estimate2 = estimate, 
                   lci2 = lci,
                   hci2 = hci)

combine_p_1_2 <- full_join(part_1, part_2)

combine_p_1_2[,c(1,2,3,4,8,9)] %>% names

tm <- forest_theme(core=list(bg_params=list(fill = c("#FFFFFF"))))

two_forest <- forest(
      data = combine_p_1_2[,c(1,2,3,4,8,9)],
      est = list(combine_p_1_2$estimate, combine_p_1_2$estimate2),
      lower = list(combine_p_1_2$lci, combine_p_1_2$lci2),
      upper = list(combine_p_1_2$hci, combine_p_1_2$hci2),
      ci_column = c(3, 5),
      xlim = c(0, 7),
      ref_line = 1,
      theme = tm)
plot(two_forest)


ggsave(two_forest, file = "output/st03_sub_lc_factors.png",
       width=20, height=12, units = "in", dpi = 300)

