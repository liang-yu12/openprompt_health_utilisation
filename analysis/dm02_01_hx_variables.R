# Load all packages
source("analysis/settings_packages.R")

# Exposure group: covariate data management:   ------
# read in matched lc exposure group:
hx_cases <- read_csv(here("output", "hx_matched_cases_with_ehr.csv"), 
                     col_types = cols(index_date = col_date(format = "%Y-%m-%d"), 
                                      bmi_date = col_skip(),
                                      end_death = col_date(format = "%Y-%m-%d"), 
                                      end_deregist = col_date(format = "%Y-%m-%d"), 
                                      end_lc_cure = col_date(format = "%Y-%m-%d")))

# Data management of common variables:
hx_cases <- hx_cases  %>% mutate(
      imd_q5 = cut2(imd, g = 5),
      ethnicity_6 = factor(
            ethnicity,
            levels = 1:6, 
            labels = c(
                  "White",
                  "Mixed", 
                  "South Asian", 
                  "Black",
                  "Other",
                  "Not stated")
      ),
      age_cat = cut(
            age, 
            breaks = c(0, seq(30, 70, 10), Inf),
            labels = c(
                  "18-29",
                  "30-39",
                  "40-49",
                  "50-59",
                  "60-69",
                  "70+"
            )),
      bmi_cat = case_when(
            ethnicity_6 %in% c("South Asian", "Other") ~ cut(bmi, breaks = c(0,18.5,23,27.5,Inf), 
                                                             labels = c("Underweight", "Normal Weight", "Overweight", "Obese")),
            !ethnicity_6 %in% c("South Asian", "Other") ~ cut(bmi, breaks = c(0,18.5,25,30,Inf),
                                                              labels = c("Underweight", "Normal Weight", "Overweight", "Obese"))
      )
)

# set as factors:
to_be_factors <- c("sex", "region", "ethnicity")
hx_cases[to_be_factors] <- lapply(hx_cases[to_be_factors], as.factor)
hx_cases[to_be_factors] <- lapply(hx_cases[to_be_factors], droplevels)

levels(hx_cases$imd_q5)<- c("least_deprived",
                            "2_deprived",
                            "3_deprived",
                            "4_deprived",
                            "most_deprived")
# commorbidities:
comorbidities <- c("cov_cancer",  "cov_mental_health",   "cov_asthma",
                   "cov_organ_transplant",   "cov_chronic_cardiac_disease",   "cov_chronic_liver_disease",
                   "cov_stroke_dementia",   "cov_other_neuro_diseases",   "cov_ra_sle_psoriasis",
                   "cov_asplenia",   "cov_hiv",   "cov_aplastic_anemia",   "cov_permanent_immune_suppress",
                   "cov_temporary_immune_suppress")
hx_cases[comorbidities] <- lapply(hx_cases[comorbidities], as.logical)
hx_cases$number_comorbidities <- rowSums(hx_cases[comorbidities], na.rm = T)
hx_cases <- hx_cases %>% 
      mutate(number_comorbidities_cat = case_when(
            number_comorbidities ==0 ~ 0,
            number_comorbidities ==1 ~ 1,
            number_comorbidities ==2 ~ 2,
            number_comorbidities >=3 ~ 3)) 
hx_cases$number_comorbidities_cat <- hx_cases$number_comorbidities_cat %>% as.factor()

hx_cases[comorbidities] <- NULL

hx_cases %>% names
# Comparator group: covariate data management ------
hx_control <- read_csv(here("output", "hx_matched_control_with_ehr.csv"), 
                       col_types = cols(index_date = col_date(format = "%Y-%m-%d"), 
                                        bmi_date = col_skip(),
                                        end_death = col_date(format = "%Y-%m-%d"), 
                                        end_deregist = col_date(format = "%Y-%m-%d"), 
                                        end_lc_cure = col_date(format = "%Y-%m-%d")))

hx_control <- hx_control  %>% mutate(
      imd_q5 = cut2(imd, g = 5),
      ethnicity_6 = factor(
            ethnicity,
            levels = 1:6, 
            labels = c(
                  "White",
                  "Mixed", 
                  "South Asian", 
                  "Black",
                  "Other",
                  "Not stated")
      ),
      age_cat = cut(
            age, 
            breaks = c(0, seq(30, 70, 10), Inf),
            labels = c(
                  "18-29",
                  "30-39",
                  "40-49",
                  "50-59",
                  "60-69",
                  "70+"
            )),
      bmi_cat = case_when(
            ethnicity_6 %in% c("South Asian", "Other") ~ cut(bmi, breaks = c(0,18.5,23,27.5,Inf), 
                                                             labels = c("Underweight", "Normal Weight", "Overweight", "Obese")),
            !ethnicity_6 %in% c("South Asian", "Other") ~ cut(bmi, breaks = c(0,18.5,25,30,Inf),
                                                              labels = c("Underweight", "Normal Weight", "Overweight", "Obese"))
      )
)

hx_control[to_be_factors] <- lapply(hx_control[to_be_factors], as.factor)
hx_control[to_be_factors] <- lapply(hx_control[to_be_factors], droplevels)


# deal with comorbidities
hx_control[comorbidities] <- lapply(hx_control[comorbidities], as.logical)
hx_control$number_comorbidities <- rowSums(hx_control[comorbidities], na.rm = T)
hx_control <- hx_control %>% 
      mutate(number_comorbidities_cat = case_when(
            number_comorbidities ==0 ~ 0,
            number_comorbidities ==1 ~ 1,
            number_comorbidities ==2 ~ 2,
            number_comorbidities >=3 ~ 3)) 

hx_control$number_comorbidities_cat <- hx_control$number_comorbidities_cat %>% as.factor()
hx_control[comorbidities] <- NULL
hx_control %>% names
