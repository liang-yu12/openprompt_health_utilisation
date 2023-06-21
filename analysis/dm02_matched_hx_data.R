# Load all packages
source("analysis/settings_packages.R")

# Initial data management for DID structure:
# Explanation: Need to create a long table specifying the time period and the exposure group, 
# So that in the following analysis we can add the interaction term in the DID model 


# First define common variables to select:
vars <- c("patient_id","age","sex","region","lc_dx","index_date","exposure",
          "ethnicity" ,"imd", "bmi" ,"cov_cancer" ,"cov_mental_health" ,
          "cov_asthma", "cov_organ_transplant" ,"cov_chronic_cardiac_disease", 
          "cov_chronic_liver_disease", "cov_stroke_dementia" ,"cov_other_neuro_diseases",     
          "cov_ra_sle_psoriasis", "cov_asplenia" ,"cov_hiv" ,"cov_aplastic_anemia",          
          "cov_permanent_immune_suppress", "cov_temporary_immune_suppress")


hx_visits <- c("hx_gp_visit_m1", "hx_gp_visit_m2", "hx_gp_visit_m3", "hx_gp_visit_m4", "hx_gp_visit_m5",
        "hx_gp_visit_m6", "hx_gp_visit_m7", "hx_gp_visit_m8", "hx_gp_visit_m9", "hx_gp_visit_m10",
        "hx_gp_visit_m11", "hx_gp_visit_m12", "hx_hos_visit_m1", "hx_hos_visit_m2", "hx_hos_visit_m3",
        "hx_hos_visit_m4", "hx_hos_visit_m5", "hx_hos_visit_m6", "hx_hos_visit_m7", "hx_hos_visit_m8",
        "hx_hos_visit_m9", "hx_hos_visit_m10", "hx_hos_visit_m11", "hx_hos_visit_m12", "hx_ae_visit_m1",
        "hx_ae_visit_m2", "hx_ae_visit_m3", "hx_ae_visit_m4", "hx_ae_visit_m5", "hx_ae_visit_m6",
        "hx_ae_visit_m7", "hx_ae_visit_m8", "hx_ae_visit_m9", "hx_ae_visit_m10", "hx_ae_visit_m11",
        "hx_ae_visit_m12")


now_visits <- c("gp_visit_m1", "gp_visit_m2", "gp_visit_m3", "gp_visit_m4", "gp_visit_m5", "gp_visit_m6",
             "gp_visit_m7", "gp_visit_m8", "gp_visit_m9", "gp_visit_m10", "gp_visit_m11", "gp_visit_m12",
             "hos_visit_m1", "hos_visit_m2", "hos_visit_m3", "hos_visit_m4", "hos_visit_m5", "hos_visit_m6",
             "hos_visit_m7", "hos_visit_m8", "hos_visit_m9", "hos_visit_m10", "hos_visit_m11", "hos_visit_m12",
             "ae_visit_m1", "ae_visit_m2", "ae_visit_m3", "ae_visit_m4", "ae_visit_m5", "ae_visit_m6",
             "ae_visit_m7", "ae_visit_m8", "ae_visit_m9", "ae_visit_m10", "ae_visit_m11", "ae_visit_m12")


# # 1. Exposure/cases:
hx_cases <- read_csv(here("output", "hx_matched_cases_with_ehr.csv"), 
                                      col_types = cols(index_date = col_date(format = "%Y-%m-%d"), 
                                                       bmi_date = col_skip()))
# subset the historical cases: 
hx_exp <- hx_cases %>% dplyr::select(all_of(vars), all_of(hx_visits)) %>% mutate(time = 0)
hx_exp <- setnames(hx_exp, old = hx_visits, new = now_visits) # Rename variables for later combinations

# subset the current cases: 
now_exp <- hx_cases %>% dplyr::select(all_of(vars), all_of(now_visits)) %>% mutate(time = 1)


# # 2. Comparators:
hx_control <- read_csv(here("output", "hx_matched_control_with_ehr.csv"), 
                                        col_types = cols(index_date = col_date(format = "%Y-%m-%d"), 
                                                         bmi_date = col_skip()))
# subset the historical cases: 
hx_com <- hx_control %>% dplyr::select(all_of(vars), all_of(hx_visits)) %>% mutate(time = 0) 
hx_com <- setnames(hx_com, old = hx_visits, new = now_visits) # Rename variables for later combinations

# subset the current cases: 
now_com <- hx_control %>% dplyr::select(all_of(vars), all_of(now_visits)) %>% mutate(time = 1)



#  combine four datasets
hx_matched_data <- bind_rows(hx_exp, now_exp, hx_com, now_com)





hx_matched_data$exposure <- hx_matched_data$exposure %>% 
      factor(label = c("Comparator", "Long COVID exposure"))


hx_matched_data <- hx_matched_data %>% mutate(
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

# hx_matched_data$imd_q5 <- factor(hx_matched_data$imd_q5, 
#                                  lebels = c("least_deprived", 
#                                            "2_deprived",
#                                            "3_deprived",
#                                            "4_deprived",
#                                            "most_deprived"))