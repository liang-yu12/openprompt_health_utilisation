# Data management goal: labelled and combined two datasets togather

# Exposure group:
lc_exposure <- fread(here::here("output/matched_cases.csv"), 
                     colClasses = c())
glimpse(lc_exposure)

names(lc_exposure)

string_var<- c("gp_practice", "long_covid_dx", "covid_positive", "previous_covid_hosp",
"cov_cancer", "cov_mental_health", "cov_asthma", "cov_organ_transplant",
"cov_chronic_cardiac_disease", "cov_chronic_liver_disease", "cov_stroke_dementia",
"cov_other_neuro_diseases", "cov_ra_sle_psoriasis", "cov_asplenia", "cov_hiv", 
"cov_aplastic_anemia", "cov_permanent_immune_suppress", "cov_temporary_immune_suppress")