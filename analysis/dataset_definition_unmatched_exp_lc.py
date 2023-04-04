from datetime import date

from databuilder.ehrql import Dataset, days, years
from databuilder.tables.beta.tpp import (
    patients, addresses, ons_deaths, sgss_covid_all_tests,
    practice_registrations, clinical_events,
)
from databuilder.codes import SNOMEDCTCode
from codelists import lc_codelists_combined
from covariates import *
# from variables import add_visits

# Defining the exposure groups

dataset = Dataset()
dataset.define_population((age >= 18) & registration.exists_for_patient() & lc_dx.exists_for_patient())
dataset.age = age
dataset.sex = patients.sex
dataset.region = registration.practice_stp
dataset.gp_practice = registration.practice_pseudo_id
dataset.registration_date = registration.start_date
dataset.long_covid_dx = lc_dx.exists_for_patient()
dataset.long_covid_dx_date = lc_dx_date
dataset.index_date = lc_dx_date
dataset.end_death = death_date
dataset.end_deregist = end_reg_date
dataset.end_lc_cure = lc_cure_date

# # the following codes will be added to matched data.
# dataset.covid_positive = latest_test_before_diagnosis.exists_for_patient()
# dataset.covid_dx_month = latest_test_before_diagnosis.specimen_taken_date.to_first_of_month() # only need dx month
# dataset.ethnicity = ethnicity
# dataset.imd = imd
# dataset.bmi = bmi
# dataset.bmi_date = bmi_date
# dataset.previous_covid_hosp = previous_covid_hos.exists_for_patient()
# dataset.cov_c19_vaccine_number = c19_vaccine_number
# dataset.cov_cancer = cancer_all.exists_for_patient()
# dataset.cov_mental_health = mental_health_issues.exists_for_patient()
# dataset.cov_asthma = asthma.exists_for_patient() & ~copd.exists_for_patient()
# dataset.cov_organ_transplant = organ_transplant.exists_for_patient()
# dataset.cov_chronic_cardiac_disease = chronic_cardiac_disease.exists_for_patient()
# dataset.cov_chronic_liver_disease = chronic_liver_disease.exists_for_patient()
# dataset.cov_stroke_dementia = stroke.exists_for_patient() | dementia.exists_for_patient()
# dataset.cov_other_neuro_diseases = other_neuro_diseases.exists_for_patient()
# dataset.cov_ra_sle_psoriasis = ra_sle_psoriasis.exists_for_patient()
# dataset.cov_asplenia = asplenia.exists_for_patient()
# dataset.cov_hiv = hiv.exists_for_patient()
# dataset.cov_aplastic_anemia = aplastic_anemia.exists_for_patient()
# dataset.cov_permanent_immune_suppress = permanent_immune_suppress.exists_for_patient()
# dataset.cov_temporary_immune_suppress = temporary_immune_suppress.exists_for_patient()

# Defining outcomes:
# GP visits: 
# add_visits(dataset, lc_dx.date, num_months=1)
# add_visits(dataset, lc_dx.date, num_months=2)
# add_visits(dataset, lc_dx.date, num_months=3)
# add_visits(dataset, lc_dx.date, num_months=4)
# add_visits(dataset, lc_dx.date, num_months=5)
# add_visits(dataset, lc_dx.date, num_months=6)
# add_visits(dataset, lc_dx.date, num_months=7)
# add_visits(dataset, lc_dx.date, num_months=8)
# add_visits(dataset, lc_dx.date, num_months=9)
# add_visits(dataset, lc_dx.date, num_months=10)
# add_visits(dataset, lc_dx.date, num_months=11)
# add_visits(dataset, lc_dx.date, num_months=12)


