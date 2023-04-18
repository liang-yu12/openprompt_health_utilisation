import datetime
from databuilder.ehrql import Dataset, days, years
from databuilder.tables.beta.tpp import (
    patients, addresses, ons_deaths, sgss_covid_all_tests,
    practice_registrations, clinical_events,
)
from databuilder.query_language import table_from_file, PatientFrame, Series
from covariates import *
from variables import add_visits, add_hos_visits, add_ae_visits
from hx_covariates import *
from variables import add_visits, add_hx_visits, add_hos_visits, add_ae_visits

# import matched data

@table_from_file("output/matched_matches_historical.csv")
class matched_hx_comparator(PatientFrame):
    age = Series(int)
    sex = Series(str)
    lc_exp = Series(int)    
    region = Series(str)
    set_id = Series(int)
    lc_exposure = Series(int)
    index_date = Series(date)


dataset = Dataset()
dataset.define_population(
    (age >= 18)
    & matched_hx_comparator.exists_for_patient()
)



dataset.age = matched_hx_comparator.age
dataset.sex = matched_hx_comparator.sex
dataset.region = matched_hx_comparator.region
dataset.lc_dx = matched_hx_comparator.lc_exp
dataset.index_date = matched_hx_comparator.index_date
dataset.exposure = matched_hx_comparator.lc_exposure

# Demographic covariates
dataset.ethnicity = ethnicity
dataset.imd = imd
dataset.bmi = bmi
dataset.bmi_date = bmi_date
dataset.cov_cancer = cancer_all.exists_for_patient()
dataset.cov_mental_health = mental_health_issues.exists_for_patient()
dataset.cov_asthma = asthma.exists_for_patient() & ~copd.exists_for_patient()
dataset.cov_organ_transplant = organ_transplant.exists_for_patient()
dataset.cov_chronic_cardiac_disease = chronic_cardiac_disease.exists_for_patient()
dataset.cov_chronic_liver_disease = chronic_liver_disease.exists_for_patient()
dataset.cov_stroke_dementia = stroke.exists_for_patient() | dementia.exists_for_patient()
dataset.cov_other_neuro_diseases = other_neuro_diseases.exists_for_patient()
dataset.cov_ra_sle_psoriasis = ra_sle_psoriasis.exists_for_patient()
dataset.cov_asplenia = asplenia.exists_for_patient()
dataset.cov_hiv = hiv.exists_for_patient()
dataset.cov_aplastic_anemia = aplastic_anemia.exists_for_patient()
dataset.cov_permanent_immune_suppress = permanent_immune_suppress.exists_for_patient()
dataset.cov_temporary_immune_suppress = temporary_immune_suppress.exists_for_patient()

# Outcome visit
# Historical GP visits: 1 year before 
add_hx_visits(dataset, one_year_before_index_date, num_months=1)
add_hx_visits(dataset, one_year_before_index_date, num_months=2)
add_hx_visits(dataset, one_year_before_index_date, num_months=3)
add_hx_visits(dataset, one_year_before_index_date, num_months=4)
add_hx_visits(dataset, one_year_before_index_date, num_months=5)
add_hx_visits(dataset, one_year_before_index_date, num_months=6)
add_hx_visits(dataset, one_year_before_index_date, num_months=7)
add_hx_visits(dataset, one_year_before_index_date, num_months=8)
add_hx_visits(dataset, one_year_before_index_date, num_months=9)
add_hx_visits(dataset, one_year_before_index_date, num_months=10)
add_hx_visits(dataset, one_year_before_index_date, num_months=11)
add_hx_visits(dataset, one_year_before_index_date, num_months=12)

# Hospital visits
add_hos_visits(dataset, lc_dx.date, num_months=1)
# add_hos_visits(dataset, lc_dx.date, num_months=4)
# add_ae_visits(dataset, lc_dx.date, num_months=5)
# add_ae_visits(dataset, lc_dx.date, num_months=6)