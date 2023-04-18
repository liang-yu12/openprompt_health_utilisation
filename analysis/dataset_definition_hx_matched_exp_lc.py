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

# import matched data

@table_from_file("output/matched_cases_historical.csv")
class matched_hx_cases(PatientFrame):
    age = Series(int)
    sex = Series(str)
    lc_exp = Series(int)
    index_date = Series(date)
    region = Series(str)
    set_id = Series(int)
    lc_exposure = Series(int)
    match_counts = Series(float)


dataset = Dataset()
dataset.define_population(
    (age >= 18)
    & matched_hx_cases.exists_for_patient()
)

dataset.age = matched_hx_cases.age
dataset.sex = matched_hx_cases.sex
dataset.region = matched_hx_cases.region
dataset.lc_dx = matched_hx_cases.lc_exp
dataset.index_date = matched_hx_cases.index_date
dataset.exposure = matched_hx_cases.lc_exposure

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