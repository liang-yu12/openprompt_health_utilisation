import datetime
from databuilder.ehrql import Dataset, days, years
from databuilder.tables.beta.tpp import (
    patients, addresses, ons_deaths, sgss_covid_all_tests,
    practice_registrations, clinical_events,
)
from databuilder.query_language import table_from_file, PatientFrame, Series
from covariates import *

# Import matched data

@table_from_file("output/matched_matches_stp.csv")
class matched_matches(PatientFrame):
    patient_id = Series(int)
    age = Series(int)
    sex = Series(str)
    region = Series(str)
    gp_practice =  Series(int)
    registration_date = Series(date)
    long_covid_dx = Series(int)
    long_covid_dx_date= Series(date)
    end_death = Series(date)
    end_deregist = Series(date)
    end_lc_cure = Series(date)
    set_id = Series(int)
    exposure = Series(int)
    index_date = Series(date)


# Define dataset variables

dataset = Dataset()
dataset.define_population(
    (age >= 18)
    & matched_matches.exists_for_patient()
)

dataset.age = matched_matches.age
dataset.sex = matched_matches.sex
dataset.region = matched_matches.region
dataset.gp_practice = matched_matches.gp_practice
dataset.registration_date = matched_matches.registration_date
dataset.long_covid_dx = matched_matches.long_covid_dx
dataset.long_covid_dx_date= matched_matches.long_covid_dx_date
dataset.index_date = matched_matches.index_date
dataset.end_death = matched_matches.end_death
dataset.end_deregist = matched_matches.end_deregist
dataset.end_lc_cure = matched_matches.end_lc_cure
dataset.set_id = matched_matches.set_id
dataset.exposure = matched_matches.exposure

dataset.covid_positive = latest_test_before_diagnosis.exists_for_patient()
dataset.covid_dx_month = latest_test_before_diagnosis.specimen_taken_date.to_first_of_month() # only need dx month
dataset.ethnicity = ethnicity
dataset.imd = imd
dataset.bmi = bmi
dataset.bmi_date = bmi_date
dataset.previous_covid_hosp = previous_covid_hos.exists_for_patient()
dataset.cov_c19_vaccine_number = c19_vaccine_number
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