from datetime import date

from databuilder.ehrql import Dataset, days, years, months
from databuilder.tables.beta.tpp import (
    patients, addresses, appointments,
    practice_registrations, clinical_events,
    sgss_covid_all_tests, ons_deaths, 
)
from codelists import lc_codelists_combined
from covariates import *
import codelists
import csv 


# Import the filtered GP list to exclude GP that was not using the long COVID codes
with open("output/dataset_lc_gp_list.csv") as csv_file:
    reader = csv.DictReader(csv_file)
    lc_gp = [int(row["gp_practice"]) for row in reader]

target_practices = practice_registrations.where(practice_registrations.practice_pseudo_id.is_in(lc_gp))

dataset = Dataset()
dataset.define_population((age >= 18) & registration.exists_for_patient() & target_practices.exists_for_patient())
dataset.age = age
dataset.sex = patients.sex
dataset.region = registration.practice_stp
dataset.gp_practice = registration.practice_pseudo_id
dataset.registration_date = registration.start_date
dataset.long_covid_dx = lc_dx.exists_for_patient().map_values({True: 1, False: 0})
dataset.long_covid_dx_date = lc_dx_date
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