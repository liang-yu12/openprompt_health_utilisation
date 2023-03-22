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
dataset.exposure_comparator = lc_dx.exists_for_patient()  # indicate this is exposure group
dataset.long_covid_dx = lc_dx.exists_for_patient()
dataset.long_covid_dx_date = lc_dx.date
dataset.index_date = lc_dx.date
dataset.end_1y_after_index = one_year_after_start
dataset.end_death = death_date
dataset.end_deregist = end_reg_date
dataset.end_lc_cure = lc_cure.date
dataset.covid_positive = latest_test_before_diagnosis.exists_for_patient()
# dataset.covid_dx_month = latest_test_before_diagnosis.specimen_taken_date.to_first_of_month() # only need dx month



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


