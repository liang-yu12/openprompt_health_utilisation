from datetime import date

from databuilder.ehrql import Dataset, days, years
from databuilder.tables.beta.tpp import (
    patients, addresses, 
    practice_registrations, clinical_events,
    sgss_covid_all_tests, ons_deaths,
)
from codelists import lc_codelists_combined
import pandas as pd
import codelists

study_start_date = date(2020, 11, 1)

# age 
age = (study_start_date - patients.date_of_birth).years

# current registration
registration = practice_registrations \
    .drop(practice_registrations.start_date > study_start_date - years(1)) \
    .drop(practice_registrations.end_date <= study_start_date) \
    .sort_by(practice_registrations.start_date).last_for_patient()

# long covid diagnoses
lc_dx = clinical_events.take(clinical_events.snomedct_code.is_in(lc_codelists_combined)) \
    .sort_by(clinical_events.date) \
    .first_for_patient() # had lc dx and dx dates

# covid tests month
latest_test_before_diagnosis = sgss_covid_all_tests \
    .take(sgss_covid_all_tests.is_positive) \
    .drop(sgss_covid_all_tests.specimen_taken_date >= lc_dx.date - days(30)) \
    .sort_by(sgss_covid_all_tests.specimen_taken_date) \
    .last_for_patient()
# only need the diagnostic month for sensitivity analysis matching


# define end date: lc dx date +12 | death | derigistration
one_year_after_start = lc_dx.date + days(365) 
death_date = ons_deaths.sort_by(ons_deaths.date) \
    .last_for_patient().date
end_reg_date = registration.end_date

# end_date = (one_year_after_start + death_date + end_reg_date) doesn't work



# minimum_for_patient()


# gp visit 1 month 

# .count_for_patient()


dataset = Dataset()
dataset.set_population((age >= 18) & registration.exists_for_patient())
dataset.age = age
dataset.sex = patients.sex
dataset.region = registration.practice_stp
dataset.gp_practice = registration.practice_pseudo_id
dataset.registration_date = registration.start_date
dataset.covid_positive = latest_test_before_diagnosis.exists_for_patient()
dataset.covid_dx_month = latest_test_before_diagnosis.specimen_taken_date.to_first_of_month() # only need dx month
dataset.long_covid_dx = lc_dx.exists_for_patient()
dataset.long_covid_dx_date = lc_dx.date
dataset.index_date = lc_dx.date
# dataset.end_date = end_follow / death_date / end_reg_date
