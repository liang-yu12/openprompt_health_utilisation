from datetime import date

from databuilder.ehrql import Dataset, days, years
from databuilder.tables.beta.tpp import (
    patients, addresses, 
    practice_registrations, clinical_events,
    sgss_covid_all_tests,
    vaccinations
)
from codelists import lc_codelists_combined

index_date = date(2020, 11, 1)
age = (index_date - patients.date_of_birth).years

# current registration
registration = practice_registrations \
    .drop(practice_registrations.start_date > index_date - years(1)) \
    .drop(practice_registrations.end_date <= index_date) \
    .sort_by(practice_registrations.start_date).last_for_patient()

# historical registration
historical_registration = practice_registrations \
    .drop(practice_registrations.start_date > date(2018, 11, 1)) \
    .drop(practice_registrations.end_date < date(2019, 11, 1))

# long covid diagnoses
lc_dx = clinical_events.take(clinical_events.snomedct_code.is_in(lc_codelists_combined)) \
    .sort_by(clinical_events.date) \
    .first_for_patient()

# covid tests
latest_test_before_diagnosis = sgss_covid_all_tests \
    .take(sgss_covid_all_tests.is_positive) \
    .drop(sgss_covid_all_tests.specimen_taken_date >= lc_dx.date - days(30)) \
    .sort_by(sgss_covid_all_tests.specimen_taken_date).last_for_patient()

dataset = Dataset()
dataset.set_population((age >= 18) & registration.exists_for_patient())
dataset.age = age
dataset.registration_date = registration.start_date
dataset.historical_comparison_group = historical_registration.exists_for_patient()
dataset.has_lc_dx = lc_dx.exists_for_patient()
dataset.dx_date = lc_dx.date
dataset.has_positive_covid_test = latest_test_before_diagnosis.exists_for_patient()
dataset.date_of_latest_positive_test_before_diagnosis = latest_test_before_diagnosis.specimen_taken_date
