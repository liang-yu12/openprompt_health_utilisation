from datetime import date

from databuilder.ehrql import Dataset
from databuilder.tables.beta.tpp import patients, practice_registrations, clinical_events

from codelists_ehrql import lc_codelists_combined

index_date = date(2020, 11, 1)
age = patients.date_of_birth.difference_in_years(index_date)

# current registration
registration = practice_registrations \
    .drop(practice_registrations.start_date.difference_in_years(index_date) < 1) \
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

dataset = Dataset()
dataset.set_population((age >= 18) & registration.exists_for_patient())
dataset.age = age
dataset.registration_date = registration.start_date
dataset.historical_comparison_group = historical_registration.exists_for_patient()
dataset.has_lc_dx = lc_dx.exists_for_patient()
dataset.dx_date = lc_dx.date
