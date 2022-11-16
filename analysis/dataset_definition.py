from datetime import date

from databuilder.ehrql import Dataset
from databuilder.tables.beta.tpp import patients, practice_registrations

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

age_okay = age >= 18
registered_okay = registration.exists_for_patient()

registration = practice_registrations \
    .drop(practice_registrations.start_date.difference_in_years(index_date) < 1) \
    .drop(practice_registrations.end_date <= index_date) \
    .sort_by(practice_registrations.start_date).last_for_patient()

dataset = Dataset()
dataset.set_population(age_okay & registered_okay)
dataset.age = age
dataset.registration_date = registration.start_date
dataset.historical_comparison_group = historical_registration.exists_for_patient()
