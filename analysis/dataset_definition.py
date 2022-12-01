from datetime import date

from databuilder.ehrql import Dataset
from databuilder.tables.beta.tpp import (
    patients, 
    addresses,
    practice_registrations,
    vaccinations,
)

# Index date issue: How to defining comparator groups' index date 
# 1. For people with long COVID: the date being diagnosed as COVID. 
# 2. For people with COVID but no long COVID: 4 weeks after COVID diagnosis; match diagnostic month with LC people
# 3. For people without COVID: match diagnostic month of LC people

index_date = date(2020, 11, 1)
age = patients.date_of_birth.difference_in_years(index_date)


# end of follow-up 
# - Death
# - End of registration
# - end of follow-up 1 year after index date. 
# - Final date: 2022-12-31

# Age
age_okay = age >= 18

# Sex

# Ethnicity 
# # ?????

# IMD
# # 1. drop the start date records after index date
# # 2. sort the date, keep the latest

index_date_address = addresses.drop(addresses.start_date > index_date) \
    .sort_by(addresses.start_date) \
    .last_for_patient()

# Region
# # practice_registrations.practice_stp


# GP registration: registered to GP for at leat 1 year 
# # Current registration
registration = practice_registrations \
    .drop(practice_registrations.start_date.difference_in_years(index_date) < 1) \
    .drop(practice_registrations.end_date <= index_date) \
    .sort_by(practice_registrations.start_date).last_for_patient()

registered_okay = registration.exists_for_patient()

# # Historical registration
historical_registration = practice_registrations \
    .drop(practice_registrations.start_date > date(2018, 11, 1)) \
    .drop(practice_registrations.end_date < date(2019, 11, 1))


# Vaccination status:# Goal: record the number of doses (0/1/2/more) before index date
# class vaccinations(EventFrame):
#     vaccination_id = Series(int)
#     date = Series(datetime.date)
#     target_disease = Series(str)
#     product_name = Series(str)

# Generate dummy data

dataset = Dataset()
dataset.set_population(age_okay & registered_okay)
dataset.age = age
dataset.sex = patients.sex
dataset.imd = index_date_address.imd_rounded
dataset.urban_rural_classification = index_date_address.rural_urban_classification 
dataset.region = registration.practice_stp
dataset.registration_date = registration.start_date
dataset.historical_comparison_group = historical_registration.exists_for_patient()
dataset.gp_practice = registration.practice_pseudo_id
