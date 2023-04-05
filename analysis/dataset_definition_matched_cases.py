from databuilder.ehrql import Dataset, days, years
from databuilder.tables.beta.tpp import (
    patients, addresses, ons_deaths, sgss_covid_all_tests,
    practice_registrations, clinical_events,
)
from covariates import *
import csv 

with open("output/matched_cases_stp.csv") as csv_file:
    matched_cases = csv.DictReader(csv_file)



dataset = Dataset()
dataset.define_population((age >= 18) & registration.exists_for_patient() & matched_cases.exists_for_patient())
