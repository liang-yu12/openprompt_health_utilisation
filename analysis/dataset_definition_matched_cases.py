from databuilder.ehrql import Dataset, days, years
from databuilder.tables.beta.tpp import (
    patients, addresses, ons_deaths, sgss_covid_all_tests,
    practice_registrations, clinical_events,
)
from covariates import *
import csv 

with open("output/matched_cases_stp.csv") as csv_file:
    reader = csv.DictReader(csv_file)
    case_id = [int(row["patient_id"]) for row in reader]


dataset = Dataset()
dataset.define_population((age >= 18) & registration.exists_for_patient() & case_id.exists_for_patient())
