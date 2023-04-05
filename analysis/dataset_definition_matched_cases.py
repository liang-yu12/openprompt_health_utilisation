from databuilder.ehrql import Dataset, days, years
from databuilder.tables.beta.tpp import (
    patients, addresses, ons_deaths, sgss_covid_all_tests,
    practice_registrations, clinical_events,
)
from databuilder.tables import PatientFrame, Series, table_from_file
from covariates import *
import csv 


@table_from_file(["output/matched_cases_stp.csv"])
class matched_cases(PatientFrame):
  [patient_id] = Series([int])
  [age] = Series([int])
  [sex] = Series([str])
  [region] = Series([str])
  [gp_practice] = Series([int])
  [registration_date] = Series([date])

dataset = Dataset()
dataset.define_population((age >= 18) & registration.exists_for_patient() & matched_cases.exists_for_patient())
