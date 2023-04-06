from datetime import date
from databuilder.ehrql import Dataset, days, years
from databuilder.tables.beta.tpp import (
    patients, addresses, ons_deaths, sgss_covid_all_tests,
    practice_registrations, clinical_events,
)
from databuilder.query_language import table_from_file, PatientFrame, Series
from covariates import *

@table_from_file("output/matched_cases_stp.csv")
class matched_cases(PatientFrame):
    patient_id = Series(int)
    age = Series(int)
    sex = Series(str)
    region = Series(str)
    gp_practice =  Series(int)
    registration_date = Series(date)
    long_covid_dx = Series(int)
    long_covid_dx_date= Series(date)
    index_date = Series(date)
    end_death = Series(date)
    end_deregist = Series(date)
    end_lc_cure = Series(date)
    set_id = Series(int)
    exposure = Series(int)
    match_counts = Series(int)

dataset = Dataset()
dataset.define_population(
    (age >= 18) 
    & registration.exists_for_patient() 
    & matched_cases.exists_for_patient()
)