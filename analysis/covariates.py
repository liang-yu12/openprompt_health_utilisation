# Functions for defining covariates
from datetime import date
from databuilder.ehrql import Dataset, days, years
from databuilder.tables.beta.tpp import (
    patients, addresses, appointments,
    practice_registrations, clinical_events,
    sgss_covid_all_tests, ons_deaths, hospital_admissions,
)
from databuilder.codes import SNOMEDCTCode
import codelists
from variables import add_visits

study_start_date = date(2020, 11, 1)

# Demographic: ethnicity
# # Ethnicity

ethnicity = clinical_events.take(clinical_events.ctv3_code.is_in(codelists.ethnicity)) \
    .sort_by(clinical_events.date) \
    .last_for_patient() \
    .ctv3_code.to_category(codelists.ethnicity.Grouping_6)

# SES: IMD

# IMD
# # 1. drop the start date records after index date
# # 2. sort the date, keep the latest

index_date_address = addresses.drop(addresses.start_date > study_start_date) \
    .sort_by(addresses.start_date) \
    .last_for_patient()

# Chronic comorbidities
# # Hypertension



# severe immunosuppression