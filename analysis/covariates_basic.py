# Explanation: 
# These covariates will be added to the matched datasets, so the final set.population 
# codes will be removed later when the importing CSV file function is enable.

# local variables for defining covariates
from datetime import date
from databuilder.ehrql import Dataset, days, years,  case, when
from databuilder.tables.beta.tpp import (
    patients, addresses, appointments,
    practice_registrations, clinical_events,
    sgss_covid_all_tests, ons_deaths, hospital_admissions,
)
from databuilder.codes import CTV3Code, DMDCode, ICD10Code, SNOMEDCTCode, Codelist
import codelists

from variables import add_visits

study_start_date = date(2020, 11, 1)


age = (study_start_date - patients.date_of_birth).years

# Ethnicity ---

ethnicity = clinical_events.take(clinical_events.ctv3_code.is_in(codelists.ethnicity)) \
    .sort_by(clinical_events.date) \
    .last_for_patient() \
    .ctv3_code.to_category(codelists.ethnicity.Grouping_6)

# BMI ---

bmi_record = (
    clinical_events.take(
        clinical_events.snomedct_code.is_in(
            [SNOMEDCTCode("60621009"), SNOMEDCTCode("846931000000101")]
        )
    )
    # Exclude out-of-range values
    .take((clinical_events.numeric_value > 4.0) & (clinical_events.numeric_value < 200.0))
    # Exclude measurements taken when patient was younger than 16
    .take((clinical_events.date >= patients.date_of_birth + years(16)) & (clinical_events.date >= study_start_date - years(5)))
    .sort_by(clinical_events.date)
    .last_for_patient()
)

bmi = bmi_record.numeric_value  # need to categorise them later. 
bmi_date = bmi_record.date


# SES: IMD
# IMD
# # 1. drop the start date records after index date
# # 2. sort the date, keep the latest

index_date_address = addresses.drop(addresses.start_date > study_start_date) \
    .sort_by(addresses.start_date) \
    .last_for_patient()


# The following codes will be removed later when the importing CSV file function is ready. 
# These codes are used for testing 

dataset = Dataset()
dataset.set_population(age >= 18)
dataset.ethnicity = ethnicity
dataset.bmi = bmi
dataset.bmi_date = bmi_date
dataset.imd= index_date_address.imd_rounded
