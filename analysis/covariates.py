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

# age  (only for set.population)
age = (study_start_date - patients.date_of_birth).years

# Demographic: ethnicity
## Ethnicity 
ethnicity = clinical_events.take(clinical_events.ctv3_code.is_in(codelists.ethnicity)) \
    .sort_by(clinical_events.date) \
    .last_for_patient() \
    .ctv3_code.to_category(codelists.ethnicity.Grouping_6)

## IMD
# # 1. drop the start date records after index date
# # 2. sort the date, keep the latest
imd = addresses.drop(addresses.start_date > study_start_date) \
    .sort_by(addresses.start_date) \
    .last_for_patient().imd_rounded

## BMI
bmi_record = (
    clinical_events.take(
        clinical_events.snomedct_code.is_in(
            [SNOMEDCTCode("60621009"), SNOMEDCTCode("846931000000101")]
        )
    )
    # Exclude out-of-range values
    .take((clinical_events.numeric_value > 4.0) & (clinical_events.numeric_value < 200.0))
    # Exclude measurements taken when patient was younger than 16
    .take(clinical_events.date >= patients.date_of_birth + years(16))
    .sort_by(clinical_events.date)
    .last_for_patient()
)

bmi = bmi_record.numeric_value
bmi_date = bmi_record.date


# Clinical factors:



# severe immunosuppression

# vaccine dose: at least one dose/one dose/two dose/three doses or more

# Mental issues:





# The following codes will be removed later when the importing CSV file function is ready. 
# Use these codes to test this is working. 
dataset = Dataset()
dataset.set_population(age>= 18)
dataset.ethnicity = ethnicity
dataset.imd = imd
dataset.bmi = bmi
dataset.bmi_date = bmi_date