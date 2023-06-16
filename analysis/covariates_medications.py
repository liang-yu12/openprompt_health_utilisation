# Codes for adding medications 

# local variables for defining covariates
from datetime import date
from databuilder.ehrql import Dataset, days, years,  case, when
from ehrql.tables.beta.core import medications
from databuilder.codes import CTV3Code, DMDCode, ICD10Code, SNOMEDCTCode
import codelists

from variables import (
    hospitalisation_diagnosis_matches,
    clinical_ctv3_matches, 
    create_sequential_variables,
)

study_start_date = date(2020, 11, 1)
study_end_date = date(2023, 3, 31)

prescriptions = (medications
    .where(date >= study_start_date)
    .where(date < study_end_date)
)
