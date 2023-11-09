from datetime import date

from databuilder.ehrql import Dataset, days, years
from databuilder.tables.beta.tpp import (
    patients, apcs_cost
)

study_start_date = date(2020, 11, 1)
study_end_date = date(2023, 1, 31)

age = (study_start_date - patients.date_of_birth).years

total_apc_cost = apcs_cost.where((apcs_cost.admission_date >= study_start_date) &
              (apcs_cost.admission_date <  study_end_date)) \
                .grand_total_payment_mff.sum_for_patient() 

dataset = Dataset()
dataset.define_population(
    (age>=18)
    & (age<=100) 
    & (patients.sex.contains("male"))
)
dataset.apc_cost = total_apc_cost