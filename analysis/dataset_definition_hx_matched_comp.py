import datetime
from databuilder.ehrql import Dataset, days, years
from databuilder.tables.beta.tpp import (
    patients, addresses, ons_deaths, sgss_covid_all_tests,
    practice_registrations, clinical_events,
)
from databuilder.query_language import table_from_file, PatientFrame, Series
from covariates import *
from variables import add_visits, add_hos_visits, add_ae_visits

# import matched data

@table_from_file("output/matched_matches_historical.csv")
class matched_hx_comparator(PatientFrame):
    age = Series(int)
    sex = Series(str)
    lc_exp = Series(int)    
    region = Series(str)
    set_id = Series(int)
    lc_exposure = Series(int)
    index_date = Series(date)


dataset = Dataset()
dataset.define_population(
    (age >= 18)
    & matched_hx_comparator.exists_for_patient()
)

dataset.age = matched_hx_comparator.age
dataset.sex = matched_hx_comparator.sex
dataset.region = matched_hx_comparator.region
dataset.lc_dx = matched_hx_comparator.lc_exp
dataset.index_date = matched_hx_comparator.index_date
dataset.exposure = matched_hx_comparator.lc_exposure


