from datetime import date

from databuilder.ehrql import Dataset
from databuilder.tables.beta.tpp import patients, practice_registrations
from databuilder.codes import REGISTRY, Codelist, codelist_from_csv
from databuilder.tables.beta.tpp import (clinical_events)


# 1. Deal with codeslist
def combine_codelists(*codelists):
    codes = set()
    for codelist in codelists:
        codes.update(codelist.codes)
    return Codelist(codes=codes, category_maps={})

# import different codelists:
long_covid_assessment_codes = codelist_from_csv(
    "codelists/opensafely-assessment-instruments-and-outcome-measures-for-long-covid.csv",
    system="snomedct", 
    column = "code"
)     
    
long_covid_dx_codes =  codelist_from_csv(
    "codelists/opensafely-nice-managing-the-long-term-effects-of-covid-19.csv",
    system="snomedct", 
    column = "code"
) 

long_covid_referral_codes = codelist_from_csv(
    "codelists/opensafely-referral-and-signposting-for-long-covid.csv",
    system="snomedct", 
    column = "code"
) 

# combine long covid codelists
lc_codelists_combined = combine_codelists(
    long_covid_dx_codes,
    long_covid_referral_codes,
    long_covid_assessment_codes
)

# 2. Filter people with the diagnoses, keep the earliest record
lc_dx = clinical_events.take(clinical_events.snomedct_code.is_in(lc_codelists_combined)) \
    .sort_by(clinical_events.date) \
    .first_for_patient()

# generate dummy tidy dataset
dataset = Dataset()
dataset.set_population(lc_dx.exists_for_patient())
dataset.dx_date = lc_dx.date
dataset.lc_dx=lc_dx.snomedct_code