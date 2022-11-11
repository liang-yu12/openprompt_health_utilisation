from databuilder.ehrql import Dataset
from databuilder.codes import REGISTRY, Codelist, codelist_from_csv
from databuilder.tables.beta.tpp import (clinical_events, patients, practice_registrations)

lc_codelist = codelist_from_csv("codelists\opensafely-assessment-instruments-and-outcome-measures-for-long-covid.csv",\
    system="snomedct", column = "code") or codelist_from_csv("codelists\opensafely-nice-managing-the-long-term-effects-of-covid-19.csv",\
    system="snomedct", column = "code") or codelist_from_csv("codelists\opensafely-referral-and-signposting-for-long-covid.csv",\
    system="snomedct", column = "code")

# set up long covid criteria
lc_dx = clinical_events.snomedct_code.is_in(lc_codelist).sort_by(clinical_events.date).first_for_patient()

registration = practice_registrations \
    .drop(practice_registrations.start_date.difference_in_years(index_date) < 1) \
    .drop(practice_registrations.end_date <= index_date) \
    .sort_by(practice_registrations.start_date).last_for_patient()


dataset = Dataset()
dataset.set_population(registration & lc_dx)
dataset.long_covid = lc_dx.exists_for_patient()