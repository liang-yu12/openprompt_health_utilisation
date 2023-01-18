from datetime import date

from databuilder.ehrql import Dataset, days, years
from databuilder.tables.beta.tpp import (
    patients, addresses, appointments,
    practice_registrations, clinical_events,
    sgss_covid_all_tests, ons_deaths, 
)
from databuilder.codes import SNOMEDCTCode
from codelists import lc_codelists_combined
import codelists

study_start_date = date(2020, 11, 1)

# age 
age = (study_start_date - patients.date_of_birth).years

# current registration
registration = practice_registrations \
    .drop(practice_registrations.start_date > study_start_date - years(1)) \
    .drop(practice_registrations.end_date <= study_start_date) \
    .sort_by(practice_registrations.start_date).last_for_patient()

# long covid diagnoses
lc_dx = clinical_events.take(clinical_events.snomedct_code.is_in(lc_codelists_combined)) \
    .sort_by(clinical_events.date) \
    .first_for_patient() # had lc dx and dx dates

# covid tests month
latest_test_before_diagnosis = sgss_covid_all_tests \
    .take(sgss_covid_all_tests.is_positive) \
    .drop(sgss_covid_all_tests.specimen_taken_date >= lc_dx.date - days(30)) \
    .sort_by(sgss_covid_all_tests.specimen_taken_date) \
    .last_for_patient()
# # only need the diagnostic month for sensitivity analysis matching

# define end date: lc dx date +12 | death | derigistration | post COVID-19 syndrome resolved
one_year_after_start = lc_dx.date + days(365) 
death_date = ons_deaths.sort_by(ons_deaths.date) \
    .last_for_patient().date
end_reg_date = registration.end_date
lc_cure = clinical_events.take(clinical_events.snomedct_code ==  SNOMEDCTCode("1326351000000108")) \
    .sort_by(clinical_events.date) \
    .first_for_patient()
# #first recorded lc cure date

# GP visit 1 month after index date
gp_app_m1 = appointments \
    .take((appointments.start_date >= lc_dx.date) & (appointments.start_date <= (lc_dx.date + days(30)))) \
    .count_for_patient()

# GP visit 2 month after index date
gp_app_m2 = appointments \
    .take((appointments.start_date >= lc_dx.date) & (appointments.start_date <= (lc_dx.date + days(60)))) \
    .count_for_patient()

# GP visit 3 month after index date
gp_app_m3 = appointments \
    .take((appointments.start_date >= lc_dx.date) & (appointments.start_date <= (lc_dx.date + days(90)))) \
    .count_for_patient()

# GP visit 4 month after index date
gp_app_m4 = appointments \
    .take((appointments.start_date >= lc_dx.date) & (appointments.start_date <= (lc_dx.date + days(120)))) \
    .count_for_patient()

# GP visit 5 month after index date
gp_app_m5 = appointments \
    .take((appointments.start_date >= lc_dx.date) & (appointments.start_date <= (lc_dx.date + days(150)))) \
    .count_for_patient()

# GP visit 6 month after index date
gp_app_m6 = appointments \
    .take((appointments.start_date >= lc_dx.date) & (appointments.start_date <= (lc_dx.date + days(180)))) \
    .count_for_patient()

# GP visit 7 month after index date
gp_app_m7 = appointments \
    .take((appointments.start_date >= lc_dx.date) & (appointments.start_date <= (lc_dx.date + days(210)))) \
    .count_for_patient()

# GP visit 8 month after index date
gp_app_m8 = appointments \
    .take((appointments.start_date >= lc_dx.date) & (appointments.start_date <= (lc_dx.date + days(240)))) \
    .count_for_patient()

# GP visit 9 months after index date
gp_app_m9 = appointments \
    .take((appointments.start_date >= lc_dx.date) & (appointments.start_date <= (lc_dx.date + days(270)))) \
    .count_for_patient()

# GP visit 10 months after index date
gp_app_m10 = appointments \
    .take((appointments.start_date >= lc_dx.date) & (appointments.start_date <= (lc_dx.date + days(300)))) \
    .count_for_patient()

# GP visit 11 months after index date
gp_app_m11 = appointments \
    .take((appointments.start_date >= lc_dx.date) & (appointments.start_date <= (lc_dx.date + days(330)))) \
    .count_for_patient()

# GP visit 12 months after index date
gp_app_m12 = appointments \
    .take((appointments.start_date >= lc_dx.date) & (appointments.start_date <= (lc_dx.date + days(360)))) \
    .count_for_patient()

dataset = Dataset()
dataset.set_population((age >= 18) & registration.exists_for_patient())
dataset.age = age
dataset.sex = patients.sex
dataset.region = registration.practice_stp
dataset.gp_practice = registration.practice_pseudo_id
dataset.registration_date = registration.start_date
dataset.covid_positive = latest_test_before_diagnosis.exists_for_patient()
dataset.covid_dx_month = latest_test_before_diagnosis.specimen_taken_date.to_first_of_month() # only need dx month
dataset.long_covid_dx = lc_dx.exists_for_patient()
dataset.long_covid_dx_date = lc_dx.date
dataset.index_date = lc_dx.date
dataset.end_1y_after_index = one_year_after_start
dataset.end_death = death_date
dataset.end_deregist = end_reg_date
# dataset.end_lc_cure = lc_cure.date
dataset.gp_visit_m1 = gp_app_m1
dataset.gp_visit_m2 = gp_app_m2
dataset.gp_visit_m3 = gp_app_m3
dataset.gp_visit_m4 = gp_app_m4
dataset.gp_visit_m5 = gp_app_m5
dataset.gp_visit_m6 = gp_app_m6
dataset.gp_visit_m7 = gp_app_m7
dataset.gp_visit_m8 = gp_app_m8
dataset.gp_visit_m9 = gp_app_m9
dataset.gp_visit_m10 = gp_app_m10
dataset.gp_visit_m11 = gp_app_m11
dataset.gp_visit_m12 = gp_app_m12