import datetime
from databuilder.ehrql import Dataset, days, years
from databuilder.tables.beta.tpp import (
    patients, addresses, ons_deaths, sgss_covid_all_tests,
    practice_registrations, clinical_events,
    vaccinations,
)
from ehrql.query_language import table_from_file, PatientFrame, Series
from covariates import *
from variables import add_visits, add_hos_visits, add_ae_visits
# Import matched data

@table_from_file("output/matched_matches_stp.csv")
class matched_matches(PatientFrame):
    age = Series(int)
    sex = Series(str)
    region = Series(str)
    gp_practice =  Series(int)
    registration_date = Series(date)
    long_covid_dx = Series(int)
    long_covid_dx_date= Series(date)
    end_death = Series(date)
    end_deregist = Series(date)
    end_lc_cure = Series(date)
    set_id = Series(int)
    exposure = Series(int)
    index_date = Series(date)


# Define dataset variables

dataset = Dataset()
dataset.define_population(
    (age >= 18)
    & matched_matches.exists_for_patient()
)

dataset.age = matched_matches.age
dataset.sex = matched_matches.sex
dataset.region = matched_matches.region
dataset.gp_practice = matched_matches.gp_practice
dataset.registration_date = matched_matches.registration_date
dataset.long_covid_dx = matched_matches.long_covid_dx
dataset.long_covid_dx_date= matched_matches.long_covid_dx_date
dataset.index_date = matched_matches.index_date
dataset.end_death = matched_matches.end_death
dataset.end_deregist = matched_matches.end_deregist
dataset.end_lc_cure = matched_matches.end_lc_cure
dataset.set_id = matched_matches.set_id
dataset.exposure = matched_matches.exposure

# Add previous covid hospitalisation
# 1. Previous hospitalized due to COVID (only look at hospitalisation before the index date)
previous_covid_hos = (hospitalisation_diagnosis_matches(hospital_admissions, codelists.hosp_covid)
    .where(hospital_admissions.admission_date < matched_matches.index_date)
    .sort_by(hospital_admissions.admission_date)
    .first_for_patient()
)

# Number of vaccines received before the index date and after study start date
all_vacc = (vaccinations.where(vaccinations.date < matched_matches.index_date)
    .where(vaccinations.date > study_start_date)
    .where(vaccinations.target_disease == "SARS-2 CORONAVIRUS")
    .sort_by(vaccinations.date)
)

c19_vaccine_number = all_vacc.count_for_patient()

create_sequential_variables(
    dataset,
    "covid_vacc_{n}_vacc_tab",
    num_variables=6,
    events=all_vacc,
    column="date"
)

hospital_stay_more_30 = hospital_admissions \
    .where(hospital_admissions.admission_date >= matched_matches.index_date) \
    .where(hospital_admissions.admission_date <= study_end_date) \
    .where(hospital_admissions.discharge_date.is_on_or_after(hospital_admissions.discharge_date)) \
    .where(hospital_admissions.discharge_date.is_after(hospital_admissions.admission_date + days(30))) \
    .count_for_patient()

dataset.covid_positive = latest_test_before_diagnosis.exists_for_patient()
dataset.covid_dx_month = latest_test_before_diagnosis.specimen_taken_date.to_first_of_month() # only need dx month
dataset.ethnicity = ethnicity
dataset.imd = imd
dataset.bmi = bmi
dataset.bmi_date = bmi_date
dataset.previous_covid_hosp = previous_covid_hos.exists_for_patient()
dataset.admit_over_1m_count = hospital_stay_more_30
dataset.cov_c19_vaccine_number = c19_vaccine_number
dataset.cov_cancer = cancer_all.exists_for_patient()
dataset.cov_mental_health = mental_health_issues.exists_for_patient()
dataset.cov_asthma = asthma.exists_for_patient() & ~copd.exists_for_patient()
dataset.cov_organ_transplant = organ_transplant.exists_for_patient()
dataset.cov_chronic_cardiac_disease = chronic_cardiac_disease.exists_for_patient()
dataset.cov_chronic_liver_disease = chronic_liver_disease.exists_for_patient()
dataset.cov_stroke_dementia = stroke.exists_for_patient() | dementia.exists_for_patient()
dataset.cov_other_neuro_diseases = other_neuro_diseases.exists_for_patient()
dataset.cov_ra_sle_psoriasis = ra_sle_psoriasis.exists_for_patient()
dataset.cov_asplenia = asplenia.exists_for_patient()
dataset.cov_hiv = hiv.exists_for_patient()
dataset.cov_aplastic_anemia = aplastic_anemia.exists_for_patient()
dataset.cov_permanent_immune_suppress = permanent_immune_suppress.exists_for_patient()
dataset.cov_temporary_immune_suppress = temporary_immune_suppress.exists_for_patient()

# Add outcomes: healthcare utilisation by months
# GP visit
add_visits(dataset, dataset.index_date, num_months=1)
add_visits(dataset, dataset.index_date, num_months=2)
add_visits(dataset, dataset.index_date, num_months=3)
add_visits(dataset, dataset.index_date, num_months=4)
add_visits(dataset, dataset.index_date, num_months=5)
add_visits(dataset, dataset.index_date, num_months=6)
add_visits(dataset, dataset.index_date, num_months=7)
add_visits(dataset, dataset.index_date, num_months=8)
add_visits(dataset, dataset.index_date, num_months=9)
add_visits(dataset, dataset.index_date, num_months=10)
add_visits(dataset, dataset.index_date, num_months=11)
add_visits(dataset, dataset.index_date, num_months=12)

# Hospital admission
add_hos_visits(dataset, dataset.index_date, num_months=1)
add_hos_visits(dataset, dataset.index_date, num_months=2)
add_hos_visits(dataset, dataset.index_date, num_months=3)
add_hos_visits(dataset, dataset.index_date, num_months=4)
add_hos_visits(dataset, dataset.index_date, num_months=5)
add_hos_visits(dataset, dataset.index_date, num_months=6)
add_hos_visits(dataset, dataset.index_date, num_months=7)
add_hos_visits(dataset, dataset.index_date, num_months=8)
add_hos_visits(dataset, dataset.index_date, num_months=9)
add_hos_visits(dataset, dataset.index_date, num_months=10)
add_hos_visits(dataset, dataset.index_date, num_months=11)
add_hos_visits(dataset, dataset.index_date, num_months=12)

# A&E
add_ae_visits(dataset, dataset.index_date, num_months=1)
add_ae_visits(dataset, dataset.index_date, num_months=2)
add_ae_visits(dataset, dataset.index_date, num_months=3)
add_ae_visits(dataset, dataset.index_date, num_months=4)
add_ae_visits(dataset, dataset.index_date, num_months=5)
add_ae_visits(dataset, dataset.index_date, num_months=6)
add_ae_visits(dataset, dataset.index_date, num_months=7)
add_ae_visits(dataset, dataset.index_date, num_months=8)
add_ae_visits(dataset, dataset.index_date, num_months=9)
add_ae_visits(dataset, dataset.index_date, num_months=10)
add_ae_visits(dataset, dataset.index_date, num_months=11)
add_ae_visits(dataset, dataset.index_date, num_months=12)


# Add drug prescription frequencies by BNF chapters 
# drugs: bnf ch1 : gi drugs
drug_1gi_number(dataset, dataset.index_date, num_months=1)
drug_1gi_number(dataset, dataset.index_date, num_months=2)
drug_1gi_number(dataset, dataset.index_date, num_months=3)
drug_1gi_number(dataset, dataset.index_date, num_months=4)
drug_1gi_number(dataset, dataset.index_date, num_months=5)
drug_1gi_number(dataset, dataset.index_date, num_months=6)
drug_1gi_number(dataset, dataset.index_date, num_months=7)
drug_1gi_number(dataset, dataset.index_date, num_months=8)
drug_1gi_number(dataset, dataset.index_date, num_months=9)
drug_1gi_number(dataset, dataset.index_date, num_months=10)
drug_1gi_number(dataset, dataset.index_date, num_months=11)
drug_1gi_number(dataset, dataset.index_date, num_months=12)

# drugs: bnf ch2: cv drugs
drug_2cv_number(dataset, dataset.index_date, num_months=1)
drug_2cv_number(dataset, dataset.index_date, num_months=2)
drug_2cv_number(dataset, dataset.index_date, num_months=3)
drug_2cv_number(dataset, dataset.index_date, num_months=4)
drug_2cv_number(dataset, dataset.index_date, num_months=5)
drug_2cv_number(dataset, dataset.index_date, num_months=6)
drug_2cv_number(dataset, dataset.index_date, num_months=7)
drug_2cv_number(dataset, dataset.index_date, num_months=8)
drug_2cv_number(dataset, dataset.index_date, num_months=9)
drug_2cv_number(dataset, dataset.index_date, num_months=10)
drug_2cv_number(dataset, dataset.index_date, num_months=11)
drug_2cv_number(dataset, dataset.index_date, num_months=12)

# drugs: bnf ch3 chest drugs 
drug_3chest_number(dataset, dataset.index_date, num_months=1)
drug_3chest_number(dataset, dataset.index_date, num_months=2)
drug_3chest_number(dataset, dataset.index_date, num_months=3)
drug_3chest_number(dataset, dataset.index_date, num_months=4)
drug_3chest_number(dataset, dataset.index_date, num_months=5)
drug_3chest_number(dataset, dataset.index_date, num_months=6)
drug_3chest_number(dataset, dataset.index_date, num_months=7)
drug_3chest_number(dataset, dataset.index_date, num_months=8)
drug_3chest_number(dataset, dataset.index_date, num_months=9)
drug_3chest_number(dataset, dataset.index_date, num_months=10)
drug_3chest_number(dataset, dataset.index_date, num_months=11)
drug_3chest_number(dataset, dataset.index_date, num_months=12)

# drugs: bnf ch4: cns
drug_4cns_number(dataset, dataset.index_date, num_months=1)
drug_4cns_number(dataset, dataset.index_date, num_months=2)
drug_4cns_number(dataset, dataset.index_date, num_months=3)
drug_4cns_number(dataset, dataset.index_date, num_months=4)
drug_4cns_number(dataset, dataset.index_date, num_months=5)
drug_4cns_number(dataset, dataset.index_date, num_months=6)
drug_4cns_number(dataset, dataset.index_date, num_months=7)
drug_4cns_number(dataset, dataset.index_date, num_months=8)
drug_4cns_number(dataset, dataset.index_date, num_months=9)
drug_4cns_number(dataset, dataset.index_date, num_months=10)
drug_4cns_number(dataset, dataset.index_date, num_months=11)
drug_4cns_number(dataset, dataset.index_date, num_months=12)

# drugs: bnf ch5: infectious
drug_5inf_number(dataset, dataset.index_date, num_months=1)
drug_5inf_number(dataset, dataset.index_date, num_months=2)
drug_5inf_number(dataset, dataset.index_date, num_months=3)
drug_5inf_number(dataset, dataset.index_date, num_months=4)
drug_5inf_number(dataset, dataset.index_date, num_months=5)
drug_5inf_number(dataset, dataset.index_date, num_months=6)
drug_5inf_number(dataset, dataset.index_date, num_months=7)
drug_5inf_number(dataset, dataset.index_date, num_months=8)
drug_5inf_number(dataset, dataset.index_date, num_months=9)
drug_5inf_number(dataset, dataset.index_date, num_months=10)
drug_5inf_number(dataset, dataset.index_date, num_months=11)
drug_5inf_number(dataset, dataset.index_date, num_months=12)

# drugs: bnf ch6: metabolism drugs
drug_6meta_number(dataset, dataset.index_date, num_months=1)
drug_6meta_number(dataset, dataset.index_date, num_months=2)
drug_6meta_number(dataset, dataset.index_date, num_months=3)
drug_6meta_number(dataset, dataset.index_date, num_months=4)
drug_6meta_number(dataset, dataset.index_date, num_months=5)
drug_6meta_number(dataset, dataset.index_date, num_months=6)
drug_6meta_number(dataset, dataset.index_date, num_months=7)
drug_6meta_number(dataset, dataset.index_date, num_months=8)
drug_6meta_number(dataset, dataset.index_date, num_months=9)
drug_6meta_number(dataset, dataset.index_date, num_months=10)
drug_6meta_number(dataset, dataset.index_date, num_months=11)
drug_6meta_number(dataset, dataset.index_date, num_months=12)

# drugs: bnf ch7 GYN drugs
drug_7gyn_number(dataset, dataset.index_date, num_months=1)
drug_7gyn_number(dataset, dataset.index_date, num_months=2)
drug_7gyn_number(dataset, dataset.index_date, num_months=3)
drug_7gyn_number(dataset, dataset.index_date, num_months=4)
drug_7gyn_number(dataset, dataset.index_date, num_months=5)
drug_7gyn_number(dataset, dataset.index_date, num_months=6)
drug_7gyn_number(dataset, dataset.index_date, num_months=7)
drug_7gyn_number(dataset, dataset.index_date, num_months=8)
drug_7gyn_number(dataset, dataset.index_date, num_months=9)
drug_7gyn_number(dataset, dataset.index_date, num_months=10)
drug_7gyn_number(dataset, dataset.index_date, num_months=11)
drug_7gyn_number(dataset, dataset.index_date, num_months=12)

# drugs: bnf ch8 cancer drugs
drug_8cancer_number(dataset, dataset.index_date, num_months=1)
drug_8cancer_number(dataset, dataset.index_date, num_months=2)
drug_8cancer_number(dataset, dataset.index_date, num_months=3)
drug_8cancer_number(dataset, dataset.index_date, num_months=4)
drug_8cancer_number(dataset, dataset.index_date, num_months=5)
drug_8cancer_number(dataset, dataset.index_date, num_months=6)
drug_8cancer_number(dataset, dataset.index_date, num_months=7)
drug_8cancer_number(dataset, dataset.index_date, num_months=8)
drug_8cancer_number(dataset, dataset.index_date, num_months=9)
drug_8cancer_number(dataset, dataset.index_date, num_months=10)
drug_8cancer_number(dataset, dataset.index_date, num_months=11)
drug_8cancer_number(dataset, dataset.index_date, num_months=12)

# drugs: bnf ch9 nutrition
drug_9diet_number(dataset, dataset.index_date, num_months=1)
drug_9diet_number(dataset, dataset.index_date, num_months=2)
drug_9diet_number(dataset, dataset.index_date, num_months=3)
drug_9diet_number(dataset, dataset.index_date, num_months=4)
drug_9diet_number(dataset, dataset.index_date, num_months=5)
drug_9diet_number(dataset, dataset.index_date, num_months=6)
drug_9diet_number(dataset, dataset.index_date, num_months=7)
drug_9diet_number(dataset, dataset.index_date, num_months=8)
drug_9diet_number(dataset, dataset.index_date, num_months=9)
drug_9diet_number(dataset, dataset.index_date, num_months=10)
drug_9diet_number(dataset, dataset.index_date, num_months=11)
drug_9diet_number(dataset, dataset.index_date, num_months=12)

# drugs: bnf ch10 muscle
drug_10muscle_number(dataset, dataset.index_date, num_months=1)
drug_10muscle_number(dataset, dataset.index_date, num_months=2)
drug_10muscle_number(dataset, dataset.index_date, num_months=3)
drug_10muscle_number(dataset, dataset.index_date, num_months=4)
drug_10muscle_number(dataset, dataset.index_date, num_months=5)
drug_10muscle_number(dataset, dataset.index_date, num_months=6)
drug_10muscle_number(dataset, dataset.index_date, num_months=7)
drug_10muscle_number(dataset, dataset.index_date, num_months=8)
drug_10muscle_number(dataset, dataset.index_date, num_months=9)
drug_10muscle_number(dataset, dataset.index_date, num_months=10)
drug_10muscle_number(dataset, dataset.index_date, num_months=11)
drug_10muscle_number(dataset, dataset.index_date, num_months=12)

# drugs: bnf ch11 eyes
drug_11eye_number(dataset, dataset.index_date, num_months=1)
drug_11eye_number(dataset, dataset.index_date, num_months=2)
drug_11eye_number(dataset, dataset.index_date, num_months=3)
drug_11eye_number(dataset, dataset.index_date, num_months=4)
drug_11eye_number(dataset, dataset.index_date, num_months=5)
drug_11eye_number(dataset, dataset.index_date, num_months=6)
drug_11eye_number(dataset, dataset.index_date, num_months=7)
drug_11eye_number(dataset, dataset.index_date, num_months=8)
drug_11eye_number(dataset, dataset.index_date, num_months=9)
drug_11eye_number(dataset, dataset.index_date, num_months=10)
drug_11eye_number(dataset, dataset.index_date, num_months=11)
drug_11eye_number(dataset, dataset.index_date, num_months=12)

# drugs: bnf ch12 ent
drug_12ent_number(dataset, dataset.index_date, num_months=1)
drug_12ent_number(dataset, dataset.index_date, num_months=2)
drug_12ent_number(dataset, dataset.index_date, num_months=3)
drug_12ent_number(dataset, dataset.index_date, num_months=4)
drug_12ent_number(dataset, dataset.index_date, num_months=5)
drug_12ent_number(dataset, dataset.index_date, num_months=6)
drug_12ent_number(dataset, dataset.index_date, num_months=7)
drug_12ent_number(dataset, dataset.index_date, num_months=8)
drug_12ent_number(dataset, dataset.index_date, num_months=9)
drug_12ent_number(dataset, dataset.index_date, num_months=10)
drug_12ent_number(dataset, dataset.index_date, num_months=11)
drug_12ent_number(dataset, dataset.index_date, num_months=12)

# drugs: bnf ch13 skin
drug_13skin_number(dataset, dataset.index_date, num_months=1)
drug_13skin_number(dataset, dataset.index_date, num_months=2)
drug_13skin_number(dataset, dataset.index_date, num_months=3)
drug_13skin_number(dataset, dataset.index_date, num_months=4)
drug_13skin_number(dataset, dataset.index_date, num_months=5)
drug_13skin_number(dataset, dataset.index_date, num_months=6)
drug_13skin_number(dataset, dataset.index_date, num_months=7)
drug_13skin_number(dataset, dataset.index_date, num_months=8)
drug_13skin_number(dataset, dataset.index_date, num_months=9)
drug_13skin_number(dataset, dataset.index_date, num_months=10)
drug_13skin_number(dataset, dataset.index_date, num_months=11)
drug_13skin_number(dataset, dataset.index_date, num_months=12)
