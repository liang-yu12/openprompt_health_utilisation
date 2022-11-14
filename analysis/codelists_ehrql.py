from cohortextractor import (codelist, codelist_from_csv, combine_codelists)


# import different codelists:
long_covid_assessment_codes = codelist_from_csv(
    "codelists\opensafely-assessment-instruments-and-outcome-measures-for-long-covid.csv",
    system="snomedct", 
    column = "code"
) 
    
    
long_covid_dx_codes =  codelist_from_csv(
    "codelists\opensafely-nice-managing-the-long-term-effects-of-covid-19.csv",
    system="snomedct", 
    column = "code"
) 

long_covid_referral_codes = codelist_from_csv(
    "codelists\opensafely-referral-and-signposting-for-long-covid.csv",
    system="snomedct", 
    column = "code"
) 

# combine long covid codelists
lc_codelists_combined = combine_codelists(
    long_covid_dx_codes,
    long_covid_referral_codes,
    long_covid_assessment_codes
)