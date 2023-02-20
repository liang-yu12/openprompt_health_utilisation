from databuilder.codes import REGISTRY, Codelist, codelist_from_csv
from codelists import *

# 0. define a function to combine multiple codeslists
def combine_codelists(*codelists):
    codes = set()
    for codelist in codelists:
        codes.update(codelist.codes)
    return Codelist(codes=codes, category_maps={})

# 1. Long COVID
# # import different long COVID codelists:
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

# # Combine long covid codelists
lc_codelists_combined = combine_codelists(
    long_covid_dx_codes,
    long_covid_referral_codes,
    long_covid_assessment_codes
)

# 2. Ethnicities: 

ethnicity = codelist_from_csv(
    "codelists/opensafely-ethnicity.csv",
    system="ctv3",
    column="Code",
)


# 3. 