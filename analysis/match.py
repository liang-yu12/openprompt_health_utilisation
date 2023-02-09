import pandas as pd
from osmatching import match

## MATCH SEPARATE STPS OF CASES AND CONTROLS

match(
    case_csv="output\dataset_exp_lc.csv",
    match_csv="output\dataset_comparator_filtered_gp.csv",
    matches_per_case=5,
    match_variables={
        "age": 1,
        "sex": "category",
        "region": "category",
    },
    closest_match_variables=["age"],
    replace_match_index_date_with_case="no_effect", 
    date_exclusion_variables={
        "death_date": "before",
        "dereg_date": "before",
        "first_known_covid19": "before",
        "covid_hosp": "before"
    },
    #  indicator_variable_name="indicatorVariableName", 
    output_suffix="_2019_stp10",
    output_path="output",
)