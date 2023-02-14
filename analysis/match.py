import pandas as pd
from osmatching import match

## MATCH SEPARATE STPS OF CASES AND CONTROLS

match(
    case_csv="dataset_exp_lc",
    match_csv="dataset_comparator_filtered_gp",
    matches_per_case=5,
    match_variables={
        "age": 1,
        "sex": "category",
        "region": "category",
    },
    closest_match_variables=["age"],
    index_date_variable = "index_date",
    replace_match_index_date_with_case="no_offset", 
    indicator_variable_name = "exposure",
    date_exclusion_variables={
        "end_death": "before",
        "end_deregist": "before",
        "long_covid_dx_date": "before",
    },
    output_path="output",
)