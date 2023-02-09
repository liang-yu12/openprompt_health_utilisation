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
    replace_match_index_date_with_case="no_effect", 
    date_exclusion_variables={
        "death_date": "before",
        "end_reg_date": "before",
        "lc_date": "before",
    },
    #  indicator_variable_name="indicatorVariableName", 
    output_path="output",
)