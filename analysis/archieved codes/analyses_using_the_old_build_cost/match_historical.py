
import pandas as pd
from osmatching import match

match(
    case_csv="hx_unmatched_exp",
    match_csv="hx_dataset_comp_unmatched",
    matches_per_case= 5,
    match_variables={
        "age": 1,
        "sex": "category",
        "region": "category"
    },
    closest_match_variables=["age"],
    index_date_variable="index_date",
    replace_match_index_date_with_case="no_offset", 
    indicator_variable_name="lc_exposure",
    date_exclusion_variables={
        "end_death": "before",
        "end_deregist": "before",
        "end_lc_cure": "before"
    },
    output_suffix="_historical",
    output_path="output",
)

# direct matching works on local machine