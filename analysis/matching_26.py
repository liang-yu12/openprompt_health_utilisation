
import pandas as pd
from osmatching import match

match(
    case_csv="exp_stp_26.csv",
    match_csv="com_stp_26.csv",
    matches_per_case=5,
    match_variables={
        "age": 1,
        "sex": "category",
    },
    closest_match_variables=["age"],
    index_date_variable="index_date",
    replace_match_index_date_with_case="no_offset", 
    indicator_variable_name="exposure",
    date_exclusion_variables={
        "end_death": "before",
        "end_deregist": "before",
        "long_covid_dx_date": "before",
    },
    output_suffix="_stp_26",
    output_path="output",
)
