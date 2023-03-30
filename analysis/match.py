import pandas as pd

# List of numbers to replace "05" in filenames
numbers = ['05', '24', '26', '23', '06', '13', '49', '15', '41', '09', '17', '25', '16',
           '07', '42', '21', '40', '27', '14', '20', '29', '43', '08', '37', '22', '12',
           '33', '35', '36', '10', '44']

# Common code
common_code = '''
import pandas as pd
from osmatching import match

match(
    case_csv="exp_stp_{num}.csv",
    match_csv="com_stp_{num}.csv",
    matches_per_case=5,
    match_variables={{
        "age": 1,
        "sex": "category",
    }},
    closest_match_variables=["age"],
    index_date_variable="index_date",
    replace_match_index_date_with_case="no_offset", 
    indicator_variable_name="exposure",
    date_exclusion_variables={{
        "end_death": "before",
        "end_deregist": "before",
        "long_covid_dx_date": "before",
    }},
    output_suffix="_stp_{num}",
    output_path="output",
)
'''

# Loop over numbers and generate code for each file
for num in numbers:
    code = common_code.format(num=num)
    with open(f'matching_{num}.py', 'w') as f:
        f.write(code)



# numbers = ['24', '26', '23', '6', '13', '49', '15', '41', '09', '17', '25', '16', '07', '42', '21', '40', '27', '14', '20', '29', '43', '08', '37', '22', '12', '33', '35', '36', '10', '44']

# # Loop over numbers and generate code for each file
# for num in numbers:
#     yaml_str = '''
#   match_comparators_{num}:
#     run:
#       python:latest python analysis/matching_{num}.py
#     needs: [split_unmatched_data_by_stp_regions]
#     outputs: 
#       highly_sensitive:
#         matched_cases: output/matched_cases_stp_{num}.csv
#         matched_matches: output/matched_matches_stp_{num}.csv
#         matched_all: output/matched_combined_stp_{num}.csv
#       moderately_sensitive: 
#         matching_report: output/matching_report_stp_{num}.txt
# '''.format(num=num)
#     print(yaml_str) # replace with your preferred way to output or save the YAML string
