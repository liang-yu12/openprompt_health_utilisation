# Load previous data management
source("analysis/dm01_matched_current_data.R")

# ============== Caclulate follow-up time by month ============== 
# Explanation: people were followed from the index date, and we will estimate 
# the healthcare utilisation by month. However, we need to consider 
# end_date may fall within the monthly follow-up time. 
# Need to define monthly