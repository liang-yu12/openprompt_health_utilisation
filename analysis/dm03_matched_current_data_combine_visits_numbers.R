# Load previous data management
source("analysis/dm01_matched_current_data.R")

# two part model package:
library(twopartm) # will be merge to setting once the package is added

# 1. Overall healthcare utlisation: 

# Use a function to combine all healthcare visits in the same month:

sum_visits_fn <- function(df, pattern, suffix) {
      # Find all variables that match the pattern
      variables <- grep(pattern, names(df), value = TRUE)
      
      # Combine the values of the found variables into a single vector
      values <- do.call(sum, df[variables])
      
      # Create a new variable name with the suffix
      new_variable_name <- paste0(suffix, values)
      
      # Assign the new variable to the data frame
      df[[new_variable_name]] <- values
      
      return(df)
}

# Calculate monthly overall healthcare utilisation:
matched_data <- sum_visits_fn(matched_data, pattern = "_m1", suffix = "month_1") # m1
matched_data <- sum_visits_fn(matched_data, pattern = "_m2", suffix = "month_2") # m2
matched_data <- sum_visits_fn(matched_data, pattern = "_m3", suffix = "month_3") # m3
matched_data <- sum_visits_fn(matched_data, pattern = "_m4", suffix = "month_4") # m4
matched_data <- sum_visits_fn(matched_data, pattern = "_m5", suffix = "month_5") # m5
matched_data <- sum_visits_fn(matched_data, pattern = "_m6", suffix = "month_6") # m6
matched_data <- sum_visits_fn(matched_data, pattern = "_m7", suffix = "month_7") # m7
matched_data <- sum_visits_fn(matched_data, pattern = "_m8", suffix = "month_8") # m8
matched_data <- sum_visits_fn(matched_data, pattern = "_m9", suffix = "month_9") # m9
matched_data <- sum_visits_fn(matched_data, pattern = "_m10", suffix = "month_10") # m10
matched_data <- sum_visits_fn(matched_data, pattern = "_m11", suffix = "month_11") # m11
matched_data <- sum_visits_fn(matched_data, pattern = "_m12", suffix = "month_12") # m12
