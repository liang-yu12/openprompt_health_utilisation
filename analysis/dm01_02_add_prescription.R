# Load previous data management
source("analysis/dm01_01_matched_current_data.R")

# Import medication records ==========
exp_drug_counts <- read_csv("output/matched_cases_with_drug_costs.csv.gz",
                            col_types = cols(
                                  age = col_skip(),
                                  sex = col_skip(),
                                  index_date = col_skip(),
                                  end_death = col_skip(),
                                  end_deregist = col_skip(),
                                  end_lc_cure = col_skip(),
                                  end_date = col_skip()
                            ))

exp_drug_counts <- exp_drug_counts %>% mutate(exposure = ifelse(exposure ==1, "Long covid exposure", "Comparator"))

com_drug_counts <- read_csv("output/matched_control_with_drug_costs.csv.gz",
                            col_types = cols(
                                  age = col_skip(),
                                  sex = col_skip(),
                                  index_date = col_skip(),
                                  end_death = col_skip(),
                                  end_deregist = col_skip(),
                                  end_lc_cure = col_skip(),
                                  end_date = col_skip()
                            ))

com_drug_counts <- com_drug_counts %>% mutate(exposure = ifelse(exposure ==1, "Long covid exposure", "Comparator"))

# Data management of the exposure group: ------

# Group same drug counts by months
num_drugs <- 12
for (i in 1:num_drugs) {
      assign(paste0("drug_", i), exp_drug_counts[, grepl(paste0("_drug_", i, "$"), names(exp_drug_counts)), with = FALSE] %>% names() %>% as.vector())
}

# Calculate the row sums by months
for (i in 1:num_drugs) {
      drug_vector <- get(paste0("drug_", i))
      exp_drug_counts[[paste0("prescription_", i)]] <- rowSums(exp_drug_counts[, drug_vector], na.rm = T)
}

prescription <- c()
for (i in 1:12) {
      prescription <- c(prescription, paste0("prescription_", i))
}

exp_drug_counts <- exp_drug_counts %>% dplyr::select(patient_id, exposure, all_of(prescription))


# Data management of the comparator group: 
# Group same drugs use in the same month
for (i in 1:num_drugs) {
      assign(paste0("drug_", i), com_drug_counts[, grepl(paste0("_drug_", i, "$"), names(com_drug_counts)), with = FALSE] %>% names() %>% as.vector())
}

# Calculate the row sums by months
for (i in 1:num_drugs) {
      drug_vector <- get(paste0("drug_", i))
      com_drug_counts[[paste0("prescription_", i)]] <- rowSums(com_drug_counts[, drug_vector], na.rm = T)
}

com_drug_counts <- com_drug_counts %>% dplyr::select(patient_id, exposure, all_of(prescription))


# Combine the prescription counts and other visits:
lc_exp_matched <- left_join(lc_exp_matched, exp_drug_counts,
                            by = c("patient_id" = "patient_id", "exposure" = "exposure"))

com_matched <-  left_join(com_matched, com_drug_counts, 
                          by = c("patient_id" = "patient_id", "exposure" = "exposure"))

# housekeeping
rm(list = ls(pattern = "drug"))
