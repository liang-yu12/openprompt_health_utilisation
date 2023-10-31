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
for (i in 1:12) {
      assign(paste0("drug_", i), exp_drug_counts[, grepl(paste0("_drug_", i, "$"), names(exp_drug_counts)), with = FALSE] %>% names() %>% as.vector())
}

# Calculate the row sums by months
for (i in 1:12) {
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


for (i in 1:12) {
      assign(paste0("drug_", i), com_drug_counts[, grepl(paste0("_drug_", i, "$"), names(com_drug_counts)), with = FALSE] %>% names() %>% as.vector())
}

# Calculate the row sums by months
for (i in 1:12) {
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


# Define the total healthcare utilisations: 
# concept: Primary care resource use = GP consultations + GP prescription
# total healthcare utilization: GP visits + GP prescriptions + Hospital admission + A&E visits + OPA visits

# set up primary care use vectors, prescription and gp visits
for (i in 1:12) {
      assign(paste0("primary_care_", i), c(paste0("prescription_", i), paste0("gp_visit_m", i)))
}

# # Exposure group: ----

# Primary care use by month: adding gp visits and prescription counts at the same month
for (i in 1:12) {
      primary_care_vector <- get(paste0("primary_care_", i))
      lc_exp_matched[[paste0("primary_care_use_", i)]] <- rowSums(lc_exp_matched[, primary_care_vector], na.rm = T)
}

# Define secondary healthcare visit vectors: need to exclude long COVID clinic visits (without opa LC visit)
for (i in 1:12) {
      visit_vector <- lc_exp_matched[, grepl(paste0("visit_m", i, "$"), names(lc_exp_matched)), with = FALSE] %>% names() %>% as.vector()
      visit_vector <- visit_vector[!visit_vector==paste0("opa_lc_visit_m", i)]
      visit_vector <- visit_vector[!visit_vector==paste0("gp_visit_m", i)]
      assign(paste0("secondary_care_", i), visit_vector)
}

# set up total visits vectors: all from primary + secondary
for (i in 1:12) {
      primary_care_vector <- get(paste0("primary_care_", i))
      secondary_care_vector <- get(paste0("secondary_care_", i))
      assign(paste0("total_care_", i), c(primary_care_vector, secondary_care_vector))
}

# Add primary and secondary care together: 
for (i in 1:12) {
      total_use <- get(paste0("total_care_", i))
      lc_exp_matched[[paste0("all_month_m", i)]] <- rowSums(lc_exp_matched[, total_use], na.rm = T)
}


#  # Control group: -----
# Define primary care use, by adding gp visits and prescription counts at the same month
for (i in 1:12) {
      primary_care_vector <- get(paste0("primary_care_", i))
      com_matched[[paste0("primary_care_use_", i)]] <- rowSums(com_matched[, primary_care_vector], na.rm = T)
}

# Add primary and secondary care together: 
for (i in 1:12) {
      total_use <- get(paste0("primary_care_", i))
      com_matched[[paste0("all_month_m", i)]] <- rowSums(com_matched[, total_use], na.rm = T)
}

# Outcomes summary: (i =1:12)
# 1. primary_care_use_i: gp visits + prescription counts 
# 2. gp_visit_mi: gp visits
# 3. prescription_i: prescription counts
# 4. hos_visit_mi: hospital admission counts
# 5. opa_visit_mi: outpatient clinic counts
# 6. ae_visit_mi: A&E visits counts
