# Load previous data management
source("analysis/dm03_matched_define_monthly_follow_up_time.R")



# 1. Estimating GP costs -------
# multiply the average costs:
# ref: https://doi.org/10.22024/UniKent%2F01.02.100519
# £42 per consultation
gp_cost <- c()
for (i in 1:12){
      gp_cost <- c(gp_cost, paste0("gp_cost_", i))
}

gp_visit <- c()
for (i in 1:12){
      gp_visit <- c(gp_visit, paste0("gp_visit_m", i))
}


# # exposure group:
lc_exp_matched[gp_cost] <- lapply(lc_exp_matched[gp_visit], function(x){x*42})

# # comparator group:
com_matched[gp_cost] <- lapply(com_matched[gp_visit], function(x){x*42})

# 2. Estimating the drug costs: ---------
# Read in the drug count data
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


# # 2.1 set the column vectors: ------
# ### GI
# costs 
gi_cost <- c()
for (i in 1:12) {
      gi_cost <- c(gi_cost, paste0("gi_cost_", i))
}
# drugs
gi_drug <- c()
for (i in 1:12) {
      gi_drug <- c(gi_drug, paste0("gi_drug_", i))
}


# ### CV:
# cost
cv_cost <- c()
for (i in 1:12) {
      cv_cost <- c(cv_cost, paste0("cv_cost_", i))
}
# drug:
cv_drug <- c()
for (i in 1:12) {
      cv_drug <- c(cv_drug, paste0("cv_drug_", i))
}


# ### chest
# cost
chest_cost <- c()
for (i in 1:12) {
      chest_cost <- c(chest_cost, paste0("chest_cost_", i))
}
# drugs:
chest_drug <- c()
for (i in 1:12){
      chest_drug <- c(chest_drug, paste0("chest_drug_", i))
}


#  ###  CNS
cns_cost <- c()
for (i in 1:12) {
      cns_cost <- c(cns_cost, paste0("cns_cost_", i))
}
# drugs:
cns_drug <- c()
for (i in 1:12){
      cns_drug <- c(cns_drug, paste0("cns_drug_", i))
}


# ### infection
# cost
inf_cost <- c()
for (i in 1:12) {
      inf_cost <- c(inf_cost, paste0("inf_cost_", i))
}
# drugs
inf_drug <- c()
for (i in 1:12) {
      inf_drug <- c(inf_drug, paste0("inf_drug_", i))
}

# ### metabolism drugs
# cost
meta_cost <- c()
for (i in 1:12) {
      meta_cost <- c(meta_cost, paste0("meta_cost_",i))
}
# drug
meta_drug <- c()
for (i in 1:12) {
      meta_drug <- c(meta_drug, paste0("meta_drug_",i))
}


# ### gyn
# cost
gyn_cost <- c()
for (i in 1:12) {
      gyn_cost <- c(gyn_cost, paste0("gyn_cost_",i))
}
# drug
gyn_drug <- c()
for (i in 1:12) {
      gyn_drug <- c(gyn_drug, paste0("gyn_drug_",i))
}


# ### cancer
# cost
cancer_cost <- c()
for (i in 1:12) {
      cancer_cost <- c(cancer_cost, paste0("cancer_cost_",i))
}
# drug
cancer_drug <- c()
for (i in 1:12) {
      cancer_drug <- c(cancer_drug, paste0("cancer_drug_",i))
}


# ### diet
# cost
diet_cost <- c()
for (i in 1:12) {
      diet_cost <- c(diet_cost, paste0("diet_cost_",i))
}
# drug
diet_drug <- c()
for (i in 1:12) {
      diet_drug <- c(diet_drug, paste0("diet_drug_",i))
}

# ### musculoskeletal
# cost
muscle_cost <- c()
for (i in 1:12) {
      muscle_cost <- c(muscle_cost, paste0("muscle_cost_",i))
}
# drug
muscle_drug <- c()
for (i in 1:12) {
      muscle_drug <- c(muscle_drug, paste0("muscle_drug_",i))
}


# #### opthalmology
# cost
eye_cost <- c()
for (i in 1:12) {
      eye_cost <- c(eye_cost, paste0("eye_cost_",i))
}
# drug
eye_drug <- c()
for (i in 1:12) {
      eye_drug <- c(eye_drug, paste0("eye_drug_",i))
}

# ### ent
# cost
ent_cost <- c()
for (i in 1:12) {
      ent_cost <- c(ent_cost, paste0("ent_cost_",i))
}
# drug
ent_drug <- c()
for (i in 1:12) {
      ent_drug <- c(ent_drug, paste0("ent_drug_",i))
}

# ### skin
# cost
skin_cost <- c()
for (i in 1:12) {
      skin_cost <- c(skin_cost, paste0("skin_cost_",i))
}
# drug
skin_drug <- c()
for (i in 1:12) {
      skin_drug <- c(skin_drug, paste0("skin_drug_",i))
}

# # 2.2 multiply the average costs -----

# GI costs: *5.71
exp_drug_counts[gi_cost] <- lapply(exp_drug_counts[gi_drug], function(x){x*5.71})
com_drug_counts[gi_cost] <- lapply(com_drug_counts[gi_drug], function(x){x*5.71})

# CV costs: *4.84
exp_drug_counts[cv_cost] <- lapply(exp_drug_counts[cv_drug], function(x){x*4.84})
com_drug_counts[cv_cost] <- lapply(com_drug_counts[cv_drug], function(x){x*4.84})

# chest: 14.47
exp_drug_counts[chest_cost] <- lapply(exp_drug_counts[chest_drug], function(x){x*14.47})
com_drug_counts[chest_cost] <- lapply(com_drug_counts[chest_drug], function(x){x*14.47})

# CNS: 6.93
exp_drug_counts[cns_cost] <- lapply(exp_drug_counts[cns_drug], function(x){x*6.93})
com_drug_counts[cns_cost] <- lapply(com_drug_counts[cns_drug], function(x){x*6.93})

# Infection: 5.26
exp_drug_counts[inf_cost] <- lapply(exp_drug_counts[inf_drug], function(x){x*5.26})
com_drug_counts[inf_cost] <- lapply(com_drug_counts[inf_drug], function(x){x*5.26})

# Metabolism: 13.04
exp_drug_counts[meta_cost] <- lapply(exp_drug_counts[meta_drug], function(x){x*13.04})
com_drug_counts[meta_cost] <- lapply(com_drug_counts[meta_drug], function(x){x*13.04})

# GYN: 7.82
exp_drug_counts[gyn_cost] <- lapply(exp_drug_counts[gyn_drug], function(x){x*7.82})
com_drug_counts[gyn_cost] <- lapply(com_drug_counts[gyn_drug], function(x){x*7.82})

# cancer: 37.16
exp_drug_counts[cancer_cost] <- lapply(exp_drug_counts[cancer_drug], function(x){x*37.16})
com_drug_counts[cancer_cost] <- lapply(com_drug_counts[cancer_drug], function(x){x*37.16})

# Diet:  11.42
exp_drug_counts[diet_cost] <- lapply(exp_drug_counts[diet_drug], function(x){x*11.42})
com_drug_counts[diet_cost] <- lapply(com_drug_counts[diet_drug], function(x){x*11.42})

# Muscle drugs: 5.03
exp_drug_counts[muscle_cost] <- lapply(exp_drug_counts[muscle_drug], function(x){x*5.03})
com_drug_counts[muscle_cost] <- lapply(com_drug_counts[muscle_drug], function(x){x*5.03})

# Eye: 9.98
exp_drug_counts[eye_cost] <- lapply(exp_drug_counts[eye_drug], function(x){x*9.98})
com_drug_counts[eye_cost] <- lapply(com_drug_counts[eye_drug], function(x){x*9.98})

# ENT: 6.81
exp_drug_counts[ent_cost] <- lapply(exp_drug_counts[ent_drug], function(x){x*6.81})
com_drug_counts[ent_cost] <- lapply(com_drug_counts[ent_drug], function(x){x*6.81})

# skin: 9.7
exp_drug_counts[skin_cost] <- lapply(exp_drug_counts[skin_drug], function(x){x*9.7})
com_drug_counts[skin_cost] <- lapply(com_drug_counts[skin_drug], function(x){x*9.7})



# 3. Combine main datasets and the drug costs datasets.
exp_drug_counts$exposure <- as.factor(exp_drug_counts$exposure)
lc_exp_matched <- lc_exp_matched %>% left_join(exp_drug_counts, by = c("patient_id" = "patient_id", "exposure" = "exposure"))

com_drug_counts$exposure <- as.factor(com_drug_counts$exposure)
com_matched <- com_matched %>% left_join(com_drug_counts, by = c("patient_id" = "patient_id", "exposure" = "exposure"))

# housekeeping
rm(exp_drug_counts, com_drug_counts)
