# 1. Total visits: ----
# Load previous data management
source("analysis/dm02_01_now_pivot_total_visits_long.R")

# Crude data management: first need to exclude rows with NA and create 1/0 outcomes:
crude_vars <- c("visits", "exposure", "follow_up")#for crude anaylsis

crude_complete_12m <- matched_data_12m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Adjusted hurdle model: 
# First need to clean the data by excluding obs with NA in variables:
adj_complete_12m <- matched_data_12m[complete.cases(matched_data_12m),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Summarize the datasets for output checking: -----

# Hurdle model count:
hurdle_model_count_fn <- function(data){
      
      data %>% filter(visits_binary>0) %>% 
            group_by(exposure) %>% 
            summarise(
                  counts = n(),
                  visits = sum(visits),
                  person_times = sum(follow_up))
}

total_model_count <- bind_rows(
         (hurdle_model_count_fn(crude_complete_12m) %>% mutate(outcome = "Total visits") %>% mutate(model = "Crude")),
         (hurdle_model_count_fn(adj_complete_12m) %>% mutate(outcome = "Total visits") %>% mutate(model = "Adjusted"))) %>% 
      relocate(model) %>% relocate(outcome) 

# Clean other data management but keep the outcomes and reusable parts
rm(list=setdiff(ls(), c("crude_vars", "hurdle_model_count_fn", "total_model_count")))



# 2. GP only visits -----
# Load previous data management
source("analysis/dm02_02_02_now_pivot_gp_visits_long.R")


# Data management: outcome: gp visits only

# follow 12 months 
matched_data_gp_12m_t <- matched_data_gp_ts %>% 
      filter(!is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_gp_visits),
            follow_up = sum(follow_up_time)) %>% 
      ungroup()

matched_data_gp_12m_t <- left_join(matched_data_gp_12m_t, for_covariates,
                                   by = c("patient_id" = "patient_id", "exposure" = "exposure"))

# correct the level of exposure groups
matched_data_gp_12m$exposure <- relevel(matched_data_gp_12m$exposure, ref = "Comparator")

# Crude GP data: exclude rows with NA and create 1/0 outcomes:
crude_complete_gp_12m <- matched_data_gp_12m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Adjusted GP dataset need to clean the data by excluding obs with NA in variables:
adj_gp_complete_12m <- matched_data_gp_12m[complete.cases(matched_data_gp_12m),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))


# Summarise GP outcomes
gp_model_count <- bind_rows(
      (hurdle_model_count_fn(crude_complete_gp_12m) %>% mutate(outcome = "GP visits") %>% mutate(model = "Crude")),
      (hurdle_model_count_fn(adj_gp_complete_12m) %>% mutate(outcome = "GP visits") %>% mutate(model = "Adjusted"))) %>% 
      relocate(model) %>% relocate(outcome) 


rm(list=setdiff(ls(), c("crude_vars", "hurdle_model_count_fn", 
                        "total_model_count", "gp_model_count")))

# 3. Prescription visits -----
# Load previous data management
source("analysis/dm02_02_01_now_pivot_drug_visits_long.R")
# Crude data
crude_complete_drug_12m <- matched_data_drug_12m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Adjusted data
adj_drug_complete_12m <- matched_data_drug_12m[complete.cases(matched_data_drug_12m),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Summarise drug visits

drug_model_count <- bind_rows(
      (hurdle_model_count_fn(crude_complete_drug_12m) %>% mutate(outcome = "Prescription visits") %>% 
            mutate(model = "Crude")) ,
      (hurdle_model_count_fn(adj_drug_complete_12m) %>% mutate(outcome = "Prescription visits") %>% 
            mutate(model = "Adjusted"))) %>%       
            relocate(model) %>% relocate(outcome) 


rm(list=setdiff(ls(), c("crude_vars", "hurdle_model_count_fn", 
                        "total_model_count", "gp_model_count",
                        "drug_model_count")))

# 4. Hospitalisation visits ----
# Load previous data management
source("analysis/dm02_03_now_pivot_hos_long.R")

# Crude dataset
crude_hos_complete_12m <- matched_data_hos_12m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Adjusted dataset
adj_hos_complete_12m <- matched_data_hos_12m[complete.cases(matched_data_hos_12m),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Summarise hospitalisation counts
hos_model_count <- bind_rows(
      (hurdle_model_count_fn(crude_hos_complete_12m) %>% mutate(outcome = "Hospitalizations") %>% 
            mutate(model = "Crude")),
      (hurdle_model_count_fn(adj_hos_complete_12m) %>% mutate(outcome = "Hospitalizations") %>% 
            mutate(model = "Adjusted"))) %>%             
      relocate(model) %>% relocate(outcome) 

rm(list=setdiff(ls(), c("crude_vars", "hurdle_model_count_fn", 
                        "total_model_count", "gp_model_count",
                        "drug_model_count", "hos_model_count")))

# 5. A&E visits -----

# Load previous data management
source("analysis/dm02_04_now_pivot_ane_long.R")

# Crude dataset
crude_ae_complete_12m <- matched_data_ae_12m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Adjusted dataset
adj_ae_complete_12m <- matched_data_ae_12m[complete.cases(matched_data_ae_12m),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

ane_model_count <- 
      bind_rows(
            (hurdle_model_count_fn(crude_ae_complete_12m) %>% mutate(outcome = "A&E visits")) %>% 
                  mutate(model = "Crude"),
            (hurdle_model_count_fn(adj_ae_complete_12m) %>% mutate(outcome = "A&E visits")) %>% 
                  mutate(model = "Adjusted"))  %>%             
      relocate(model) %>% relocate(outcome) 

# House keeping
rm(list=setdiff(ls(), c("crude_vars", "hurdle_model_count_fn", 
                        "total_model_count", "gp_model_count",
                        "drug_model_count", "hos_model_count",
                        "ane_model_count")))


# 6. OPA visits ----
# Load previous data management
source("analysis/dm02_05_now_pivot_opa_long.R")

# Crude dataset
crude_opa_complete_12m <- matched_data_opa_12m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Adjusted dataset
adj_opa_complete_12m <- matched_data_opa_12m[complete.cases(matched_data_opa_12m),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Summarise model counts
opa_model_count <- bind_rows(
      (hurdle_model_count_fn(crude_opa_complete_12m) %>% mutate(outcome = "OPA visits")) %>% 
            mutate(model = "Crude"),
      (hurdle_model_count_fn(adj_opa_complete_12m) %>% mutate(outcome = "OPA visits")) %>% 
            mutate(model = "Adjusted"))  %>%             
      relocate(model) %>% relocate(outcome) 

# Combine all outcomes: ------
bind_rows(total_model_count, gp_model_count, drug_model_count, 
          hos_model_count, ane_model_count, opa_model_count) %>% 
      write_csv("output/st02_rev_model_counts.csv")
