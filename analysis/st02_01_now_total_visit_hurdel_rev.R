# Load previous data management
source("analysis/dm02_01_now_pivot_total_visits_long.R")

# Stats: two part (Hurdle) model -----

# Crude data management: first need to exclude rows with NA and create 1/0 outcomes:
crude_vars <- c("visits", "exposure", "follow_up")#for crude anaylsis

crude_complete_12m <- matched_data_12m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Adjusted hurdle model: 
# First need to clean the data by excluding obs with NA in variables:
adj_complete_12m <- matched_data_12m[complete.cases(matched_data_12m),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Summarize the datasets for output checking: -----

# Logit model count:
bi_model_count_fn <- function(data){
      
      data %>% group_by(exposure) %>% 
            summarise(
                  non_zero_count = sum(visits_binary > 0),
                  zero_count = sum(visits_binary == 0),
                  n = n()
            )
}

# Hurdle model count:
hurdle_model_count_fn <- function(data){
      
      data %>% filter(visits_binary>0) %>% 
            group_by(exposure) %>% 
            summarise(
                  mean_visit = mean(visits),
                  min_visit = min(visits),
                  max_visit = max(visits),
                  n = n(),
                  visits = sum(visits),
                  demonimator = sum(follow_up))
}



# Summarise binomial model data:
bind_rows(
         (bi_model_count_fn(crude_complete_12m) %>% mutate(time = "12m") %>% mutate(model = "Crude")),
         (bi_model_count_fn(adj_complete_12m) %>% mutate(time = "12m") %>% mutate(model = "Adjusted"))) %>% 
      write_csv("output/st02_01_total_binomial_model_counts_for_rev.csv")


bind_rows(
         (hurdle_model_count_fn(crude_complete_12m) %>% mutate(time = "12m") %>% mutate(model = "Crude")),
         (hurdle_model_count_fn(adj_complete_12m) %>% mutate(time = "12m") %>% mutate(model = "Adjusted"))) %>% 
      write_csv("output/st02_01_total_hurdle_model_counts_for_rev.csv")



