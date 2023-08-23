# Load previous data management
source("analysis/dm03_5_matched_pivot_long.R")

# Data management for modeling:: --------
# Collapsing data by summarising the visits and follow-up time, and 
# generate three datasets for follow-up 3m, 6m, and 12m

# # 3 months
matched_data_3m <- matched_data_ts %>% 
      filter(month %in% c(1,2,3)& !is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_visits),
            follow_up = sum(follow_up_time)) %>% 
      ungroup()

# # 6 months
matched_data_6m <- matched_data_ts %>% 
      filter(month %in% c(1,2,3,4,5,6)& !is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_visits),
            follow_up = sum(follow_up_time)) %>% 
      ungroup()

# follow 12 months 
matched_data_12m <- matched_data_ts %>% 
      filter(!is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_visits),
            follow_up = sum(follow_up_time)) %>% 
      ungroup()


# # Add covariates for adjustment
for_covariates <- matched_data_ts %>% distinct(patient_id, exposure, .keep_all = T) %>% 
      dplyr::select("patient_id",     
                    "exposure",           
                    "age_cat",                 
                    "sex",                     
                    "bmi_cat",
                    "ethnicity_6",             
                    "imd_q5",                  
                    "region",      
                    "previous_covid_hosp",     
                    "cov_covid_vax_n_cat",     
                    "number_comorbidities_cat")

# # add covariates back to the summarised data frame
matched_data_3m <- left_join(matched_data_3m, for_covariates,
                               by = c("patient_id" = "patient_id", "exposure" = "exposure"))

matched_data_6m <- left_join(matched_data_6m, for_covariates,
                             by = c("patient_id" = "patient_id", "exposure" = "exposure"))

matched_data_12m <- left_join(matched_data_12m, for_covariates,
                             by = c("patient_id" = "patient_id", "exposure" = "exposure"))

# correct the level of exposure groups
matched_data_3m$exposure <- relevel(matched_data_3m$exposure, ref = "Comparator")
matched_data_6m$exposure <- relevel(matched_data_6m$exposure, ref = "Comparator")
matched_data_12m$exposure <- relevel(matched_data_12m$exposure, ref = "Comparator")

# Stats: two part (Hurdle) model -----
# first need to exclude rows with NA and create 1/0 outcomes:
crude_vars <- c("visits", "exposure", "follow_up")#for crude anaylsis

crude_complete_3m <- matched_data_3m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))
crude_complete_6m <- matched_data_6m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))
crude_complete_12m <- matched_data_12m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Comparing outputs from packages and manually calculation:
crude_hurdle_3m<- hurdle(visits ~ exposure + offset(log(follow_up)), 
       data = crude_complete_3m,
       zero.dist = "binomial",
       dist = "negbin")

crude_binomial_3m <- glm(visits_binary ~ exposure + offset(log(follow_up)), data = crude_complete_3m,
                         family=binomial(link="logit")) 

crude_nb_3m <- glm.nb(visits ~ exposure + offset(log(follow_up)), 
                      data = subset(crude_complete_3m, visits_binary > 0), 
                      link = log)


# Hurdle model coef
crude_hurdle_3m%>% coef() %>% exp 
# Binomial part: the same
crude_binomial_3m%>% coef() %>% exp
# Truncated negative binomial: the results are different.
crude_nb_3m %>% coef() %>% exp 




# Calculate manually: 
# Model 1: crude binomial model: -----
# 3m
crude_binomial_3m <- glm(visits_binary ~ exposure + offset(log(follow_up)), data = crude_complete_3m,
                         family=binomial(link="logit")) 
# 6m
crude_binomial_6m <- glm(visits_binary ~ exposure, data = crude_complete_6m,
                         family=binomial(link="logit")) 
# 12m
crude_binomial_12m <- glm(visits_binary ~ exposure, data = crude_complete_12m,
                         family=binomial(link="logit")) 

# # Predict the chance of 1/0:
# 3m
crude_complete_3m$prob_visits <- predict(crude_binomial_3m, type = "response")
# 6m
crude_complete_6m$prob_visits <- predict(crude_binomial_6m, type = "response")
# 12m
crude_complete_12m$prob_visits <- predict(crude_binomial_12m, type = "response")


# Model 2: truncated negative binomial model: ------
# Data management: model through the non-zero outcomes
non_zero_3m <- subset(crude_complete_3m, visits_binary > 0)
non_zero_6m <- subset(crude_complete_6m, visits_binary > 0)
non_zero_12m <- subset(crude_complete_12m, visits_binary > 0)

# 3m 
nb_crude_3m <- glm.nb(visits ~ exposure + offset(log(follow_up)), 
                      data = non_zero_3m, link = log)
# 6m 
nb_crude_6m <- glm.nb(visits ~ exposure + offset(log(follow_up)), 
                      data = non_zero_6m, link = log)
# 12m 
nb_crude_12m <- glm.nb(visits ~ exposure + offset(log(follow_up)), 
                       data = non_zero_12m, link = log)

# # Predict the value:
crude_non_zero_3m <- predict(nb_crude_3m, se.fit = T, type = "response") %>% 
      as.data.frame() %>% cbind(non_zero_3m)

crude_non_zero_6m <- predict(nb_crude_6m, se.fit = T, type = "response") %>% 
      as.data.frame() %>% cbind(non_zero_6m)

crude_non_zero_12m <- predict(nb_crude_12m, se.fit = T, type = "response") %>% 
      as.data.frame() %>% cbind(non_zero_12m)

# Multiply the first part and the second part: ---------
crude_non_zero_3m <- crude_non_zero_3m %>% 
      mutate(visit_hurdle = prob_visits*fit) %>% 
      mutate(se_hurdle = prob_visits*se.fit)

crude_non_zero_6m <- crude_non_zero_6m %>% 
      mutate(visit_hurdle = prob_visits*fit) %>% 
      mutate(se_hurdle = prob_visits*se.fit)

crude_non_zero_12m <- crude_non_zero_12m %>% 
      mutate(visit_hurdle = prob_visits*fit) %>% 
      mutate(se_hurdle = prob_visits*se.fit)



crude_non_zero_3m %>% write_csv(here("output", "crude_non_zero_3m.csv.gz"))
crude_non_zero_6m %>% write_csv(here("output", "crude_non_zero_6m.csv.gz"))
crude_non_zero_12m %>% write_csv(here("output", "crude_non_zero_12m.csv.gz"))