# Load previous data management
source("analysis/dm03_5_matched_pivot_long.R")

# Data management: GEE only runs using complete data
complete_vars <- c("patient_id", "exposure", "monthly_visits", "month", "follow_up_time") # only keep necessary variables
gee_crude_data <- matched_data_ts %>% dplyr::select(all_of(complete_vars)) %>% 
      filter(!is.na(patient_id) & !is.na(monthly_visits) & 
                   !is.na(exposure) & !is.na(follow_up_time) & 
                   !is.na(month) & (monthly_visits >=0) &
                   (follow_up_time > 0) )

# Ordering data by cluster and time
gee_crude_data <- arrange(gee_crude_data, patient_id, month)

# The hurdle model for the count data:

# The first part: crude binomial component: ----
# # Create binary variable for non-zero healthcare visits
gee_crude_data <- gee_crude_data %>% mutate(visits_binary = ifelse(monthly_visits>0, 1, 0))

# Run the binomial component:
hurdle_binomial <- geeglm(visits_binary ~ exposure,
                          data = gee_crude_data,
                          id = patient_id,
                          waves = month,
                          family = binomial(link = "logit") ,
                          corstr = "ar1")


# # Predict the probability of the first part 
gee_crude_data$prob_visits <- predict(hurdle_binomial, type = "response")

# The second part: crude truncated GEE Poisson regression component: ----
hurdle_gee <- geeglm( monthly_visits ~ exposure + offset(log(follow_up_time)),
                     data = subset(gee_crude_data, visits_binary >0),
                     id = patient_id,
                     waves = month,
                     family = poisson(link = "log") ,
                     corstr = "ar1")


# # Predict the second part:
gee_crude_predicted_data <- subset(gee_crude_data, visits_binary >0) # Can only predict among non-zero visits
gee_crude_predicted_data$model_visits <- predict(hurdle_gee, type =  "response")
gee_crude_predicted_data$model_visits %>% summary # less monthly visits now 


# Combining two parts to get the expected visits: 
gee_crude_predicted_data$predicted_hurdle <- gee_crude__predicted_data$model_visits*gee_crude__predicted_data$prob_visits
gee_crude_predicted_data$predicted_hurdle %>% summary



gee_crude_predicted_data %>% nrow

# # try prediction using the original data:
gee_crude_data$model_visits <- predict(hurdle_gee, gee_crude_data, type =  "response")
gee_crude_data$model_visits %>% summary # same as the subset data

gee_crude_data$predicted_hurdle <- gee_crude_data$model_visits*gee_crude_data$prob_visits
gee_crude_data$predicted_hurdle %>% summary # same outcome 


