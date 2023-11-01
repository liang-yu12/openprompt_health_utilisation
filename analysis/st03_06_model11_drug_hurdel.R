# Load previous data management
source("analysis/dm03_11_pivot_drug_long.R")

# Data management for modeling:: --------
# Collapsing data by summarising the visits and follow-up time, and 
# generate three datasets for follow-up 12m


# follow 12 months 
matched_data_drug_12m <- matched_data_drug_ts %>% 
  filter(!is.na(follow_up_time)) %>% 
  group_by(patient_id, exposure) %>% 
  summarise(
    visits = sum(monthly_drug_visits),
    follow_up = sum(follow_up_time)) %>% 
  ungroup()


# # Add covariates for adjustment
for_covariates <- matched_data_drug_ts %>% distinct(patient_id, exposure, .keep_all = T) %>% 
      dplyr::select("patient_id",     
                    "exposure",           
                    "age", "age_cat",               
                    "sex",                     
                    "bmi_cat",
                    "ethnicity_6",             
                    "imd_q5",                  
                    "region",      
                    "cov_asthma",
                    "cov_mental_health",   
                    "previous_covid_hosp",     
                    "cov_covid_vax_n_cat",     
                    "number_comorbidities_cat")
for_covariates$sex <- relevel(for_covariates$sex, ref = "male")
for_covariates$bmi_cat <- relevel(for_covariates$bmi_cat, ref = "Normal Weight")
for_covariates$ethnicity_6 <- relevel(for_covariates$ethnicity_6, ref = "White")
for_covariates$imd_q5 <- relevel(for_covariates$imd_q5, ref = "least_deprived")
for_covariates$region <- relevel(for_covariates$region, ref = "London" )
for_covariates$cov_mental_health <- relevel(for_covariates$cov_mental_health, ref = "FALSE")
for_covariates$previous_covid_hosp <- relevel(for_covariates$previous_covid_hosp, ref = "FALSE")
for_covariates$previous_covid_hosp <- relevel(for_covariates$previous_covid_hosp, ref = "FALSE")
for_covariates$cov_covid_vax_n_cat <- relevel(for_covariates$cov_covid_vax_n_cat, ref = "0 dose")
for_covariates$number_comorbidities_cat <- relevel(for_covariates$number_comorbidities_cat, ref = "0")

# # add covariates back to the summarised data frame

matched_data_drug_12m <- left_join(matched_data_drug_12m, for_covariates,
                                 by = c("patient_id" = "patient_id", "exposure" = "exposure"))

# correct the level of exposure groups
matched_data_drug_12m$exposure <- relevel(matched_data_drug_12m$exposure, ref = "Comparator")


# Stats: two part (Hurdle) model -----
# first need to exclude rows with NA and create 1/0 outcomes:
crude_vars <- c("visits", "exposure", "follow_up")#for crude anaylsis
crude_complete_drug_12m <- matched_data_drug_12m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Crude Hurdle model: ------

# Crude hurdle model: ----
# # 12 months
# binomial
crude_binomial_12m <-  glm(visits_binary ~ exposure + offset(log(follow_up)), data = crude_complete_drug_12m,
                           family=binomial(link="logit")) 
# Positive negative binomial (truncated)
crude_nb_12m <- vglm(visits ~ exposure + offset(log(follow_up)),
                     family = posnegbinomial(),
                     data = subset(crude_complete_drug_12m, visits_binary > 0))


# Use a function to organised the regression outputs to get RR and CI:
# Tidy binomial model:
binomial_tidy_fn <- function(bi_reg){
      bi_results <- bi_reg %>% tidy() %>% mutate(
            model = "binomial",
            lci = exp(estimate - 1.69*std.error),
            hci = exp(estimate + 1.69*std.error),
            estimate = exp(estimate)) %>% 
            dplyr::select(model, term, estimate, lci, hci, p.value)%>% 
            filter(term == "exposureLong covid exposure" )      
      return(bi_results)
}

# tidy vglm outputs:
positive_nb_tidy_fu <- function(vg_reg){
      
      t1 <- vg_reg %>%summary
      t2 <- t1@coef3 %>% as.data.frame()
      t2$term <- rownames(t2)
      t3 <- t2 %>% filter(term == "exposureLong covid exposure" )
      results <- t3 %>% mutate(
            lci = exp(Estimate - 1.96*`Std. Error`),
            hci = exp(Estimate + 1.96*`Std. Error`),
            estimate = exp(Estimate),
            p.value = `Pr(>|z|)`,
            model = "Positive Negative Bionomial") %>% 
            dplyr::select(model, term, estimate, lci, hci, p.value)
      return(results)
}


# Organise the first part outputs:
crude_binomial_outputs <-bind_rows(
       (binomial_tidy_fn(crude_binomial_12m) %>% mutate(time="12 months"))
) %>% mutate(Adjustment = "Crude drug")

# Organise the second part outputs:
crude_hurdle_outputs <- bind_rows(
      (positive_nb_tidy_fu(crude_nb_12m) %>% mutate(time="12 months"))
) %>% mutate(Adjustment = "Crude drug")


# Adjusted hurdle model: 
# First need to clean the data by excluding obs with NA in variables:
adj_drug_complete_12m <- matched_data_drug_12m[complete.cases(matched_data_drug_12m),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Run the adjusted model using the complete data:


# Hurdle model part 1: binomial model:
# 12 Months
adj_binomial_12m <- glm(visits_binary ~ exposure + offset(log(follow_up)) +
                              age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                              previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat, 
                        data = adj_drug_complete_12m,
                        family=binomial(link="logit")) 

# Hurdle model part 2: positive negative binomial model:

# Positive negative binomial
# 12 months
adj_nb_12m <- vglm(visits ~ exposure + offset(log(follow_up))+
                         age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                         previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat, 
                   family = posnegbinomial(),
                   data = subset(adj_drug_complete_12m, visits_binary > 0))

# Combine and organised regression outputs
adj_binomial_outputs <-bind_rows(
      (binomial_tidy_fn(adj_binomial_12m) %>% mutate(time="12 months"))
) %>% mutate(Adjustment = "drug Adjusted")

# Organise the second part outputs:
adj_hurdle_outputs <- bind_rows(
      (positive_nb_tidy_fu(adj_nb_12m) %>% mutate(time="12 months"))
) %>% mutate(Adjustment = "drug Adjusted")

# Save the detailed outputs to a text file:
sink(here("output", "st03_11_drug_reg_summary.txt"))
print("# Crude binomial model output part 1 ---------")
print(summary(crude_binomial_12m))
print("# Crude hurdle model output part 2 ---------")
print(summary(crude_nb_12m))
print("# Adjusted binomial model output part 1 ---------")
print(summary(adj_binomial_12m))
print("# Adjusted hurdle model output part 2 ---------")
print(summary(adj_nb_12m))
sink()

# Save both outputs: # Combine total outputs and save:
st03_11_drug_binomial <- bind_rows(crude_binomial_outputs, adj_binomial_outputs)
st03_11_drug_binomial %>% write_csv(here("output", "st03_11_drug_binomial.csv"))

st03_11_drug_hurdle <- bind_rows(crude_hurdle_outputs, adj_hurdle_outputs)
st03_11_drug_hurdle %>% write_csv(here("output", "st03_11_drug_hurdle.csv"))





# Predict the average healthcare visits in the secondary care sector:  ----
# function to predict the average adjusted visits:
average_visits_fn <- function(dataset, reg_1st, reg_2nd){
      
      dataset <- dataset %>% mutate(follow_up = 30*12)
      # part 1:
      p1 <- predict(reg_1st, type= "response") # predict the first part non-zero prob
      dataset$nonzero_prob <- p1 # add the probability to the original data
      # part 2: 
      p2 <- predictvglm(reg_2nd, newdata = dataset, type = "link", se.fit = T)
      dataset <- dataset %>% mutate(
            predict_visit = exp(p2$fitted.values),
            predict_lci = exp(p2$fitted.values - 1.96*p2$se.fit),
            predict_hci = exp(p2$fitted.values + 1.96*p2$se.fit)
      )
      # multiply p1 and p2
      dataset <- dataset %>% mutate(
            c_visit = nonzero_prob*predict_visit,
            c_lci = nonzero_prob*predict_lci,
            c_hci = nonzero_prob*predict_hci)
      
      results <- dataset %>% group_by(exposure) %>% 
            summarise(visits = mean(c_visit),
                      lci = mean(c_lci),
                      hci = mean(c_hci)
            )
      return(results)
}

# run the predict function and summarised the average vistis:
summarised_results <- bind_rows(
      (average_visits_fn(dataset = crude_complete_drug_12m, 
                         reg_1st = crude_binomial_12m, 
                         reg_2nd = crude_nb_12m) %>% mutate(model = "Crude")),
      (average_visits_fn(dataset = adj_drug_complete_12m, 
                         reg_1st = adj_binomial_12m, 
                         reg_2nd = adj_nb_12m) %>% mutate(model = "Adjusted")))

summarised_results %>% write_csv(here("output", "st03_11_drug_predicted_counts.csv"))


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
                  demonimator = sum(follow_up))
}



# Summarise binomial model data:
bind_rows(
      (bi_model_count_fn(crude_complete_drug_12m) %>% mutate(time = "12m") %>% 
            mutate(model = "Crude")),
      (bi_model_count_fn(adj_drug_complete_12m) %>% mutate(time = "12m")) %>% 
            mutate(model = "Adjusted")) %>% 
      write_csv("output/st03_11_drug_binomial_model_counts.csv")


bind_rows(
      (hurdle_model_count_fn(crude_complete_drug_12m) %>% mutate(time = "12m")) %>% 
            mutate(model = "Crude"),
      (hurdle_model_count_fn(adj_drug_complete_12m) %>% mutate(time = "12m")) %>% 
            mutate(model = "Adjusted")) %>% 
      write_csv("output/st03_11_drug_hurdle_model_counts.csv")
