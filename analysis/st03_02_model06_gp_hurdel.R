# Load previous data management
source("analysis/dm03_6_pivot_gp_long.R")

# Data management for modeling:: --------
# Collapsing data by summarising the visits and follow-up time, and 
# generate three datasets for follow-up 3m, 6m, and 12m
matched_data_gp_ts$month %>% table
# # 3 months
matched_data_gp_3m <- matched_data_gp_ts %>% 
  filter(month %in% c(1,2,3) & !is.na(follow_up_time)) %>% 
  group_by(patient_id, exposure) %>% 
  summarise(
    visits = sum(monthly_gp_visits),
    follow_up = sum(follow_up_time)) %>% 
  ungroup()

# # 6 months
matched_data_gp_6m <- matched_data_gp_ts %>% 
  filter(month %in% c(1,2,3,4,5,6) & !is.na(follow_up_time)) %>% 
  group_by(patient_id, exposure) %>% 
  summarise(
    visits = sum(monthly_gp_visits),
    follow_up = sum(follow_up_time)) %>% 
  ungroup()

# follow 12 months 
matched_data_gp_12m <- matched_data_gp_ts %>% 
  filter(!is.na(follow_up_time)) %>% 
  group_by(patient_id, exposure) %>% 
  summarise(
    visits = sum(monthly_gp_visits),
    follow_up = sum(follow_up_time)) %>% 
  ungroup()


# # Add covariates for adjustment
for_covariates <- matched_data_gp_ts %>% distinct(patient_id, exposure, .keep_all = T) %>% 
  dplyr::select("patient_id",     
                "exposure",           
                "age",                 
                "sex",                     
                "bmi_cat",
                "ethnicity_6",             
                "imd_q5",                  
                "region",      
                "previous_covid_hosp",     
                "cov_covid_vax_n_cat",     
                "number_comorbidities_cat")
for_covariates$sex <- relevel(for_covariates$sex, ref = "male")
for_covariates$bmi_cat <- relevel(for_covariates$bmi_cat, ref = "Normal Weight")
for_covariates$ethnicity_6 <- relevel(for_covariates$ethnicity_6, ref = "White")
for_covariates$imd_q5 <- relevel(for_covariates$imd_q5, ref = "least_deprived")
for_covariates$region <- relevel(for_covariates$region, ref = "London" )
for_covariates$previous_covid_hosp <- relevel(for_covariates$previous_covid_hosp, ref = "FALSE")
for_covariates$cov_covid_vax_n_cat <- relevel(for_covariates$cov_covid_vax_n_cat, ref = "0 dose")
for_covariates$number_comorbidities_cat <- relevel(for_covariates$number_comorbidities_cat, ref = "0")

# # add covariates back to the summarised data frame
matched_data_gp_3m <- left_join(matched_data_gp_3m, for_covariates,
                                by = c("patient_id" = "patient_id", "exposure" = "exposure"))

matched_data_gp_6m <- left_join(matched_data_gp_6m, for_covariates,
                                by = c("patient_id" = "patient_id", "exposure" = "exposure"))

matched_data_gp_12m <- left_join(matched_data_gp_12m, for_covariates,
                                 by = c("patient_id" = "patient_id", "exposure" = "exposure"))

# correct the level of exposure groups
matched_data_gp_3m$exposure <- relevel(matched_data_gp_3m$exposure, ref = "Comparator")
matched_data_gp_6m$exposure <- relevel(matched_data_gp_6m$exposure, ref = "Comparator")
matched_data_gp_12m$exposure <- relevel(matched_data_gp_12m$exposure, ref = "Comparator")


# Stats: two part (Hurdle) model -----
# first need to exclude rows with NA and create 1/0 outcomes:
crude_vars <- c("visits", "exposure", "follow_up")#for crude anaylsis

crude_complete_gp_3m <- matched_data_gp_3m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))
crude_complete_gp_6m <- matched_data_gp_6m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))
crude_complete_gp_12m <- matched_data_gp_12m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Crude Hurdle model: ------




# Crude hurdle model: ----
# # 3 months
# binomial model: 
crude_binomial_3m <-  glm(visits_binary ~ exposure + offset(log(follow_up)), data = crude_complete_gp_3m,
                          family=binomial(link="logit")) 
# Positive negative binomial (truncated)
crude_nb_3m <- vglm(visits ~ exposure + offset(log(follow_up)),
                    family = posnegbinomial(),
                    data = subset(crude_complete_gp_3m, visits_binary > 0))

# # 6 months:
# binomial
crude_binomial_6m <-  glm(visits_binary ~ exposure + offset(log(follow_up)), data = crude_complete_gp_6m,
                          family=binomial(link="logit")) 
# Positive negative binomial (truncated)
crude_nb_6m <- vglm(visits ~ exposure + offset(log(follow_up)),
                    family = posnegbinomial(),
                    data = subset(crude_complete_gp_6m, visits_binary > 0))

# # 12 months
# binomial
crude_binomial_12m <-  glm(visits_binary ~ exposure + offset(log(follow_up)), data = crude_complete_gp_12m,
                           family=binomial(link="logit")) 
# Positive negative binomial (truncated)
crude_nb_12m <- vglm(visits ~ exposure + offset(log(follow_up)),
                     family = posnegbinomial(),
                     data = subset(crude_complete_gp_12m, visits_binary > 0))



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
            lci = exp(Estimate - 1.69*`Std. Error`),
            hci = exp(Estimate + 1.69*`Std. Error`),
            estimate = exp(Estimate),
            p.value = `Pr(>|z|)`,
            model = "Positive Negative Bionomial") %>% 
            dplyr::select(model, term, estimate, lci, hci, p.value)
      return(results)
}


# Organise the first part outputs:
crude_binomial_outputs <-bind_rows(
      (binomial_tidy_fn(crude_binomial_3m) %>% mutate(time="3 months")),
      (binomial_tidy_fn(crude_binomial_6m) %>% mutate(time="6 months")),
      (binomial_tidy_fn(crude_binomial_12m) %>% mutate(time="12 months"))
) %>% mutate(Adjustment = "Crude GP")

# Organise the second part outputs:
crude_hurdle_outputs <- bind_rows(
      (positive_nb_tidy_fu(crude_nb_3m) %>% mutate(time="3 months")),
      (positive_nb_tidy_fu(crude_nb_6m) %>% mutate(time="6 months")),
      (positive_nb_tidy_fu(crude_nb_12m) %>% mutate(time="12 months"))
) %>% mutate(Adjustment = "Crude GP")


# Adjusted hurdle model: 
# First need to clean the data by excluding obs with NA in variables:
adj_gp_complete_3m <- matched_data_gp_3m[complete.cases(matched_data_gp_3m),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))
adj_gp_complete_6m <- matched_data_gp_6m[complete.cases(matched_data_gp_6m),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))
adj_gp_complete_12m <- matched_data_gp_12m[complete.cases(matched_data_gp_12m),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Run the adjusted model using the complete data:


# Hurdle model part 1: binomial model:
# 3 Months
adj_binomial_3m <- glm(visits_binary ~ exposure + offset(log(follow_up)) +
                             age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + 
                             previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat, 
                       data = adj_gp_complete_3m,
                       family=binomial(link="logit")) 

# 6 Months
adj_binomial_6m <- glm(visits_binary ~ exposure + offset(log(follow_up)) +
                             age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + 
                             previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat, 
                       data = adj_gp_complete_6m,
                       family=binomial(link="logit")) 

# 12 Months
adj_binomial_12m <- glm(visits_binary ~ exposure + offset(log(follow_up)) +
                              age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + 
                              previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat, 
                        data = adj_gp_complete_12m,
                        family=binomial(link="logit")) 

# Hurdle model part 2: positive negative binomial model:

# Positive negative binomial
# 3 months
adj_nb_3m <- vglm(visits ~ exposure + offset(log(follow_up))+
                        age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + 
                        previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat, 
                  family = posnegbinomial(),
                  data = subset(adj_gp_complete_3m, visits_binary > 0))

# 6 months 
adj_nb_6m <- vglm(visits ~ exposure + offset(log(follow_up))+
                        age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + 
                        previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat, 
                  family = posnegbinomial(),
                  data = subset(adj_gp_complete_6m, visits_binary > 0))

# 12 months
adj_nb_12m <- vglm(visits ~ exposure + offset(log(follow_up))+
                         age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + 
                         previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat, 
                   family = posnegbinomial(),
                   data = subset(adj_gp_complete_12m, visits_binary > 0))

# Combine and organised regression outputs
adj_binomial_outputs <-bind_rows(
      (binomial_tidy_fn(adj_binomial_3m) %>% mutate(time="3 months")),
      (binomial_tidy_fn(adj_binomial_6m) %>% mutate(time="6 months")),
      (binomial_tidy_fn(adj_binomial_12m) %>% mutate(time="12 months"))
) %>% mutate(Adjustment = "GP Adjusted")

# Organise the second part outputs:
adj_hurdle_outputs <- bind_rows(
      (positive_nb_tidy_fu(adj_nb_3m) %>% mutate(time="3 months")),
      (positive_nb_tidy_fu(adj_nb_6m) %>% mutate(time="6 months")),
      (positive_nb_tidy_fu(adj_nb_12m) %>% mutate(time="12 months"))
) %>% mutate(Adjustment = "GP Adjusted")



# Save both outputs: # Combine total outputs and save:
st03_02_gp_binomial <- bind_rows(crude_binomial_outputs, adj_binomial_outputs)
st03_02_gp_binomial %>% write_csv(here("output", "st03_02_gp_binomial.csv"))

st03_02_gp_hurdle <- bind_rows(crude_hurdle_outputs, adj_hurdle_outputs)
st03_02_gp_hurdle %>% write_csv(here("output", "st03_02_gp_hurdle.csv"))





# Predict the average healthcare visits in the secondary care sector:  ----
# function to predict the average adjusted visits:
average_visits_fn <- function(dataset, reg_1st, reg_2nd){
      # part 1:
      p1 <- predict(reg_1st, type= "response") # predict the first part non-zero prob
      dataset$nonzero_prob <- p1 # add the probability to the original data
      # part 2: 
      p2 <- predictvglm(reg_2nd, newdata = dataset, type = "terms", se.fit = T)
      dataset <- mutate(
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
      (average_visits_fn(dataset = adj_gp_complete_3m, 
                         reg_1st = adj_binomial_3m, 
                         reg_2nd = adj_nb_3m) %>% mutate(time = "3 months")),
      (average_visits_fn(dataset = adj_gp_complete_6m, 
                         reg_1st = adj_binomial_6m, 
                         reg_2nd = adj_nb_6m) %>% mutate(time = "6 months")),
      (average_visits_fn(dataset = adj_gp_complete_12m, 
                         reg_1st = adj_binomial_12m, 
                         reg_2nd = adj_nb_12m) %>% mutate(time = "12 months")))

summarised_results %>% write_csv(here("output", "st03_02_gp_predicted_counts.csv"))



