# Load previous data management
source("analysis/dm02_04_now_pivot_ane_long.R")

# Stats: two part (Hurdle) model -----
# first need to exclude rows with NA and create 1/0 outcomes:
crude_vars <- c("visits", "exposure", "follow_up")#for crude anaylsis
crude_ae_complete_12m <- matched_data_ae_12m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Crude Hurdle model: ------
# Part 1: binomial model:
crude_binomial_12m <-  glm(visits_binary ~ exposure + offset(log(follow_up)), data = crude_ae_complete_12m,
                           family=binomial(link="logit")) 

# Part 2: Positive negative binomial (truncated)
crude_nb_12m <- vglm(visits ~ exposure + offset(log(follow_up)),
                     family = pospoisson(),
                     data = subset(crude_ae_complete_12m, visits_binary > 0))


# Use a function to organised the regression outputs to get RR and CI:
# Tidy binomial model:
binomial_tidy_fn <- function(bi_reg){
      bi_results <- bi_reg %>% tidy() %>% mutate(
            model = "binomial",
            lci = exp(estimate - 1.96*std.error),
            hci = exp(estimate + 1.96*std.error),
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
) %>% mutate(Adjustment = "Crude A&E visits")

# Organise the second part outputs:
crude_hurdle_outputs <- bind_rows(
      (positive_nb_tidy_fu(crude_nb_12m) %>% mutate(time="12 months"))
) %>% mutate(Adjustment = "Crude A&E visits")

# Adjusted hurdle model: -----
# First need to clean the data by excluding obs with NA in variables:
adj_ae_complete_12m <- matched_data_ae_12m[complete.cases(matched_data_ae_12m),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Hurdle model part 1: binomial model:
# 12 Months
adj_binomial_12m <- glm(visits_binary ~ exposure + offset(log(follow_up)) +
                              age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                              previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat, 
                        data = adj_ae_complete_12m,
                        family=binomial(link="logit")) 

# Hurdle model part 2: truncated negative binomial model:
# 12 months
adj_nb_12m <- vglm(visits ~ exposure + offset(log(follow_up))+
                         age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                         previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat, 
                   family = pospoisson(),
                   data = subset(adj_ae_complete_12m, visits_binary > 0))


# Combine and organised regression outputs
adj_binomial_outputs <-bind_rows(
      (binomial_tidy_fn(adj_binomial_12m) %>% mutate(time="12 months"))
) %>% mutate(Adjustment = "Adjusted A&E visits")

# Organise the second part outputs:
adj_hurdle_outputs <- bind_rows(
      (positive_nb_tidy_fu(adj_nb_12m) %>% mutate(time="12 months"))
) %>% mutate(Adjustment = "Adjusted A&E visits")

# Combine outputs
# Save both outputs: # Combine total outputs and save:
st03_04_ane_binomial <- bind_rows(crude_binomial_outputs, adj_binomial_outputs)
st03_04_ane_binomial %>% write_csv(here("output", "st02_04_ane_binomial.csv"))

st03_04_ane_hurdle <- bind_rows(crude_hurdle_outputs, adj_hurdle_outputs)
st03_04_ane_hurdle %>% write_csv(here("output", "st02_04_ane_hurdle.csv"))

# Save the detailed outputs to a text file:
sink(here("output", "st02_04_ane_reg_summary.txt"))
print("# Crude binomial model output part 1 ---------")
print(summary(crude_binomial_12m))
print("# Crude hurdle model output part 2 ---------")
print(summary(crude_nb_12m))
print("# Adjusted binomial model output part 1 ---------")
print(summary(adj_binomial_12m))
print("# Adjusted hurdle model output part 2 ---------")
print(summary(adj_nb_12m))
sink()


# Predict the average healthcare visits:  ----
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
      (average_visits_fn(dataset = crude_ae_complete_12m, 
                         reg_1st = crude_binomial_12m, 
                         reg_2nd = crude_nb_12m) %>% mutate(model = "Crude")),
      (average_visits_fn(dataset = adj_ae_complete_12m, 
                         reg_1st = adj_binomial_12m, 
                         reg_2nd = adj_nb_12m) %>% mutate(model = "Adjusted")))

summarised_results %>% write_csv(here("output", "st02_04_ane_predicted_counts.csv"))


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
      (bi_model_count_fn(crude_ae_complete_12m) %>% mutate(time = "12m") %>% 
             mutate(model = "Crude")),
      (bi_model_count_fn(adj_ae_complete_12m) %>% mutate(time = "12m")) %>% 
            mutate(model = "Adjusted")) %>% 
      write_csv("output/st02_04_ane_binomial_model_counts.csv")


bind_rows(
      (hurdle_model_count_fn(crude_ae_complete_12m) %>% mutate(time = "12m")) %>% 
            mutate(model = "Crude"),
      (hurdle_model_count_fn(adj_ae_complete_12m) %>% mutate(time = "12m")) %>% 
            mutate(model = "Adjusted")) %>% 
      write_csv("output/st02_04_ane_hurdle_model_counts.csv")