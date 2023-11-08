# Load previous data management
source("analysis/dm02_01_now_pivot_total_visits_long.R")


# Show the factors associated with increased outcomes among:
# 1. All participants 
# 2. People with long COVID
# 3. People without long COVID

# 0. Common part:  -----

# Stats: two part (Hurdle) model -----

# Use a function to organised the regression outputs to get RR and CI:
# Tidy binomial model:
binomial_tidy_fn <- function(bi_reg){
      bi_results <- bi_reg %>% tidy() %>% mutate(
            model = "binomial",
            lci = exp(estimate - 1.96*std.error),
            hci = exp(estimate + 1.96*std.error),
            estimate = exp(estimate)) %>% 
            dplyr::select(model, term, estimate, lci, hci, p.value)
      return(bi_results)
}

# tidy vglm outputs:
positive_nb_tidy_fu <- function(vg_reg){
      
      t1 <- vg_reg %>%summary
      t2 <- t1@coef3 %>% as.data.frame()
      t2$term <- rownames(t2)
      results <- t2 %>% mutate(
            lci = exp(Estimate - 1.96*`Std. Error`),
            hci = exp(Estimate + 1.96*`Std. Error`),
            estimate = exp(Estimate),
            p.value = `Pr(>|z|)`,
            model = "Positive Negative Bionomial") %>% 
            dplyr::select(model, term, estimate, lci, hci, p.value)
      return(results)
}

# Predict average outcome:
predict_visit_fn <- function(dataset, part_1, part_2){
      input <- dataset %>% mutate(follow_up = 360)
      
      input$non_zero_prob <- predict(part_1, 
                                     newdata = input, 
                                     type = "response")
      
      p2 <- predict(part_2, 
                    newdata = input, 
                    type = "link", 
                    se.fit = T)
      
      input<- bind_cols(
            input, 
            (p2$fitted.values %>% as.data.frame() %>% 
                   dplyr::select(`loglink(munb)`) %>% 
                   rename(p_visits = `loglink(munb)`)),
            (p2$se.fit %>% as.data.frame()%>% 
                   dplyr::select(`loglink(munb)`) %>% 
                   rename(p_se = `loglink(munb)`))
      )
      input_c <- input %>% 
            mutate(c_visits = exp(p_visits)*non_zero_prob,
                   c_lci = exp(p_visits - 1.96*p_se)*non_zero_prob,
                   c_hci = exp(p_visits + 1.96*p_se)*non_zero_prob) %>% 
            dplyr::select(exposure, c_visits, c_lci, c_hci)
      
      # summarise the results
      results <- input_c %>% 
            summarise(
                  visits = mean(c_visits, na.rm =T),
                  lci = mean(c_lci, na.rm =T),
                  hci = mean(c_hci, na.rm = T)) 
}

# 1. All partricipants -------------------

matched_data_all_12m <- matched_data_12m %>% dplyr::select(-total_drug_visit)
# set binary visit outcomes
all_complete_12m <- matched_data_all_12m[complete.cases(matched_data_all_12m),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Model:

# 12 Months
all_binomial_12m <- glm(visits_binary ~ exposure + offset(log(follow_up)) +
                              age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                              previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat, 
                        data = all_complete_12m,
                        family=binomial(link="logit")) 

# 12 months
all_nb_12m <- vglm(visits ~ exposure+ offset(log(follow_up))+
                         age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                         previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat, 
                   family = posnegbinomial(),
                   data = subset(all_complete_12m, visits_binary > 0))

# Combine and organised regression outputs
all_binomial_outputs <- binomial_tidy_fn(all_binomial_12m) %>% 
      mutate(time="12 months") %>% 
      mutate(Adjustment = "Adjusted",
             Data = "Total")

# Organise the second part outputs:
all_hurdle_outputs <- positive_nb_tidy_fu(all_nb_12m) %>% 
      mutate(time="12 months") %>% 
      mutate(Adjustment = "Adjusted",
             Data = "Total")

# save outputs:
all_binomial_outputs %>% write_csv(here("output", "st04_02_all_factors_binomial.csv"))
all_hurdle_outputs %>% write_csv(here("output", "st04_02_all_factors_hurdle.csv"))


# Obtain results in absolute scale:
average_visits_total <- predict_visit_fn(dataset = all_complete_12m,
                                             part_1 = all_binomial_12m,
                                             part_2 = all_nb_12m) %>% 
      mutate(data_subset="All") %>% relocate(data_subset)

# 2. LC only: ------

# Data management for modeling:: 
# Collapsing data by summarising the visits and follow-up time, and 
# generate three datasets for follow-up 3m, 6m, and 12m

# follow 12 months 
matched_data_lc_12m <- matched_data_all_12m %>% subset(exposure == "Long covid exposure")

# correct the level of exposure groups
matched_data_lc_12m$exposure <- relevel(matched_data_lc_12m$exposure, ref = "Comparator")

# Adjusted hurdle model: 
# First need to clean the data by excluding obs with NA in variables:
adj_complete_12m <- matched_data_lc_12m[complete.cases(matched_data_lc_12m),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))


# Hurdle model part 1: binomial model:
# 12 Months
adj_binomial_12m <- glm(visits_binary ~ offset(log(follow_up)) +
                              age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                              previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat, 
                        data = adj_complete_12m,
                        family=binomial(link="logit")) 

# Hurdle model part 2: positive negative binomial model:

# Positive negative binomial
# 12 months
adj_nb_12m <- vglm(visits ~ offset(log(follow_up))+
                         age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                         previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat, 
                   family = posnegbinomial(),
                   data = subset(adj_complete_12m, visits_binary > 0))


# Combine and organised regression outputs
adj_binomial_outputs <-binomial_tidy_fn(adj_binomial_12m) %>% 
      mutate(time="12 months") %>% mutate(Adjustment = "Adjusted")

# Organise the second part outputs:
adj_hurdle_outputs <- positive_nb_tidy_fu(adj_nb_12m) %>% 
      mutate(time="12 months") %>% mutate(Adjustment = "Adjusted")

# Save the detailed outputs to a text file:
sink(here("output", "st04_02_lc_only_factor_reg_summary.txt"))
print("# Adjusted binomial model output part 1 ---------")
print(summary(adj_binomial_12m))
print("# Adjusted hurdle model output part 2 ---------")
print(summary(adj_nb_12m))
sink()

# Combine total outputs and save:
adj_binomial_outputs %>% write_csv(here("output", "st04_02_lc_only_factor_binomial.csv"))
adj_hurdle_outputs %>% write_csv(here("output", "st04_02_lc_only_factor_hurdle.csv"))


# results in asolute scale: 
average_visits_lc_only <- predict_visit_fn(dataset = adj_complete_12m,
                                           part_1 = adj_binomial_12m,
                                           part_2 = adj_nb_12m) %>% 
      mutate(data_subset="LC only") %>% relocate(data_subset)



# 3. Non-LC group subset:------

# follow 12 months 
matched_data_nolc_12m <- matched_data_all_12m %>% subset(exposure != "Long covid exposure")

# Adjusted hurdle model: 

# First need to clean the data by excluding obs with NA in variables:
nolc_complete_12m <- matched_data_nolc_12m[complete.cases(matched_data_nolc_12m),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))



# Hurdle model: 

# 12 Months
nolc_binomial_12m <- glm(visits_binary ~ offset(log(follow_up)) +
                              age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                              previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat, 
                        data = nolc_complete_12m,
                        family=binomial(link="logit")) 

# 12 months
nolc_nb_12m <- vglm(visits ~ offset(log(follow_up))+
                         age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                         previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat, 
                   family = posnegbinomial(),
                   data = subset(nolc_complete_12m, visits_binary > 0))

# Combine and organised regression outputs
nolc_binomial_outputs <- binomial_tidy_fn(nolc_binomial_12m) %>% 
      mutate(time="12 months") %>% 
      mutate(Adjustment = "Adjusted",
             Data = "No long COVID")

# Organise the second part outputs:
nolc_hurdle_outputs <- positive_nb_tidy_fu(nolc_nb_12m) %>% 
      mutate(time="12 months") %>% 
      mutate(Adjustment = "Adjusted",
             Data = "No long COVID")

# save outputs:
nolc_binomial_outputs %>% write_csv(here("output", "st04_02_nolc_factor_binomial.csv"))
nolc_hurdle_outputs %>% write_csv(here("output", "st04_02_nolc_factor_hurdle.csv"))


# results in absolute scale:
average_visits_nolc <- predict_visit_fn(dataset = nolc_complete_12m,
                                        part_1 = nolc_binomial_12m,
                                        part_2 = nolc_nb_12m) %>% 
      mutate(data_subset="No LC") %>% relocate(data_subset)



# 4. Combine the absolute scales outputs: -----
bind_rows(
      average_visits_total,
      average_visits_lc_only,
      average_visits_nolc) %>% 
      write_csv(here("output", "st04_02_factors_predicted_visits.csv"))



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
            summarise(
                  mean_visit = mean(visits),
                  min_visit = min(visits),
                  max_visit = max(visits),
                  n = n(),
                  demonimator = sum(follow_up))
}



# Summarise model data: Long covid only group
bind_rows(
      bi_model_count_fn(all_complete_12m) %>% mutate(time = "12m") %>% mutate(model = "All factors"),
      bi_model_count_fn(adj_complete_12m) %>% mutate(time = "12m") %>% mutate(model = "LC only"),
      bi_model_count_fn(nolc_complete_12m) %>% mutate(time = "12m") %>% mutate(model = "No LC only")) %>%
      write_csv("output/st04_02_factors_bi_model_counts.csv")

bind_rows(
      hurdle_model_count_fn(all_complete_12m) %>% mutate(time = "12m") %>% mutate(model = "All factors"),
      hurdle_model_count_fn(adj_complete_12m) %>% mutate(time = "12m") %>% mutate(model = "LC only"),
      hurdle_model_count_fn(adj_complete_12m) %>% mutate(time = "12m") %>% mutate(model = "No LC only")) %>% 
      write_csv("output/st04_02_factors_hurdle_model_counts.csv")

