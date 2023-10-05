source("analysis/dm04_03_pivot_long.R")

# Goal: analysing long COVID exposure and the cost outcomes
# Model: two-part model 

# Data management: colllapsing data by different follow-up time. -------
# 3 months 
matched_cost_3m <- matched_cost_ts %>% 
      filter(month %in% c(1,2,3)& !is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            total_cost = sum(monthly_total_cost, na.rm =T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup()

# 6 months
matched_cost_6m <- matched_cost_ts %>% 
      filter(month %in% c(1,2,3,4,5,6)& !is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            total_cost = sum(monthly_total_cost, na.rm =T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup()

# 12 months
matched_cost_12m <- matched_cost_ts %>% 
      filter(!is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            total_cost = sum(monthly_total_cost, na.rm =T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup()


# # Add covariates for adjustment
for_covariates <- matched_cost_ts %>% distinct(patient_id, exposure, .keep_all = T) %>% 
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

levels_check <- c("exposure", "sex", "bmi_cat", "ethnicity_6", "imd_q5",                  
                  "region", "previous_covid_hosp", "cov_covid_vax_n_cat", "number_comorbidities_cat")


for_covariates$sex <- relevel(for_covariates$sex, ref = "male")
for_covariates$bmi_cat <- relevel(for_covariates$bmi_cat, ref = "Normal Weight")
for_covariates$ethnicity_6 <- relevel(for_covariates$ethnicity_6, ref = "White")
for_covariates$imd_q5 <- relevel(for_covariates$imd_q5, ref = "least_deprived")
for_covariates$region <- relevel(for_covariates$region, ref = "London" )
for_covariates$previous_covid_hosp <- relevel(for_covariates$previous_covid_hosp, ref = "FALSE")
for_covariates$cov_covid_vax_n_cat <- relevel(for_covariates$cov_covid_vax_n_cat, ref = "0 dose")
for_covariates$number_comorbidities_cat <- relevel(for_covariates$number_comorbidities_cat, ref = "0")

lapply(for_covariates[levels_check], levels) # need to correct some levels

# # add covariates back to the summarised data frame
matched_cost_3m <- left_join(matched_cost_3m, for_covariates,
                             by = c("patient_id" = "patient_id", "exposure" = "exposure"))

matched_cost_6m <- left_join(matched_cost_6m, for_covariates,
                             by = c("patient_id" = "patient_id", "exposure" = "exposure"))

matched_cost_12m <- left_join(matched_cost_12m, for_covariates,
                              by = c("patient_id" = "patient_id", "exposure" = "exposure"))

# Make sure the exposure level is correct
matched_cost_3m$exposure <- relevel(matched_cost_3m$exposure, ref = "Comparator")
matched_cost_6m$exposure <- relevel(matched_cost_6m$exposure, ref = "Comparator")
matched_cost_12m$exposure <- relevel(matched_cost_12m$exposure, ref = "Comparator")

# Stats: Two-part model: -----
# Data management: exclude rows with NA in the model: 
crude_vars <- c("total_cost", "exposure", "follow_up")

crude_cost_complete_3m <- matched_cost_3m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(cost_binary = ifelse(total_cost>0, 1, 0))
crude_cost_complete_6m <- matched_cost_3m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(cost_binary = ifelse(total_cost>0, 1, 0))
crude_cost_complete_12m <- matched_cost_3m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(cost_binary = ifelse(total_cost>0, 1, 0)) 

# Crude Two-part model: -----
# First part: binomial model 

curde_binomial_3m <- glm(cost_binary ~ exposure + offset(log(follow_up)),
                         data = crude_cost_complete_3m,
                         family = binomial(link="logit"))

curde_binomial_6m <- glm(cost_binary ~ exposure + offset(log(follow_up)),
                         data = crude_cost_complete_6m,
                         family = binomial(link="logit"))

curde_binomial_12m <- glm(cost_binary ~ exposure + offset(log(follow_up)),
                         data = crude_cost_complete_12m,
                         family = binomial(link="logit"))


# Write a function to organise the crude model outputs:
binary_output_fn <- function(bi_reg){
      
      # Binomial part: 
      results <-  tidy(bi_reg) %>% mutate(
            model = "binomial",
            lci = exp(estimate - 1.96*std.error),
            hci = exp(estimate + 1.96*std.error),
            estimate = exp(estimate)) %>% 
            dplyr::select(model, term, estimate, lci, hci, p.value)%>% 
        filter(term == "exposureLong covid exposure" )
      
      return(results)
      
}

crude_binary <- bind_rows(
      binary_output_fn(curde_binomial_3m) %>% mutate(time = "3m"),
      binary_output_fn(curde_binomial_6m) %>% mutate(time = "6m"),
      binary_output_fn(curde_binomial_12m) %>% mutate(time = "12m")
) %>% mutate(model = "Crude")



# Gamma glm part: 
crude_gamma_3m <-  glm(total_cost ~ exposure + offset(log(follow_up)),
                       data = subset(crude_cost_complete_3m, cost_binary>0),
                       family = Gamma(link="log"))

crude_gamma_6m <-  glm(total_cost ~ exposure + offset(log(follow_up)),
                       data = subset(crude_cost_complete_6m, cost_binary>0),
                       family = Gamma(link="log"))

crude_gamma_12m <-  glm(total_cost ~ exposure + offset(log(follow_up)),
                       data = subset(crude_cost_complete_12m, cost_binary>0),
                       family = Gamma(link="log"))


gamma_output_fn <- function(gamma_reg){
      
      # Binomial part: 
      results <-  tidy(gamma_reg) %>% mutate(
            model = "Gamma GLM",
            lci = exp(estimate - 1.96*std.error),
            hci = exp(estimate + 1.96*std.error),
            estimate = exp(estimate)) %>% 
            dplyr::select(model, term, estimate, lci, hci, p.value)%>% 
            filter(term == "exposureLong covid exposure" )
      
      return(results)
      
}

crude_gamma <- bind_rows(
      gamma_output_fn(crude_gamma_3m) %>% mutate(time = "3m"),
      gamma_output_fn(crude_gamma_6m) %>% mutate(time = "6m"),
      gamma_output_fn(crude_gamma_12m) %>% mutate(time = "12m")
) %>% mutate(model = "Crude")


# Adjusted two-part model: ----------
# Data management: keep complete data
adj_cost_complete_3m <- matched_cost_3m[complete.cases(matched_cost_3m),] %>% 
  mutate(cost_binary = ifelse(total_cost>0, 1, 0))
adj_cost_complete_6m <- matched_cost_6m[complete.cases(matched_cost_6m),] %>% 
  mutate(cost_binary = ifelse(total_cost>0, 1, 0))
adj_cost_complete_12m <- matched_cost_12m[complete.cases(matched_cost_12m),] %>% 
  mutate(cost_binary = ifelse(total_cost>0, 1, 0))


# First part: binomial model:

adj_binomial_3m <- glm(cost_binary ~ exposure + offset(log(follow_up))+ age + sex  + cov_covid_vax_n_cat + 
                               bmi_cat + imd_q5 + ethnicity_6 + region + number_comorbidities_cat,
                         data = adj_cost_complete_3m,
                         family = binomial(link="logit"))

adj_binomial_6m <- glm(cost_binary ~ exposure + offset(log(follow_up))+ age + sex  + cov_covid_vax_n_cat + 
                             bmi_cat + imd_q5 + ethnicity_6 + region + number_comorbidities_cat,
                       data = adj_cost_complete_6m,
                       family = binomial(link="logit"))

adj_binomial_12m <- glm(cost_binary ~ exposure + offset(log(follow_up))+ age + sex  + cov_covid_vax_n_cat + 
                             bmi_cat + imd_q5 + ethnicity_6 + region + number_comorbidities_cat,
                       data = adj_cost_complete_12m,
                       family = binomial(link="logit"))

adj_binary <- bind_rows(
      binary_output_fn(adj_binomial_3m) %>% mutate(time = "3m"),
      binary_output_fn(adj_binomial_6m) %>% mutate(time = "6m"),
      binary_output_fn(adj_binomial_12m) %>% mutate(time = "12m")
) %>% mutate(model = "Adjusted")


# Second part: Gamma GLM

adj_gamma_3m <-  glm(total_cost ~ exposure + offset(log(follow_up))+ age + sex  + cov_covid_vax_n_cat + 
                             bmi_cat + imd_q5 + ethnicity_6 + region + number_comorbidities_cat,
                       data = subset(adj_cost_complete_3m, cost_binary>0),
                       family = Gamma(link="log"))

adj_gamma_6m <-  glm(total_cost ~ exposure + offset(log(follow_up))+ age + sex  + cov_covid_vax_n_cat + 
                           bmi_cat + imd_q5 + ethnicity_6 + region + number_comorbidities_cat,
                     data = subset(adj_cost_complete_6m, cost_binary>0),
                     family = Gamma(link="log"))

adj_gamma_12m <-  glm(total_cost ~ exposure + offset(log(follow_up))+ age + sex  + cov_covid_vax_n_cat + 
                           bmi_cat + imd_q5 + ethnicity_6 + region + number_comorbidities_cat,
                     data = subset(adj_cost_complete_12m, cost_binary>0),
                     family = Gamma(link="log"))


adj_gamma <- bind_rows(
      gamma_output_fn(adj_gamma_3m) %>% mutate(time = "3m"),
      gamma_output_fn(adj_gamma_6m) %>% mutate(time = "6m"),
      gamma_output_fn(adj_gamma_12m) %>% mutate(time = "12m")
) %>% mutate(model = "Adjusted")


# Save the detailed outputs to a text file:
sink(here("output", "st04_01_total_reg_summary.txt"))
print("# Crude binomial model output part 1 ---------")
print(summary(crude_binary))
print("# Crude hurdle model output part 2 ---------")
print(summary(crude_gamma))
print("# Adjusted binomial model output part 1 ---------")
print(summary(adj_binary))
print("# Adjusted hurdle model output part 2 ---------")
print(summary(adj_gamma))
sink()



# Organised all outputs: -------
all_binary <- bind_rows(crude_binary, adj_binary)
all_gamma <- bind_rows(crude_gamma, adj_gamma)

all_binary %>% write_csv(here("output", "st04_01_total_cost_binary.csv"))
all_gamma%>% write_csv(here("output", "st04_01_total_cost_gammaglm.csv"))

# Predicting cost: ------

adj_predict_cost_fn <- function(dataset, fu_time, reg_1st, reg_2nd ){
      # change the dataset fullow up time for the prediction
      input <- dataset %>% mutate(follow_up = fu_time)
      
      # Part 1: predict the first part non-zero prob
      input$nonzero_prob <- predict(reg_1st, newdata = input,  type= "response")

      # Part 2: predict the second part visits
      p2 <- predict(reg_2nd, newdata = input, type = "response")
      
      # Calculate the confidence interval of the part 2, then multiply by the first part:
      results <- input %>% 
            mutate(predicted_cost = p2) %>% 
            mutate(c_cost = nonzero_prob*predicted_cost) %>% 
            group_by(exposure) %>% 
            summarise(visits = mean(c_cost) # summarised the results by exposure: 
            )
      
      return(results)
}

adj_predict_cost_fn(dataset = adj_cost_complete_12m,
                    fu_time = 30*12,
                    reg_1st = adj_binomial_12m,
                    reg_2nd = adj_gamma_12m) %>% 
      write_csv(here("output", "st04_01_total_cost_predicted_costs.csv"))




# Summarize the datasets for output checking: -----

# Logit model count:
bi_model_count_fn <- function(data){
      
      data %>% group_by(exposure) %>% 
            summarise(
                  non_zero_count = sum(cost_binary > 0),
                  zero_count = sum(cost_binary == 0),
                  n = n()
            )
}

# Gamma model count:
gamma_model_count_fn <- function(data){
      
      data %>% filter(cost_binary>0) %>% 
            group_by(exposure) %>% 
            summarise(
                  mean_cost = mean(total_cost),
                  min_cost = min(total_cost),
                  max_cost = max(total_cost),
                  n = n(),
                  demonimator = sum(follow_up))
}

# Summarise binomial model data:
bind_rows(
      bind_rows(
            bi_model_count_fn(crude_cost_complete_3m) %>% mutate(time = "3m"),
            bi_model_count_fn(crude_cost_complete_6m) %>% mutate(time = "6m"),
            bi_model_count_fn(crude_cost_complete_12m) %>% mutate(time = "12m")) %>% 
            mutate(model = "Crude"),
      bind_rows(
            bi_model_count_fn(adj_cost_complete_3m) %>% mutate(time = "3m"),
            bi_model_count_fn(adj_cost_complete_6m) %>% mutate(time = "6m"),
            bi_model_count_fn(adj_cost_complete_12m) %>% mutate(time = "12m")) %>% 
            mutate(model = "Adjusted")) %>% 
      write_csv("output/st04_01_total_binomial_model_counts.csv")


bind_rows(
      bind_rows(
            gamma_model_count_fn(crude_cost_complete_3m) %>% mutate(time = "3m"),
            gamma_model_count_fn(crude_cost_complete_6m) %>% mutate(time = "6m"),
            gamma_model_count_fn(crude_cost_complete_12m) %>% mutate(time = "12m")) %>% 
            mutate(model = "Crude"),
      bind_rows(
            gamma_model_count_fn(adj_cost_complete_3m) %>% mutate(time = "3m"),
            gamma_model_count_fn(adj_cost_complete_6m) %>% mutate(time = "6m"),
            gamma_model_count_fn(adj_cost_complete_12m) %>% mutate(time = "12m")) %>% 
            mutate(model = "Adjusted")) %>% 
      write_csv("output/st04_01_total_gamma_model_counts.csv")
