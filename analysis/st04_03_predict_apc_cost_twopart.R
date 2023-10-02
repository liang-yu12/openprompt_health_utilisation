source("analysis/dm04_02_02_apc_costs_pivot.R")

# Goal: analysing long COVID exposure and the APC cost outcomes
# Model: two-part model 

# Data management: colllapsing data by different follow-up time. -------
# 3 months 
matched_cost_3m <- matched_apc_cost_ts %>% 
      filter(month %in% c(1,2,3)& !is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            apc_cost = sum(monthly_apc_cost, na.rm =T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup()

# 6 months
matched_cost_6m <- matched_apc_cost_ts %>% 
      filter(month %in% c(1,2,3,4,5,6)& !is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            apc_cost = sum(monthly_apc_cost, na.rm =T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup()

# 12 months
matched_cost_12m <- matched_apc_cost_ts %>% 
      filter(!is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            apc_cost = sum(monthly_apc_cost, na.rm =T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup()


# # Add covariates for adjustment
for_covariates <- matched_apc_cost_ts %>% distinct(patient_id, exposure, .keep_all = T) %>% 
      dplyr::select("patient_id",     
                    "exposure",           
                    "age_cat",  "age",
                    "sex",                     
                    "bmi_cat",
                    "ethnicity_6",             
                    "imd_q5",                  
                    "region",      
                    "previous_covid_hosp",     
                    "cov_covid_vax_n_cat",     
                    "number_comorbidities_cat")

levels_check <- c("exposure", "age_cat", "sex", "bmi_cat", "ethnicity_6", "imd_q5",                  
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


# Crude model data management: -----
# exclude rows with NA in the model: 
crude_vars <- c("apc_cost", "exposure", "follow_up")

crude_apc_cost_complete_3m <- matched_cost_3m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(cost_binary = ifelse(apc_cost>0, 1, 0))
crude_apc_cost_complete_6m <- matched_cost_6m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(cost_binary = ifelse(apc_cost>0, 1, 0))
crude_apc_cost_complete_12m <- matched_cost_12m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(cost_binary = ifelse(apc_cost>0, 1, 0)) 

# Crude binomial reg (first part) -----
# Binomial model function:
crude_bi_fn <- function(dataset){
      glm(cost_binary ~ exposure + offset(log(follow_up)),
          data = dataset,
          family = binomial(link="logit")) 
}

# Run crude binomial regression 
crude_bi_3m <- crude_bi_fn(crude_apc_cost_complete_3m)
crude_bi_6m <- crude_bi_fn(crude_apc_cost_complete_6m)      
crude_bi_12m <- crude_bi_fn(crude_apc_cost_complete_12m)

# Function for tidying the binomial reg results:

tidy_binomial_fn <- function(logit_reg){
      part_binomial <-  tidy(logit_reg) %>% mutate(
            model = "binomial",
            lci = exp(estimate - 1.96*std.error),
            hci = exp(estimate + 1.96*std.error),
            estimate = exp(estimate)) %>% 
            dplyr::select(model, term, estimate, lci, hci, p.value)
      return(part_binomial)
}

crude_binomial_apc <- bind_rows(
      tidy_binomial_fn(crude_bi_3m) %>% mutate(time = "3 months"),
      tidy_binomial_fn(crude_bi_6m) %>% mutate(time = "6 months"),      
      tidy_binomial_fn(crude_bi_12m) %>% mutate(time = "12 months")) %>% 
      mutate(adjustment = "Crude") %>% 
      arrange(desc(term =="exposureLong covid exposure"))  # combine the binomial outputs

# Crude Gamma GLM function (Second part) -----
# Gamma GLM model function:
crude_gamma_fn <- function(dataset){
      glm(apc_cost ~ exposure + offset(log(follow_up)),
          data = subset(dataset, cost_binary>0),
          family = Gamma(link="log"))
}

# run crude gamma GLM 
crude_gamma_3m <- crude_gamma_fn(crude_apc_cost_complete_3m)
crude_gamma_6m <- crude_gamma_fn(crude_apc_cost_complete_6m)
crude_gamma_12m <- crude_gamma_fn(crude_apc_cost_complete_12m)

# Organise the gamma GLM outputs

tidy_gamma_glm_fn <- function(gamma_glm){
      part_gaama <- tidy(gamma_glm) %>% mutate(
            model = "Gamma GLM",
            lci = exp(estimate - 1.96*std.error),
            hci = exp(estimate + 1.96*std.error),
            estimate = exp(estimate)) %>% 
            dplyr::select(model, term, estimate, lci, hci, p.value)
      return(part_gaama)
}

crude_gamma_glm_apc <- bind_rows(
      tidy_gamma_glm_fn(crude_gamma_3m) %>% mutate(time = "3 months"),
      tidy_gamma_glm_fn(crude_gamma_6m) %>% mutate(time = "6 months"),
      tidy_gamma_glm_fn(crude_gamma_12m) %>% mutate(time = "12 months")) %>%
      mutate(adjustment = "Crude") %>% 
      arrange(desc(term =="exposureLong covid exposure")) 

# Adjusted models:

# Data management: keep complete data
adj_apc_cost_complete_3m <- matched_cost_3m[complete.cases(matched_cost_3m),] %>% 
      mutate(cost_binary = ifelse(apc_cost>0, 1, 0))
adj_apc_cost_complete_6m <- matched_cost_6m[complete.cases(matched_cost_6m),] %>% 
      mutate(cost_binary = ifelse(apc_cost>0, 1, 0))
adj_apc_cost_complete_12m <- matched_cost_12m[complete.cases(matched_cost_12m),] %>% 
      mutate(cost_binary = ifelse(apc_cost>0, 1, 0))

# Adjusted binomial model (First part) ----
# Adjusted binomial function:
adj_bi_fn <- function(dataset){
      glm(cost_binary ~ exposure + offset(log(follow_up))+ age + sex  + cov_covid_vax_n_cat + 
                bmi_cat + imd_q5 + ethnicity_6 + region + number_comorbidities_cat,
          data = dataset,
          family = binomial(link="logit"))
}

# run model:
adj_bi_3m <- adj_bi_fn(adj_apc_cost_complete_3m)
adj_bi_6m <- adj_bi_fn(adj_apc_cost_complete_6m)
adj_bi_12m <- adj_bi_fn(adj_apc_cost_complete_12m)

# Organise binomial outputs 
adj_binomial_apc <- bind_rows(
      tidy_binomial_fn(adj_bi_3m) %>% mutate(time = "3 months"),
      tidy_binomial_fn(adj_bi_6m) %>% mutate(time = "6 months"),
      tidy_binomial_fn(adj_bi_12m) %>% mutate(time = "12 months")) %>% 
      mutate(adjustement = "Adjusted")%>% 
      arrange(desc(term =="exposureLong covid exposure")) 

# adjusted Gamma GLM model (Second part) ----
# Adjusted Gamma glm function
adj_gamma_fn <- function(dataset){
      glm(apc_cost ~ exposure + offset(log(follow_up))+ age + sex  + cov_covid_vax_n_cat + 
            bmi_cat + imd_q5 + ethnicity_6 + region + number_comorbidities_cat,
          data = subset(dataset, cost_binary>0),
          family = Gamma(link="log")) 
}

adj_gamma_3m <- adj_gamma_fn(adj_apc_cost_complete_3m)
adj_gamma_6m <- adj_gamma_fn(adj_apc_cost_complete_6m)
adj_gamma_12m <- adj_gamma_fn(adj_apc_cost_complete_12m)


# Organise the output:
adj_gamma_glm_apc <- bind_rows(
      tidy_gamma_glm_fn(adj_gamma_3m) %>% mutate(time = "3 months"),       
      tidy_gamma_glm_fn(adj_gamma_6m) %>% mutate(time = "6 months"),       
      tidy_gamma_glm_fn(adj_gamma_12m) %>% mutate(time = "12 months")) %>% 
      mutate(adjustment = "Adjusted")%>% 
      arrange(desc(term =="exposureLong covid exposure")) 



# save the output:

# # Binomial part
bind_rows(crude_binomial_apc, adj_binomial_apc) %>% 
      write_csv(here("output", "st04_03_apc_cost_binomial_output.csv"))
# Gamma glm part 
bind_rows(crude_binomial_apc, crude_gamma_glm_apc,
      adj_binomial_apc, adj_gamma_glm_apc) %>% 
      write_csv(here("output", "st04_03_apc_cost_twopm_output.csv"))


# Crude prediction: -----
# Use the outputs from the previous models and run the prediction.
# the follow-up time are set according to the model length
# write function: 
predict_avg_apc_cost_fn <- function(dataset, fu_time, first_reg, sec_reg){
      
      # change the dataset fullow up time for the prediction
      input <- dataset %>% mutate(follow_up = fu_time)
      
      # predict non-zero visit chance
      input$nonzero_chance <- predict(first_reg, newdata = input, type = "response")
      
      # Predict the cost by using original data
      tpm <- predict(sec_reg, newdata = input, type ="link", se.fit =T)
      
      # Calculate 95% CI
      input <- input %>% mutate(
            twopm_cost= exp(tpm$fit),
            twopm_cost_lci = exp(tpm$fit - 1.96*tpm$se.fit),
            twopm_cost_hci = exp(tpm$fit + 1.96*tpm$se.fit))
      
      # Multiply the non-zero chance and the predicted costs
      input <- input %>% mutate(
            c_apc_cost = nonzero_chance*twopm_cost,
            c_apc_cost_lci =nonzero_chance*twopm_cost_lci,
            c_apc_cost_hci =nonzero_chance*twopm_cost_hci,)
      
      # Summarise the output:
      results <- input %>% group_by(exposure) %>% 
            summarise(cost=mean(c_apc_cost, na.rm =T),
                      lci=mean(c_apc_cost_lci, na.rm =T),
                      uci=mean(c_apc_cost_hci, na.rm =T)
            )
      return(results)
}

# run the prediction model and combine outcomes:
# Crude costs: ----
crude_apc_costs <- bind_rows(
      predict_avg_apc_cost_fn(dataset = matched_cost_3m, 
                             fu_time = 30*3,
                             first_reg = crude_bi_3m, 
                             sec_reg = crude_gamma_3m) %>% mutate(time="3 months"),
      predict_avg_apc_cost_fn(dataset = matched_cost_6m, 
                             fu_time = 30*6,
                             first_reg = crude_bi_6m, 
                             sec_reg = crude_gamma_6m) %>% mutate(time="6 months"),
      predict_avg_apc_cost_fn(dataset = matched_cost_12m, 
                             fu_time = 30*12,
                             first_reg = crude_bi_12m, 
                             sec_reg = crude_gamma_12m) %>% mutate(time="12 months")) %>% 
      mutate(adjustment = "Crude") %>% relocate(adjustment)

# Adjusted costs: ----
# combine outputs
adj_apc_costs <- bind_rows(
      predict_avg_apc_cost_fn(dataset = matched_cost_3m, 
                             fu_time = 30*3,
                             first_reg = adj_bi_3m, 
                             sec_reg = adj_gamma_3m) %>% mutate(time="3 months"),
      predict_avg_apc_cost_fn(dataset = matched_cost_6m, 
                             fu_time = 30*6,
                             first_reg = adj_bi_6m, 
                             sec_reg = adj_gamma_6m) %>% mutate(time="6 months"),
      predict_avg_apc_cost_fn(dataset = matched_cost_12m, 
                             fu_time = 30*12,
                             first_reg = adj_bi_12m, 
                             sec_reg = adj_gamma_12m) %>% mutate(time="12 months")) %>% 
      mutate(adjustment = "Adjusted") %>% relocate(adjustment)


total_apc_costs <- bind_rows(crude_apc_costs, adj_apc_costs) 

total_apc_costs %>% write_csv(here("output","st04_03_predict_apc_cost_tpm.csv"))
