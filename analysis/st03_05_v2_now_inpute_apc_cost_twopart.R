source("analysis/dm03_03_v2_now_pivot_total_long_inputed_cost.R")

# Data management: change file names and variable names for stats
matched_cost_12m <- matched_cost_12m %>% mutate(apc_cost = apc_costs_inputed)

# Goal: analysing long COVID exposure and the APC cost outcomes
# Model: two-part model 

# Crude model data management: -----
# exclude rows with NA in the model: 
crude_vars <- c("apc_cost", "exposure", "follow_up")


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

crude_binomial_apc <- tidy_binomial_fn(crude_bi_12m) %>% mutate(time = "12 months") %>% 
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

crude_gamma_glm_apc <- tidy_gamma_glm_fn(crude_gamma_12m) %>% mutate(time = "12 months") %>%
      mutate(adjustment = "Crude") %>% 
      arrange(desc(term =="exposureLong covid exposure")) 

# Adjusted models:

# Data management: keep complete data
adj_apc_cost_complete_12m <- matched_cost_12m[complete.cases(matched_cost_12m),] %>% 
      mutate(cost_binary = ifelse(apc_cost>0, 1, 0))

# Adjusted binomial model (First part) ----
# Adjusted binomial function:
adj_bi_fn <- function(dataset){
      glm(cost_binary ~ exposure + offset(log(follow_up))+ age + sex  + cov_covid_vax_n_cat + 
                cov_asthma + cov_mental_health + 
                bmi_cat + imd_q5 + ethnicity_6 + region + number_comorbidities_cat,
          data = dataset,
          family = binomial(link="logit"))
}

# run model:
adj_bi_12m <- adj_bi_fn(adj_apc_cost_complete_12m)

# Organise binomial outputs 
adj_binomial_apc <- tidy_binomial_fn(adj_bi_12m) %>% mutate(time = "12 months") %>% 
      mutate(adjustment = "Adjusted")%>% 
      arrange(desc(term =="exposureLong covid exposure")) 

# adjusted Gamma GLM model (Second part) ----
# Adjusted Gamma glm function
adj_gamma_fn <- function(dataset){
      glm(apc_cost ~ exposure + offset(log(follow_up))+ age + sex  + cov_covid_vax_n_cat + 
                cov_asthma + cov_mental_health + 
            bmi_cat + imd_q5 + ethnicity_6 + region + number_comorbidities_cat,
          data = subset(dataset, cost_binary>0),
          family = Gamma(link="log")) 
}

adj_gamma_12m <- adj_gamma_fn(adj_apc_cost_complete_12m)


# Organise the output:
adj_gamma_glm_apc <- tidy_gamma_glm_fn(adj_gamma_12m) %>% mutate(time = "12 months") %>% 
      mutate(adjustment = "Adjusted")%>% 
      arrange(desc(term =="exposureLong covid exposure")) 



# save the output:

# # Binomial part
bind_rows(crude_binomial_apc, adj_binomial_apc) %>% 
      write_csv(here("output", "st03_05_v2_now_inputed_apc_cost_binomial_output.csv"))
# Gamma glm part 
bind_rows(crude_gamma_glm_apc, adj_gamma_glm_apc) %>% 
      write_csv(here("output", "st03_05_v2_now_inputed_apc_cost_twopm_output.csv"))


# Save the detailed outputs to a text file:
sink(here("output", "st03_05_v2_now_inputed_apc_cost_reg_summary.txt"))
print("# Crude binomial model output part 1 ---------")
print(summary(crude_binomial_apc))
print("# Crude hurdle model output part 2 ---------")
print(summary(crude_gamma_glm_apc))
print("# Adjusted binomial model output part 1 ---------")
print(summary(adj_binomial_apc))
print("# Adjusted hurdle model output part 2 ---------")
print(summary(adj_gamma_glm_apc))
sink()




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
crude_apc_costs <- predict_avg_apc_cost_fn(dataset = matched_cost_12m, 
                             fu_time = 30*12,
                             first_reg = crude_bi_12m, 
                             sec_reg = crude_gamma_12m) %>% 
      mutate(time="12 months") %>% 
      mutate(adjustment = "Crude") %>% relocate(adjustment)

# Adjusted costs: ----
# combine outputs
adj_apc_costs <- predict_avg_apc_cost_fn(dataset = matched_cost_12m, 
                             fu_time = 30*12,
                             first_reg = adj_bi_12m, 
                             sec_reg = adj_gamma_12m) %>% 
      mutate(time="12 months") %>% 
      mutate(adjustment = "Adjusted") %>% relocate(adjustment)


total_apc_costs <- bind_rows(crude_apc_costs, adj_apc_costs) 

total_apc_costs %>% write_csv(here("output","st03_05_v2_inputed_predict_apc_cost_tpm.csv"))



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
                  mean_cost = mean(apc_cost),
                  min_cost = min(apc_cost),
                  max_cost = max(apc_cost),
                  n = n(),
                  demonimator = sum(follow_up))
}


# Summarise binomial model data:
bind_rows(bi_model_count_fn(crude_apc_cost_complete_12m) %>% mutate(time = "12m") %>% 
            mutate(model = "Crude"),
          bi_model_count_fn(adj_apc_cost_complete_12m) %>% mutate(time = "12m") %>% 
            mutate(model = "Adjusted")) %>% 
      write_csv("output/st03_05_inputed_apc_binomial_model_counts.csv")


bind_rows(gamma_model_count_fn(crude_apc_cost_complete_12m) %>% mutate(time = "12m") %>% 
            mutate(model = "Crude"),
          gamma_model_count_fn(adj_apc_cost_complete_12m) %>% mutate(time = "12m") %>% 
            mutate(model = "Adjusted")) %>% 
      write_csv("output/st03_05_inputed_apc_gamma_model_counts.csv")

