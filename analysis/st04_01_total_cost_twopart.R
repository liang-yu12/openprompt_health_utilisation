source("analysis/dm03_03_now_pivot_total_long.R")

# Goal: analysing long COVID exposure and the cost outcomes
# Model: two-part model 


# Stats: Two-part model: -----
# Data management: exclude rows with NA in the model: 
crude_vars <- c("total_cost", "exposure", "follow_up")

crude_cost_complete_12m <- matched_cost_12m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(cost_binary = ifelse(total_cost>0, 1, 0)) 

# Crude Two-part model: -----
# First part: binomial model 

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

crude_binary <- binary_output_fn(curde_binomial_12m) %>% 
      mutate(time = "12m") %>% 
      mutate(model = "Crude")



# Gamma glm part: 
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

crude_gamma <-gamma_output_fn(crude_gamma_12m) %>% mutate(time = "12m") %>% mutate(model = "Crude")


# Adjusted two-part model: ----------
# Data management: keep complete data
adj_cost_complete_12m <- matched_cost_12m[complete.cases(matched_cost_12m),] %>% 
  mutate(cost_binary = ifelse(total_cost>0, 1, 0))


# First part: binomial model:
adj_binomial_12m <- glm(cost_binary ~ exposure + offset(log(follow_up))+ age + sex  + cov_covid_vax_n_cat + 
                              bmi_cat + imd_q5 + ethnicity_6 + region + cov_asthma + cov_mental_health +
                              number_comorbidities_cat,
                       data = adj_cost_complete_12m,
                       family = binomial(link="logit"))

adj_binary <- binary_output_fn(adj_binomial_12m) %>% 
      mutate(time = "12m") %>% mutate(model = "Adjusted")


# Second part: Gamma GLM
adj_gamma_12m <-  glm(total_cost ~ exposure + offset(log(follow_up))+ age + sex  + cov_covid_vax_n_cat + 
                            bmi_cat + imd_q5 + ethnicity_6 + region + cov_asthma + cov_mental_health +
                            number_comorbidities_cat,
                     data = subset(adj_cost_complete_12m, cost_binary>0),
                     family = Gamma(link="log"))


adj_gamma <-gamma_output_fn(adj_gamma_12m) %>% 
      mutate(time = "12m") %>% mutate(model = "Adjusted")


# Save the detailed outputs to a text file:
sink(here("output", "st03_01_total_reg_summary.txt"))
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

all_binary %>% write_csv(here("output", "st03_01_total_cost_binary.csv"))
all_gamma%>% write_csv(here("output", "st03_01_total_cost_gammaglm.csv"))

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
            summarise(costs = mean(c_cost) # summarised the results by exposure: 
            )
      
      return(results)
}

adj_predict_cost_fn(dataset = adj_cost_complete_12m,
                    fu_time = 30*12,
                    reg_1st = adj_binomial_12m,
                    reg_2nd = adj_gamma_12m) %>% 
      write_csv(here("output", "st03_01_total_cost_predicted_costs.csv"))




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
      (bi_model_count_fn(crude_cost_complete_12m) %>% 
             mutate(time = "12m") %>% 
             mutate(model = "Crude")),
      (bi_model_count_fn(adj_cost_complete_12m) %>% 
            mutate(time = "12m") %>% 
            mutate(model = "Adjusted"))) %>% 
      write_csv("output/st03_01_total_cost_binomial_model_counts.csv")


bind_rows(
      gamma_model_count_fn(crude_cost_complete_12m) %>% 
            mutate(time = "12m") %>% 
            mutate(model = "Crude"),
      gamma_model_count_fn(adj_cost_complete_12m) %>% 
            mutate(time = "12m") %>% 
            mutate(model = "Adjusted")) %>% 
      write_csv("output/st03_01_total_cost_gamma_model_counts.csv")
