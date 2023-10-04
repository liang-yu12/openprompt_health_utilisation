# Run difference in difference model:

# Load previous data management
source("analysis/dm02_04_combine_long_data_for_did.R")
did_data_12m %>% names()

# Create binary visits in two time periods:
did_tpm_12m <- did_data_12m %>% group_by(time) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0)) %>% 
      ungroup()
      

# First part: Binomial model with interaction term with time
crude_binomial_12m <-  glm(visits_binary ~ exposure*time + offset(log(follow_up)), 
                           data = did_tpm_12m,
                           family=binomial(link="logit")) 

# Positive negative binomial (truncated) with interaction term time
crude_nb_12m <- vglm(visits ~ exposure*time  + offset(log(follow_up)),
                     family = posnegbinomial(),
                     data = subset(did_tpm_12m, visits_binary > 0))


# Predict crude model: setup function for outputs: ----

crude_predic_fn <- function(i.exp, i.time, reg_1st, reg_2nd){
      # # set up input new data frame
      input <- expand.grid(exposure = i.exp, 
                          time = i.time,
                          follow_up = 360)
      # Prediction:
      results <- data.frame(
            exposure = i.exp,
            time = i.time,
            nonzero_prob = predict(reg_1st, newdata = input, type = "response"))  # first part 
      
      p2 <- predictvglm(reg_2nd, newdata = input, type = "link", se.fit = T) 
      
      results <- results %>% mutate(
            visits = mean(exp(p2$fitted.values), na.rm = T)*nonzero_prob,
            lci   = mean(exp(p2$fitted.values - 1.96*p2$se.fit))*nonzero_prob,
            hci   = mean(exp(p2$fitted.values + 1.96*p2$se.fit))*nonzero_prob,
      )
      results$nonzero_prob <- NULL
      
      return(results)
}

# combine outputs: 
predicted_crude_value <- bind_rows(
      crude_predic_fn(i.exp = "Comparator", i.time = "Historical", reg_1st = crude_binomial_12m, reg_2nd = crude_nb_12m),
      crude_predic_fn(i.exp = "Long COVID exposure", i.time = "Historical", reg_1st = crude_binomial_12m, reg_2nd = crude_nb_12m),
      crude_predic_fn(i.exp = "Comparator", i.time = "Contemporary", reg_1st = crude_binomial_12m, reg_2nd = crude_nb_12m),
      crude_predic_fn(i.exp = "Long COVID exposure", i.time = "Contemporary", reg_1st = crude_binomial_12m, reg_2nd = crude_nb_12m)
)

# Visualize the results

# Order the outputs
exposure_order <- c("Comparator", "Long COVID exposure")
time_order <- c("Historical", "Contemporary")

predicted_crude_value$exposure <- factor(predicted_crude_value$exposure, levels = exposure_order)
predicted_crude_value$time <- factor(predicted_crude_value$time, levels = time_order)


ggplot(predicted_crude_value, aes(x= time,
                                  y= visits,
                                  color = exposure)) +
      geom_point() + geom_errorbar(aes(ymin=lci, ymax=hci), width=0.2) +
      geom_line(aes(group = exposure))  + theme_bw() +
      xlab("Time period") + ylab("Average healthcare visits") +
      guides(color=guide_legend(title="Exposure group")) 

ggsave("output/st05_did_crude.png", width = 9, height = 4, units = "in")


# Adjusted two-part model -------

# data management: select complete cases:
adj_did_tpm_12m <- did_data_12m[complete.cases(did_data_12m),] %>% group_by(time) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0)) %>% 
      ungroup()

adj_did_tpm_12m$ethnicity_6 <- droplevels(adj_did_tpm_12m$ethnicity_6)

# First part: adjusted binomial model with interaction term with time
adj_binomial_12m <-  glm(visits_binary ~ exposure*time + offset(log(follow_up)) +
                                 sex + age + bmi_cat + ethnicity_6 + region + imd_q5 + number_comorbidities_cat, 
                           data = adj_did_tpm_12m,
                           family=binomial(link="logit")) 

# Second part: adjusted positive negative binomial (truncated) with interaction term time
adj_nb_12m <- vglm(visits ~ exposure*time  + offset(log(follow_up)) +
                           sex + age + bmi_cat + ethnicity_6 + region + imd_q5 + number_comorbidities_cat,
                     family = posnegbinomial(),
                     data = subset(adj_did_tpm_12m, visits_binary > 0))

adj_predic_fn <- function(i.exp, i.time, reg_1st, reg_2nd){
  mean_age <- adj_did_tpm_12m %>% group_by(exposure) %>% 
    summarise(mean_age= mean(age, na.rm = T))
  
  # # set up input new data frame
  input <- expand.grid(exposure = i.exp, 
                       time = i.time, 
                       follow_up = 360,
                       sex = c("female", "male"),
                       age =  mean_age %>% filter(exposure == i.exp) %>% .$mean_age,
                       ethnicity_6 = c("White","Mixed","South Asian", "Black","Other"),
                       bmi_cat = c("Underweight","Normal Weight", "Overweight", "Obese"),
                       region = c("East", "East Midlands", "London", "North East","North West",
                                  "South East","South West","West Midlands", "Yorkshire and The Humber"),
                       imd_q5 = c("least_deprived", "2_deprived", "3_deprived","4_deprived","most_deprived"),
                       number_comorbidities_cat = c("0", "1", "2", "3"))
  
  p1 <- predict(reg_1st, newdata = input, type = "response")                     
  p2 <- predict(reg_2nd, newdata = input, type = "link", se.fit = T)
  
  
  # the fitted value outcome is a matrix. Only need the mean value
  p2_fit<- p2$fitted.values %>% as.data.frame() %>% 
    dplyr::select(`loglink(munb)`) %>% rename(fitted = `loglink(munb)`) 
  
  p2_se <- p2$se.fit %>% as.data.frame()%>% 
    dplyr::select(`loglink(munb)`)  %>% rename(se = `loglink(munb)`)
  
  # multiply the first part and the second part:
  predicted_visits <- data.frame(
    nonzero_prob = p1,
    p_visits = p2_fit$fitted,
    p_se= p2_se$se) %>% 
    mutate(c_visits = exp(p_visits)*nonzero_prob,
           c_lci = exp(p_visits - 1.96*p_se)*nonzero_prob,
           c_hci = exp(p_visits + 1.96*p_se)*nonzero_prob) %>% 
    dplyr::select(c_visits, c_lci, c_hci)
  
  # summarise the results
  results <- predicted_visits %>% summarise(
    visits = mean(c_visits, na.rm =T),
    lci = mean(c_lci, na.rm =T),
    hci = mean(c_hci, na.rm = T)) %>% 
    mutate(exposure = i.exp, time = i.time) %>% 
    relocate(exposure, time)
  
  return(results)
}


predicted_adj_value <- bind_rows(
      adj_predic_fn(i.exp = "Comparator", i.time = "Historical", reg_1st = adj_binomial_12m, reg_2nd = adj_nb_12m),
      adj_predic_fn(i.exp = "Long COVID exposure", i.time = "Historical", reg_1st = adj_binomial_12m, reg_2nd = adj_nb_12m),
      adj_predic_fn(i.exp = "Comparator", i.time = "Contemporary", reg_1st = adj_binomial_12m, reg_2nd = adj_nb_12m),
      adj_predic_fn(i.exp = "Long COVID exposure", i.time = "Contemporary", reg_1st = adj_binomial_12m, reg_2nd = adj_nb_12m)
)


predicted_adj_value$exposure <- factor(predicted_adj_value$exposure, levels = exposure_order)
predicted_adj_value$time <- factor(predicted_adj_value$time, levels = time_order)

# Visualize the results

ggplot(predicted_adj_value, aes(x= time, y= visits, color = exposure)) +
      geom_point() + geom_errorbar(aes(ymin=lci, ymax=hci), width=0.2) +
      geom_line(aes(group = exposure))  + theme_bw() +
  xlab("Time period") + ylab("Average healthcare visits") +
  guides(color=guide_legend(title="Exposure group")) 

ggsave("output/st05_did_adj.png", width = 9, height = 4, units = "in")


# save the output table
bind_rows(
      (predicted_crude_value %>% mutate(model = "crude") %>% relocate(model)),
      (predicted_adj_value %>% mutate(model="adjusted") %>% relocate(model))
) %>% write_csv(here("output", "st05_did_tpm_predicted.csv"))
