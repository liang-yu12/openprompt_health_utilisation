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

# First part: adjusted binomial model with interaction term with time
adj_binomial_12m <-  glm(visits_binary ~ exposure*time + offset(log(follow_up)) +
                                 sex + age + bmi_cat +  region + imd_q5 + number_comorbidities_cat, 
                           data = adj_did_tpm_12m,
                           family=binomial(link="logit")) 

# Second part: adjusted positive negative binomial (truncated) with interaction term time
adj_nb_12m <- vglm(visits ~ exposure*time  + offset(log(follow_up)) +
                           sex + age + bmi_cat +  region + imd_q5 + number_comorbidities_cat,
                     family = posnegbinomial(),
                     data = subset(adj_did_tpm_12m, visits_binary > 0))




adj_predic_fn <- function(i.exp, i.time, reg_1st, reg_2nd){

      # sort the age issue:  
      mean_age <- adj_did_tpm_12m %>% group_by(exposure) %>% 
            summarise(mean_age= mean(age, na.rm = T))
      
      # # set up input new data frame
      input <- expand.grid(exposure = i.exp, 
                           time = i.time,
                           follow_up = 360,
                           sex = c("female", "male"),
                           age =  mean_age %>% filter(exposure == i.exp) %>% .$mean_age,
                           # ethnicity_6 = c("White","Mixed","South Asian", "Black","Other","Not stated"),
                           bmi_cat = c("Underweight","Normal Weight", "Overweight", "Obese"),
                           region = c("East", "East Midlands", "London", "North East","North West",
                                      "South East","South West","West Midlands", "Yorkshire and The Humber"),
                           imd_q5 = c("least_deprived", "2_deprived", "3_deprived","4_deprived","most_deprived"),
                           number_comorbidities_cat = c("0", "1", "2", "3")
      )
      # Prediction:
      p1 <- predict(reg_1st, newdata = input, type = "response")
      p2 <- predictvglm(reg_2nd, newdata = input, type = "link", se.fit = T)
      
      results <- data.frame(
            exposure = i.exp,
            time = i.time,
            nonzero_prob = p1,
            p2_visits = exp(p2$fitted.values),
            p2_lci = exp(p2$fitted.values - 1.96*p2$se.fit),
            p2_lci = exp(p2$fitted.values + 1.96*p2$se.fit)) %>% 
            mutate(visits = p2_visits*nonzero_prob,
                   lci = p2_lci*nonzero_prob,
                   hci = p2_hci*nonzero_prob) 
      
      
      
      return(results)
}


predicted_adj_value <- bind_rows(
      adj_predic_fn(i.exp = "Comparator", i.time = "Historical", reg_1st = adj_binomial_12m, reg_2nd = adj_nb_12m),
      adj_predic_fn(i.exp = "Long COVID exposure", i.time = "Historical", reg_1st = adj_binomial_12m, reg_2nd = adj_nb_12m),
      adj_predic_fn(i.exp = "Comparator", i.time = "Contemporary", reg_1st = adj_binomial_12m, reg_2nd = adj_nb_12m),
      adj_predic_fn(i.exp = "Long COVID exposure", i.time = "Contemporary", reg_1st = adj_binomial_12m, reg_2nd = adj_nb_12m)
)

adj_did_tpm_12m$ethnicity_6 %>% table





# set up functions:
adj_predic_fn <- function(i.exp, i.time){
      # # set up input new data frame
      input <- expand.grid(exposure = i.exp, 
                          time = i.time,
                          follow_up = 360,
                          sex = c("female", "male"),
                          age_cat = c("18-29", "30-39", "40-49", "50-59","60-69", "70+"),
                          ethnicity_6 = c("White","Mixed","South Asian", "Black","Other","Not stated"),
                          bmi_cat = c("Underweight","Normal Weight", "Overweight", "Obese"),
                          region = c("East", "East Midlands", "London", "North East","North West",
                                     "South East","South West","West Midlands", "Yorkshire and The Humber"),
                          imd_q5 = c("least_deprived", "2_deprived", "3_deprived","4_deprived","most_deprived"),
                          number_comorbidities_cat = c("0", "1", "2", "3")
                          )
      # Predict model
      predicted <- predict(did_crude_nb, 
                           newdata = input,
                           se.fit = T, type = "response") %>% 
            as.data.frame() %>% 
            mutate(exposure = i.exp) %>% 
            mutate(time = i.time) %>% relocate(exposure, time)
      return(predicted)
}


# combine outputs: 
predicted_adj_value <- bind_rows(
      adj_predic_fn(i.exp = "Comparator", i.time = "Historical"),
      adj_predic_fn(i.exp = "Long COVID exposure", i.time = "Historical"),
      adj_predic_fn(i.exp = "Comparator", i.time = "Contemporary"),
      adj_predic_fn(i.exp = "Long COVID exposure", i.time = "Contemporary")
) %>% rename(visits = fit, se=se.fit)

predicted_adj_value$time <- as.factor(predicted_adj_value$time)
levels(predicted_adj_value$time) <- c("Historical", "Contemporary")
predicted_adj_value$exposure <- predicted_adj_value$exposure %>% as.factor()
predicted_adj_value$residual.scale <- NULL

# calculate ci:
predicted_adj_value <- predicted_adj_value %>% 
      mutate(hci=visits+1.96*se) %>% 
      mutate(lci=visits-1.96*se)



# visualised the data
png(file=here("output", "st05_did_adj.png"), width=1200, height=600)

ggplot(predicted_adj_value, aes(x= time, y= visits, color = exposure)) +
      geom_point() + geom_errorbar(aes(ymin=lci, ymax=hci), width=0.2) +
      geom_line(aes(group = exposure)) 

dev.off()


# save the output table
bind_rows(
      (predicted_crude_value %>% mutate(model = "crude") %>% relocate(model)),
      (predicted_adj_value %>% mutate(model="adjusted") %>% relocate(model))
) %>% write_csv(here("output", "st05_did_reg.csv"))
