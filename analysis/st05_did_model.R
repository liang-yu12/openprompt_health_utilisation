# Run difference in difference model:

# Load previous data management
source("analysis/dm02_04_combine_long_data_for_did.R")

crude_distribution <- did_data_12m %>% group_by(exposure, time) %>% 
      summarise(mean_visits = mean(visits))


# Curde negative bionomial model -----
did_crude_nb <- glm.nb(visits ~ exposure*time + offset(log(follow_up)),
                   data = did_data_12m) 

did_crude_nb %>% summary
# Predict crude model: setup function for outputs: ----

crude_predic_fn <- function(i.exp, i.time){
      # # set up input new data frame
      input <- data.frame(exposure = i.exp, 
                          time = i.time,
                          follow_up = 360)
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
predicted_crude_value <- bind_rows(
      crude_predic_fn(i.exp = "Comparator", i.time = "Historical"),
      crude_predic_fn(i.exp = "Long COVID exposure", i.time = "Historical"),
      crude_predic_fn(i.exp = "Comparator", i.time = "Contemporary"),
      crude_predic_fn(i.exp = "Long COVID exposure", i.time = "Contemporary")
) %>% rename(visits = fit, se=se.fit)

predicted_crude_value$time <- as.factor(predicted_crude_value$time)
levels(predicted_crude_value$time) <- c("Historical", "Contemporary")
predicted_crude_value$exposure <- predicted_crude_value$exposure %>% as.factor()
predicted_crude_value$residual.scale <- NULL

# calculate ci:
predicted_crude_value <- predicted_crude_value %>% 
      mutate(hci=visits+1.96*se) %>% 
      mutate(lci=visits-1.96*se)


# visualised the data
png(file=here("output", "st05_did_crude.png"), width=1200, height=600)

ggplot(predicted_crude_value, aes(x= time,
                                  y= visits,
                                  color = exposure)) +
      geom_point() + geom_errorbar(aes(ymin=lci, ymax=hci), width=0.2) +
      geom_line(aes(group = exposure)) 

dev.off()


# Adjusted negative binomial model ------
did_adj_nb <- glm.nb(visits ~ exposure*time + offset(log(follow_up)) +
                           sex + age_cat + ethnicity_6 + bmi_cat + region + imd_q5 + number_comorbidities_cat,
                     data = did_data_12m) 


# set up functions:
adj_predic_fn <- function(i.exp, i.time){
      # # set up input new data frame
      input <- data.frame(exposure = i.exp, 
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
