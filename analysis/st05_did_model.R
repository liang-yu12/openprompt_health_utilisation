# Run difference in difference model:

# Load previous data management
source("analysis/dm02_04_combine_long_data_for_did.R")

# Curde negative bionomial model -----
did_crude_nb <- glm.nb(visits ~ exposure*time + offset(log(follow_up)),
                   data = did_data_12m) 

did_crude_nb %>% summary

# Predicting the # historical comparator: -----
# # obtain average follow-up time
hx_com_subset <- did_data_12m %>% 
      filter(exposure == "Comparator" & time == "Historical") %>% 
      dplyr::select(follow_up)

# # set up input new data frame
hx_com_input <- data.frame(exposure = "Comparator", 
                     time = "Historical",
                     follow_up = mean(hx_com_subset$follow_up))
# Predict model
crude_predict_com_hx <- predict(did_crude_nb, 
                              newdata = hx_com_input,
                              se.fit = T, type = "response") %>% 
      as.data.frame()

# Predicting the contemporary comparator: -----
# # obtain average follow-up time
now_com_subset <- did_data_12m %>% 
      filter(exposure == "Comparator" & time == "Contemporary") %>% 
      dplyr::select(follow_up)

# # set up input new data frame
now_com_input <- data.frame(exposure = "Comparator", 
                     time = "Contemporary",
                     follow_up = mean(now_com_subset$follow_up))
# Predict model
crude_predict_com_now <- predict(did_crude_nb, 
                                newdata = now_com_input,
                                se.fit = T, type = "response") %>% 
      as.data.frame()

# Predicting the historical exposure: -----
hx_exp_subset <- did_data_12m %>% 
      filter(exposure == "Long COVID exposure" & time == "Historical") %>% 
      dplyr::select(follow_up)

# # set up input new data frame
hx_exp_input <- data.frame(exposure = "Long COVID exposure", 
                     time = "Historical",
                     follow_up = mean(hx_exp_subset$follow_up))
# Predict model
crude_predict_exp_hx <- predict(did_crude_nb, 
                                newdata = hx_exp_input,
                                se.fit = T, type = "response") %>% 
      as.data.frame()

# Predicting the contemporary exposure -----
# # obtain average follow-up time
now_exp_subset <- did_data_12m %>% 
      filter(exposure == "Long COVID exposure" & time == "Contemporary") %>% 
      dplyr::select(follow_up)

# # set up input new data frame
now_com_input <- data.frame(exposure = "Long COVID exposure", 
                            time = "Contemporary",
                            follow_up = mean(now_exp_subset$follow_up))
# Predict model
crude_predict_exp_now <- predict(did_crude_nb, 
                                 newdata = now_com_input,
                                 se.fit = T, type = "response") %>% 
      as.data.frame()




did_data_12m$exposure %>% table









ggplot(did_12m_crude, aes(x= time, y= crude_predict,
                          color = exposure)) +
      geom_point() +
      geom_line(aes(group = exposure)) 


# Adjusted negative binomial model
did_adj_nb <- glm.nb(visits ~ exposure*time + offset(log(follow_up)) +
                           sex + age_cat + ethnicity_6 + bmi_cat + region + imd_q5 + number_comorbidities_cat,
                     data = did_data_12m) 


