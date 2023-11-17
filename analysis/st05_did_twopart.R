# Run difference in difference model:

# Load previous data management
source("analysis/dm05_04_hx_combine_long_data_for_did.R")
did_data_12m %>% names()

# Create binary visits in two time periods:
did_tpm_12m <- did_data_12m %>% group_by(time) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0)) %>% 
      ungroup()
      
# Crude complete data: keep complete cases
crude_did_tpm_12 <- did_tpm_12m %>% dplyr::select(visits ,visits_binary, exposure, time, follow_up)
crude_did_tpm_12 <- crude_did_tpm_12[complete.cases(crude_did_tpm_12),]
      
      
# First part: Binomial model with interaction term with time
crude_binomial_12m <-  glm(visits_binary ~ exposure*time + offset(log(follow_up)), 
                           data = crude_did_tpm_12,
                           family=binomial(link="logit")) 

# Positive negative binomial (truncated) with interaction term time
crude_nb_12m <- vglm(visits ~ exposure*time  + offset(log(follow_up)),
                     family = posnegbinomial(),
                     data = subset(crude_did_tpm_12, visits_binary > 0))

# Function to tidy vglm output:
tidy.vglm <- function(x, conf.int=FALSE, conf.level=0.95) {
      co <- as.data.frame(coef(summary(x)))
      names(co) <- c("estimate","std.error","statistic","p.value")
      if (conf.int) {
            qq <- qnorm((1+conf.level)/2)
            co <- transform(co,
                            conf.low=estimate-qq*std.error,
                            conf.high=estimate+qq*std.error)
      }
      co <- data.frame(term=rownames(co),co)
      rownames(co) <- NULL
      return(co)
}


# Predict crude model: setup function for outputs: ----

crude_predic_fn <- function(i.exp, i.time){
      # # set up input new data frame
      input <- crude_did_tpm_12 %>% filter(exposure == i.exp, time == i.time) %>% 
            mutate(follow_up = 360)
      
      # Prediction:
      p1 <- predict(crude_binomial_12m, newdata = input, type = "response")           
      
      p2 <- predictvglm(crude_nb_12m, newdata = input, type = "link", se.fit = T) 
      
      p2_fit<- p2$fitted.values %>% as.data.frame() %>% 
            dplyr::select(`loglink(munb)`) %>% rename(fitted = `loglink(munb)`) 
      
      p2_se <- p2$se.fit %>% as.data.frame()%>% 
            dplyr::select(`loglink(munb)`)  %>% rename(se = `loglink(munb)`)
      
      
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

# combine outputs: 
predicted_crude_value <- bind_rows(
      crude_predic_fn(i.exp = "Comparator", i.time = "Historical"),
      crude_predic_fn(i.exp = "Long COVID exposure", i.time = "Historical"),
      crude_predic_fn(i.exp = "Comparator", i.time = "Contemporary"),
      crude_predic_fn(i.exp = "Long COVID exposure", i.time = "Contemporary")
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
      geom_point() + geom_errorbar(aes(ymin=lci, ymax=hci), width=0.5) +
      geom_line(aes(group = exposure), size = 1)  + theme_bw() +
      xlab("Time period") + ylab("Average healthcare visits") +
      scale_color_manual(values=c("#E1BE6A", "#40B0A6")) +
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
                               sex + age + bmi_cat + ethnicity_6 + region + imd_q5 + 
                               cov_asthma + cov_mental_health + number_comorbidities_cat, 
                           data = adj_did_tpm_12m,
                           family=binomial(link="logit")) 

# Second part: adjusted positive negative binomial (truncated) with interaction term time
adj_nb_12m <- vglm(visits ~ exposure*time  + offset(log(follow_up)) +
                         sex + age + bmi_cat + ethnicity_6 + region + imd_q5 + 
                         cov_asthma + cov_mental_health + number_comorbidities_cat,
                     family = posnegbinomial(),
                     data = subset(adj_did_tpm_12m, visits_binary > 0))

adj_predic_fn <- function(i.exp, i.time){

  # # set up input new data frame
  input <- adj_did_tpm_12m %>% filter(exposure == i.exp, time == i.time) %>% 
        mutate(follow_up = 360)
  
  p1 <- predict(adj_binomial_12m, newdata = input, type = "response")                     
  p2 <- predict(adj_nb_12m, newdata = input, type = "link", se.fit = T)
  
  
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
      adj_predic_fn(i.exp = "Comparator", i.time = "Historical"),
      adj_predic_fn(i.exp = "Long COVID exposure", i.time = "Historical"),
      adj_predic_fn(i.exp = "Comparator", i.time = "Contemporary"),
      adj_predic_fn(i.exp = "Long COVID exposure", i.time = "Contemporary")
)


predicted_adj_value$exposure <- factor(predicted_adj_value$exposure, levels = exposure_order)
predicted_adj_value$time <- factor(predicted_adj_value$time, levels = time_order)

# Visualize the results

ggplot(predicted_adj_value, aes(x= time, y= visits, color = exposure)) +
      geom_point() + geom_errorbar(aes(ymin=lci, ymax=hci), width=0.1) +
      geom_line(aes(group = exposure), size = 1)  + theme_bw() +
      xlab("Time period") + ylab("Average healthcare visits") +
      scale_color_manual(values=c("#E1BE6A", "#40B0A6")) +
      guides(color=guide_legend(title="Exposure group")) 

ggsave("output/st05_did_adj.png", width = 9, height = 4, units = "in")


# save the output table
bind_rows(
      (predicted_crude_value %>% mutate(model = "crude") %>% relocate(model)),
      (predicted_adj_value %>% mutate(model="adjusted") %>% relocate(model))
) %>% write_csv(here("output", "st05_did_tpm_predicted.csv"))


# Save the regression outputs:


sink(here("output", "st05_did_reg_summary_output.txt"))
print("# Crude binomial model output part 1 ---------")
print(crude_binomial_12m %>% tidy(exponentiate = T))
print("# Crude hurdle model output part 2 ---------")
print(tidy.vglm(crude_nb_12m, conf.int=T))
print("# Adjusted binomial model output part 1 ---------")
print(adj_binomial_12m %>% tidy(exponentiate = T), n = 28)
print("# Adjusted hurdle model output part 2 ---------")
print(tidy.vglm(adj_nb_12m, conf.int=T))
sink()

