# Load previous data management
source("analysis/dm02_01_now_pivot_total_visits_long.R")

# Goal: run the gubgroup analyses by 
# - sex: "female" "male"  
# - age groups: "18-29" "30-39" "40-49" "50-59" "60-69" "70+"  
# - previous hospitalisation:"FALSE" "TRUE" 

# outcome types:12m,
# model:hurdle model 

# Data management for modeling:: --------

# Keep complete cases
adj_complete_12m <- matched_data_12m[complete.cases(matched_data_12m),] %>% 
  mutate(visits_binary = ifelse(visits>0, 1, 0))


# Functions for organising outcomes: 


# Tidy binomial model:
binomial_tidy_fn <- function(bi_reg){
      bi_results <- bi_reg %>% tidy() %>% mutate(
            model = "binomial",
            lci = exp(estimate - 1.96*std.error),
            hci = exp(estimate + 1.96*std.error),
            estimate = exp(estimate)) %>% 
            dplyr::select(model, term, estimate, lci, hci, p.value)%>% 
            filter(term == "exposureLong covid exposure" )      
      return(bi_results)
}


# Tidy up second part outcome
positive_nb_tidy_fu <- function(vg_reg){
      
      t1 <- vg_reg %>%summary
      t2 <- t1@coef3 %>% as.data.frame()
      t2$term <- rownames(t2)
      t3 <- t2 %>% filter(term == "exposureLong covid exposure" )
      results <- t3 %>% mutate(
            lci = exp(Estimate - 1.96*`Std. Error`),
            hci = exp(Estimate + 1.96*`Std. Error`),
            estimate = exp(Estimate),
            p.value = `Pr(>|z|)`,
            model = "Positive Negative Bionomial") %>% 
            dplyr::select(model, term, estimate, lci, hci, p.value)
      return(results)
}

# Predict the outcomes by different group and stratum
adj_predic_fn <- function(factor, value, part_1, part_2){
      
      input <- adj_complete_12m %>% 
            filter(factor == value) %>% 
            mutate(follow_up = 360)
      
      input$nonzero_prob <- predict(part_1, newdata = input, type = "response")                     
      
      
      p2 <- predict(part_2, newdata = input, type = "link", se.fit = T)
      
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
            mutate(c_visits = exp(p_visits)*nonzero_prob,
                   c_lci = exp(p_visits - 1.96*p_se)*nonzero_prob,
                   c_hci = exp(p_visits + 1.96*p_se)*nonzero_prob) %>% 
            dplyr::select(exposure, c_visits, c_lci, c_hci)
      
      # summarise the results
      results <- input_c %>% 
            group_by(exposure) %>% 
            summarise(
            visits = mean(c_visits, na.rm =T),
            lci = mean(c_lci, na.rm =T),
            hci = mean(c_hci, na.rm = T)) %>% 
            mutate(stratum = value) %>% relocate(stratum)
      return(results)
}

# LR test for hurdle moderl: Write a function to organise the vglm outcomes
lrt_hurdle <- function(interaction, no_interaction){
      compare_hurdle_lrt <- VGAM::lrtest_vglm(interaction, no_interaction)
      results <- compare_hurdle_lrt@Body %>% as.data.frame() %>% dplyr::select(`Pr(>Chisq)`)
      return(results)
}
# Stats: Subgroup analyses by different covariates -----
# Fit an interaction term between exposure and the cov.

# # By previous hospitalisation ----

adj_complete_12m$previous_covid_hosp %>% levels()  # "FALSE" "TRUE" 
# create a new var using TRUE as baseline for interaction
adj_complete_12m$previous_covid_hosp_true <- factor(adj_complete_12m$previous_covid_hosp, 
                                                    levels = c("TRUE", "FALSE"))

# first-part: Binomial model

# No interaction:
hos_no_binomial_12m <- glm(visits_binary ~ exposure + previous_covid_hosp + offset(log(follow_up)) +
                            age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                            cov_covid_vax_n_cat +number_comorbidities_cat, 
                          data = adj_complete_12m,
                          family=binomial(link="logit")) 
# False stratum: 
hos_f_binomial_12m <- glm(visits_binary ~ exposure*previous_covid_hosp + offset(log(follow_up)) +
                          age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                          cov_covid_vax_n_cat +number_comorbidities_cat, 
                        data = adj_complete_12m,
                        family=binomial(link="logit")) 

# True stratum:  
hos_t_binomial_12m <- glm(visits_binary ~ exposure*previous_covid_hosp_true + offset(log(follow_up)) +
                            age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                            cov_covid_vax_n_cat +number_comorbidities_cat, 
                          data = adj_complete_12m,
                          family=binomial(link="logit")) 

# Likelihood ratio test 
lrt_hos_bi <- lmtest::lrtest(hos_f_binomial_12m, hos_no_binomial_12m) %>% 
  dplyr::select(`Pr(>Chisq)`) 


# Organise binomial outcomes:
hos_bi_sub <- bind_rows(
  binomial_tidy_fn(hos_f_binomial_12m) %>% mutate(stratum = "No hospital admission"),
  binomial_tidy_fn(hos_t_binomial_12m) %>% mutate(stratum = "Admitted due to COVID")) %>% 
bind_cols(lrt_hos_bi)


# # Second hurdle part: positive negative binomial:


# No interaction: 
hos_no_hurdle_12m <- vglm(visits ~ exposure + previous_covid_hosp + offset(log(follow_up))+
                     age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                     cov_covid_vax_n_cat +number_comorbidities_cat, 
                   family = posnegbinomial(),
                   data = subset(adj_complete_12m, visits_binary > 0))
# No admission stratum:
hos_f_hurdle_12m <- vglm(visits ~ exposure*previous_covid_hosp + offset(log(follow_up))+
                            age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                            cov_covid_vax_n_cat +number_comorbidities_cat, 
                          family = posnegbinomial(),
                          data = subset(adj_complete_12m, visits_binary > 0))
# Admitted stratum
hos_t_hurdle_12m <- vglm(visits ~ exposure*previous_covid_hosp_true + offset(log(follow_up))+
                           age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                           cov_covid_vax_n_cat +number_comorbidities_cat, 
                         family = posnegbinomial(),
                         data = subset(adj_complete_12m, visits_binary > 0))

      
lrt_hos_tpm <- lrt_hurdle(hos_f_hurdle_12m, hos_no_hurdle_12m)      

# Organise hurdle outcomes
hos_hurdle_sub <- bind_rows(
      positive_nb_tidy_fu(hos_f_hurdle_12m) %>% mutate(stratum = "No hospital admission"),
      positive_nb_tidy_fu(hos_t_hurdle_12m) %>% mutate(stratum = "Admitted due to COVID")) %>% 
      bind_cols(lrt_hos_tpm)

# Save the output
bind_rows(hos_bi_sub, hos_hurdle_sub) %>% write_csv(here("output", "st04_04_stratum_hospitalisation.csv"))


# Predict the outcomes: 

predicted_by_hos <- bind_rows(
      adj_predic_fn(factor = adj_complete_12m$previous_covid_hosp, value = "FALSE",
                    part_1 = hos_f_binomial_12m, part_2 = hos_f_hurdle_12m),
      adj_predic_fn(factor = adj_complete_12m$previous_covid_hosp, value = "TRUE",
                    part_1 = hos_t_binomial_12m, part_2 = hos_t_hurdle_12m)) %>% 
      mutate(group = "Previous hospitalisation") %>% relocate(group)

## By sex:-----
# Ref: male
adj_complete_12m$sex %>% levels  #"male"   "female"
adj_complete_12m$sex_m <- adj_complete_12m$sex
adj_complete_12m$sex_m %>% levels # "male"   "female"
# Ref: female
adj_complete_12m$sex_f <- factor(adj_complete_12m$sex, levels = c("female","male"))
adj_complete_12m$sex_f %>% levels # "female" "male"  
# Part 1: Binomial model:

# No interaction:
sex_no_binomial_12m <- glm(visits_binary ~ exposure + sex_m + offset(log(follow_up)) +
                                 age + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                                 previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat,
                              data = adj_complete_12m,
                              family=binomial(link="logit"))

# Female stratum:
sex_f_binomial <- glm(visits_binary ~ exposure*sex_f + offset(log(follow_up)) +
                            age + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                            previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat,
                      data = adj_complete_12m,
                      family=binomial(link="logit"))

# Male stratum:
sex_m_binomial <- glm(visits_binary ~ exposure*sex_m + offset(log(follow_up)) +
                            age + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                            previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat,
                      data = adj_complete_12m,
                      family=binomial(link="logit"))

# LR test:
lrt_sex_bi <- lmtest::lrtest(sex_m_binomial, sex_no_binomial_12m) %>%
      dplyr::select(`Pr(>Chisq)`)

# Organise binomial outcomes:
sex_bi_sub <- bind_rows(
      binomial_tidy_fn(sex_f_binomial) %>% mutate(stratum = "Female"),
      binomial_tidy_fn(sex_m_binomial) %>% mutate(stratum = "Male")) %>%
      bind_cols(lrt_sex_bi)

# Part 2: Truncated negative binomial reg

# No interaction:
sex_no_hurdle_12m <- vglm(visits ~ exposure + sex_m + offset(log(follow_up)) +
                                age + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                                previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat,
                          family = posnegbinomial(),
                          data = subset(adj_complete_12m, visits_binary > 0))

# Female stratum:
sex_f_hurdle_12m <- vglm(visits ~ exposure*sex_f + offset(log(follow_up)) +
                                age + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                                previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat,
                          family = posnegbinomial(),
                          data = subset(adj_complete_12m, visits_binary > 0))

# Male stratum:
sex_m_hurdle_12m <- vglm(visits ~ exposure*sex_m + offset(log(follow_up)) +
                               age + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                               previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat,
                         family = posnegbinomial(),
                         data = subset(adj_complete_12m, visits_binary > 0))

# LR test
sex_hurdle_lrt <- lrt_hurdle(sex_f_hurdle_12m, sex_no_hurdle_12m)

# organise_hurdle_outcomes
sex_hudle_sub <- bind_rows(
      positive_nb_tidy_fu(sex_f_hurdle_12m) %>% mutate(stratum = "Female"),
      positive_nb_tidy_fu(sex_m_hurdle_12m) %>% mutate(stratum = "Male")) %>%
      bind_cols(sex_hurdle_lrt)

# save outputs
bind_rows(sex_bi_sub, sex_hudle_sub) %>% write_csv(here("output", "st04_04_stratum_sex.csv"))


# Predicted the outcomes:
predicted_by_sex <- bind_rows(
      adj_predic_fn(factor = adj_complete_12m$sex, value = "female",
                    part_1 = sex_f_binomial, part_2 = sex_f_hurdle_12m),
      adj_predic_fn(factor = adj_complete_12m$sex, value = "male",
                    part_1 = sex_m_binomial, part_2 = sex_m_hurdle_12m)) %>%
      mutate(group = "Sex") %>% relocate(group)



# By age groups-----
# set ref for each stratum
adj_complete_12m$age_cat %>% levels #"18-29" "30-39" "40-49" "50-59" "60-69" "70+"
# ref: 18-29
adj_complete_12m$age_cat_18 <- relevel(adj_complete_12m$age_cat, ref = "18-29")
# ref: "30-39"
adj_complete_12m$age_cat_30 <- relevel(adj_complete_12m$age_cat, ref = "30-39")
# ref: "40-49"
adj_complete_12m$age_cat_40 <- relevel(adj_complete_12m$age_cat, ref = "40-49")
# ref: "50-59"
adj_complete_12m$age_cat_50 <- relevel(adj_complete_12m$age_cat, ref = "50-59")
# ref: "60-69"
adj_complete_12m$age_cat_60 <- relevel(adj_complete_12m$age_cat, ref = "60-69")
# ref: "70+"
adj_complete_12m$age_cat_70 <- relevel(adj_complete_12m$age_cat, ref = "70+"  )



# Part 1: Binomial model:

# No interaction:
age_no_binomial_12m <- glm(visits_binary ~ exposure + sex + offset(log(follow_up)) +
                                 age_cat_18 + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                                 previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat,
                           data = adj_complete_12m,
                           family=binomial(link="logit"))
# set up a function to shorten the codes:
age_bi_interaction_fn <- function(cat){
      glm(visits_binary ~ exposure*cat + sex + offset(log(follow_up)) +
                bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat,
          data = adj_complete_12m,
          family=binomial(link="logit"))
}

# Age 18 group (same as default)
age_18_bi <- age_bi_interaction_fn(adj_complete_12m$age_cat_18)
# Age 30 group
age_30_bi <- age_bi_interaction_fn(adj_complete_12m$age_cat_30)
# Age 40:
age_40_bi <- age_bi_interaction_fn(adj_complete_12m$age_cat_40)
# Age 50:
age_50_bi <- age_bi_interaction_fn(adj_complete_12m$age_cat_50)
# Age 60:
age_60_bi <- age_bi_interaction_fn(adj_complete_12m$age_cat_60)
# Age 70:
age_70_bi <- age_bi_interaction_fn(adj_complete_12m$age_cat_70)

# LR test
lr_age_bi <- lmtest::lrtest(age_18_bi, age_no_binomial_12m) %>%
      dplyr::select(`Pr(>Chisq)`) %>%
      add_row(`Pr(>Chisq)` = c(NA,NA,NA,NA)) # add rows for combining results

# Organise binomial regression outputs
age_bi_sub <- bind_rows( binomial_tidy_fn(age_18_bi) %>% mutate(stratum = "18-29"),
                         binomial_tidy_fn(age_30_bi) %>% mutate(stratum = "30-39"),
                         binomial_tidy_fn(age_40_bi) %>% mutate(stratum = "40-49"),
                         binomial_tidy_fn(age_50_bi) %>% mutate(stratum = "50-59"),
                         binomial_tidy_fn(age_60_bi) %>% mutate(stratum = "60-69"),
                         binomial_tidy_fn(age_70_bi) %>% mutate(stratum = "70+")) %>%
      add_column(lr_age_bi)


# Part 2: positive negative binomial

# No interaction
age_no_hurdle_12m <- vglm(visits ~ exposure + age_cat_18 + offset(log(follow_up)) +
                                sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                                previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat,
                          family = posnegbinomial(),
                          data = subset(adj_complete_12m, visits_binary > 0))

# ref: 18
age_18_hurdle <- vglm(visits ~ exposure*age_cat_18 + offset(log(follow_up)) +
           sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
           previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat,
     family = posnegbinomial(),
     data = subset(adj_complete_12m, visits_binary > 0))

# ref: 30
age_30_hurdle <- vglm(visits ~ exposure*age_cat_30 + offset(log(follow_up)) +
                            sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                            previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat,
                      family = posnegbinomial(),
                      data = subset(adj_complete_12m, visits_binary > 0))

# ref: 40
age_40_hurdle <- vglm(visits ~ exposure*age_cat_40 + offset(log(follow_up)) +
                            sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                            previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat,
                      family = posnegbinomial(),
                      data = subset(adj_complete_12m, visits_binary > 0))

# ref: 50
age_50_hurdle <- vglm(visits ~ exposure*age_cat_50 + offset(log(follow_up)) +
                            sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                            previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat,
                      family = posnegbinomial(),
                      data = subset(adj_complete_12m, visits_binary > 0))

# ref: 60
age_60_hurdle <- vglm(visits ~ exposure*age_cat_60 + offset(log(follow_up)) +
                            sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                            previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat,
                      family = posnegbinomial(),
                      data = subset(adj_complete_12m, visits_binary > 0))

# ref: 70
age_70_hurdle <- vglm(visits ~ exposure*age_cat_70 + offset(log(follow_up)) +
                            sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                            previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat,
                      family = posnegbinomial(),
                      data = subset(adj_complete_12m, visits_binary > 0))

# LR test:
age_cat_hurdle_lrt <- lrt_hurdle(age_18_hurdle, age_no_hurdle_12m) %>%
      add_row(`Pr(>Chisq)` = c(NA,NA,NA,NA))


# organise age outputs
age_hurdle_sub <- bind_rows(
      positive_nb_tidy_fu(age_18_hurdle) %>% mutate(stratum = "18-29"),
      positive_nb_tidy_fu(age_30_hurdle) %>% mutate(stratum = "30-39"),
      positive_nb_tidy_fu(age_40_hurdle) %>% mutate(stratum = "40-49"),
      positive_nb_tidy_fu(age_50_hurdle) %>% mutate(stratum = "50-59"),
      positive_nb_tidy_fu(age_60_hurdle) %>% mutate(stratum = "60-69"),
      positive_nb_tidy_fu(age_70_hurdle) %>% mutate(stratum = "70+")) %>%
      add_column(age_cat_hurdle_lrt)


# Save outputs:
bind_rows(age_bi_sub, age_hurdle_sub) %>% write_csv(here("output", "st04_04_stratum_age.csv"))


# Predict by age groups:
predicted_by_agegroup <- bind_rows(
      adj_predic_fn(factor = adj_complete_12m$age_cat, value = "18-29",
                    part_1 = age_18_bi, part_2 = age_18_hurdle),
      adj_predic_fn(factor = adj_complete_12m$age_cat, value = "30-39",
                    part_1 = age_30_bi, part_2 = age_30_hurdle),
      adj_predic_fn(factor = adj_complete_12m$age_cat, value = "40-49",
                    part_1 = age_40_bi, part_2 = age_40_hurdle),
      adj_predic_fn(factor = adj_complete_12m$age_cat, value = "50-59",
                    part_1 = age_50_bi, part_2 = age_50_hurdle),
      adj_predic_fn(factor = adj_complete_12m$age_cat, value = "60-69",
                    part_1 = age_60_bi, part_2 = age_60_hurdle),
      adj_predic_fn(factor = adj_complete_12m$age_cat, value = "70+",
                    part_1 = age_70_bi, part_2 = age_70_hurdle)) %>%
      mutate(group = "Age group") %>% relocate(group)


# combine the prediction outputs:
bind_rows(
      predicted_by_hos,
      predicted_by_sex,predicted_by_agegroup
      ) %>% write_csv(here("output", "st04_04_stratum_predicted_value.csv"))
