# Load previous data management
source("analysis/dm03_5_matched_pivot_long.R")

# Goal: run the gubgroup analyses by 
# - sex: "female" "male"  
# - age groups: "18-29" "30-39" "40-49" "50-59" "60-69" "70+"  
# - previous hospitalisation:"FALSE" "TRUE" 

# outcome types: 3m, 6m, 12m,
# model:hurdle model 

# Data management for modeling:: --------
# Collapsing data by summarising the visits and follow-up time, and 
# generate three datasets for follow-up 3m, 6m, and 12m

# # 3 months
matched_data_3m <- matched_data_ts %>% 
      filter(month %in% c(1,2,3) & !is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_visits),
            follow_up = sum(follow_up_time)) %>% 
      ungroup()

# # 6 months
matched_data_6m <- matched_data_ts %>% 
      filter(month %in% c(1,2,3,4,5,6) & !is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_visits),
            follow_up = sum(follow_up_time)) %>% 
      ungroup()

# follow 12 months 
matched_data_12m <- matched_data_ts %>% 
      filter(!is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_visits),
            follow_up = sum(follow_up_time)) %>% 
      ungroup()


# # Add covariates for adjustment
for_covariates <- matched_data_ts %>% distinct(patient_id, exposure, .keep_all = T) %>% 
      dplyr::select("patient_id",     
                    "exposure",           
                    "age", "age_cat",               
                    "sex",                     
                    "bmi_cat",
                    "ethnicity_6",             
                    "imd_q5",                  
                    "region",      
                    "cov_asthma",
                    "cov_mental_health",   
                    "previous_covid_hosp",     
                    "cov_covid_vax_n_cat",     
                    "number_comorbidities_cat")
# Assign covariate levels before adding them back:
for_covariates$sex <- relevel(for_covariates$sex, ref = "female")
for_covariates$cov_mental_health <- relevel(for_covariates$cov_mental_health, ref = "FALSE")
for_covariates$previous_covid_hosp <- relevel(for_covariates$previous_covid_hosp, ref = "FALSE")
for_covariates$previous_covid_hosp <- relevel(for_covariates$previous_covid_hosp, "FALSE")
for_covariates$number_comorbidities_cat <- as.factor(for_covariates$number_comorbidities_cat)

# # add covariates back to the summarised data frame
matched_data_3m <- left_join(matched_data_3m, for_covariates,
                               by = c("patient_id" = "patient_id", "exposure" = "exposure"))

matched_data_6m <- left_join(matched_data_6m, for_covariates,
                             by = c("patient_id" = "patient_id", "exposure" = "exposure"))

matched_data_12m <- left_join(matched_data_12m, for_covariates,
                             by = c("patient_id" = "patient_id", "exposure" = "exposure"))

# correct the levels of variables
matched_data_3m$exposure <- relevel(matched_data_3m$exposure, ref = "Comparator")
matched_data_6m$exposure <- relevel(matched_data_6m$exposure, ref = "Comparator")
matched_data_12m$exposure <- relevel(matched_data_12m$exposure, ref = "Comparator")

# Keep complete cases
adj_complete_3m <- matched_data_12m[complete.cases(matched_data_12m),] %>% 
  mutate(visits_binary = ifelse(visits>0, 1, 0))

adj_complete_6m <- matched_data_12m[complete.cases(matched_data_12m),] %>% 
  mutate(visits_binary = ifelse(visits>0, 1, 0))

adj_complete_12m <- matched_data_12m[complete.cases(matched_data_12m),] %>% 
  mutate(visits_binary = ifelse(visits>0, 1, 0))


# Stats: Subgroup analyses by different covariates -----
# Fit an interaction term between exposure and the cov.

# # By previous hospitalisation ----

adj_complete_12m$previous_covid_hosp %>% levels()  # "FALSE" "TRUE" 
# create a new var using TRUE as baseline for interaction
adj_complete_12m$previous_covid_hosp_true <- factor(adj_complete_12m$previous_covid_hosp, 
                                                    levels = c("TRUE", "FALSE"))

# first-part: Binomial model

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
lrt_bi <- lmtest::lrtest(hos_f_binomial_12m, hos_no_binomial_12m) %>% 
  dplyr::select(`Pr(>Chisq)`) 


# Organise binomial outcomes:
hos_bi_sub <- bind_rows(
  binomial_tidy_fn(hos_f_binomial_12m) %>% mutate(stratum = "No hospital admission"),
  binomial_tidy_fn(hos_t_binomial_12m) %>% mutate(stratum = "Admitted due to COVID")) %>% 
bind_cols(lrt_bi)


# # Second hurdle part: positive negative binomial:

# Tidy up the function
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
# LR test:
lrt_hurdle <- lrtest_vglm(hos_f_hurdle_12m, hos_no_hurdle_12m)
lrt_hos_tpm <- lrt_hurdle@Body %>% as.data.frame() %>% dplyr::select(`Pr(>Chisq)`)


bind_rows(positive_nb_tidy_fu(hos_f_hurdle_12m) %>% mutate(stratum = "No hospital admission"),
          positive_nb_tidy_fu(hos_t_binomial_12m) %>% mutate(stratum = "Admitted due to COVID")) %>% 
bind_cols(lrt_hos_tpm)








## By sex:-----




### By agegroup
# customize a function for age group:
crude_age_subgroup_fn <- function(data, age_var){
      # split the data by covid hospitalisation
      age_c <-split(data, age_var)
      age_18_29 <- age_c$`18-29`
      age_30_39 <- age_c$`30-39`
      age_40_49 <- age_c$`40-49`
      age_50_59 <- age_c$`50-59`
      age_60_69 <- age_c$`60-69`
      age_over_70 <- age_c$`70+`
      
      results <- bind_rows(
            nb_reg_crude_fn(age_18_29, "Age 28-29"),
            nb_reg_crude_fn(age_30_39, "Age 30-39"),
            nb_reg_crude_fn(age_40_49, "Age 40-49"),
            nb_reg_crude_fn(age_50_59, "Age 50-59"),
            nb_reg_crude_fn(age_60_69, "Age 60-69"),
            nb_reg_crude_fn(age_over_70, "Age over 70")
      ) 
      return(results)
}

crude_age_subgroup <- bind_rows(
      crude_age_subgroup_fn(matched_data_3m, matched_data_3m$age_cat) %>% mutate(time="3m"),
      crude_age_subgroup_fn(matched_data_6m, matched_data_6m$age_cat) %>% mutate(time="6m"),
      crude_age_subgroup_fn(matched_data_12m, matched_data_12m$age_cat) %>% mutate(time="12m")
)


# Model 2: adjusted hurdle model with negative binomial: ------
# Only Keep complete data:

adj_complete_3m <- matched_data_3m[complete.cases(matched_data_3m),]
adj_complete_6m <- matched_data_6m[complete.cases(matched_data_6m),]
adj_complete_12m <- matched_data_12m[complete.cases(matched_data_12m),]


# By hospitalisation adjusted model ----
# Set up reg function output: need to customized for hospitalisation: 
hos_hurdle_reg_adj_fn <- function(sub_data, sub_group_name){
      
      reg <- hurdle(visits ~ exposure + sex + age_cat + region  + imd_q5 + 
                          ethnicity_6 + bmi_cat + number_comorbidities_cat + 
                          cov_covid_vax_n_cat, 
                    offset = log(follow_up),
                    data = sub_data,
                    zero.dist = "binomial",
                    dist = "negbin")
      
      output <- bind_cols((coef(reg) %>% exp() %>% as.data.frame()),
                          (confint(reg) %>% exp() %>% as.data.frame()))
      
      output$terms <- rownames(output)
      output <- output %>% filter(terms != "count_(Intercept)" & terms != "zero_(Intercept)")
      output <- rename(output, estimates = .) %>% relocate(terms) %>% 
            mutate(subgroup = sub_group_name) %>% relocate(subgroup)
      return(output)
}


adj_hos_subgroup_fn <- function(data, hos_var){
      # split the data by covid hospitalisation
      hos <-split(data, hos_var)
      hos_t <- hos$`TRUE`
      hos_f <- hos$`FALSE`
      
      results <- bind_rows(
            hos_hurdle_reg_adj_fn(hos_t, "Hospitalised"),
            hos_hurdle_reg_adj_fn(hos_f, "Not hospitalised")
      ) 
      return(results)
}

adj_hos_subgroup <- bind_rows(
      adj_hos_subgroup_fn(adj_complete_3m, adj_complete_3m$previous_covid_hosp) %>% mutate(time="3m"),
      adj_hos_subgroup_fn(adj_complete_6m, adj_complete_6m$previous_covid_hosp) %>% mutate(time="6m"),
      adj_hos_subgroup_fn(adj_complete_12m, adj_complete_12m$previous_covid_hosp) %>% mutate(time="12m"))


# By sex adjusted model ----
# set hurdle function for running sex subgroup
sex_hurdle_reg_adj_fn <- function(sub_data, sub_group_name){
      
      reg <- hurdle(visits ~ exposure + age_cat + region  + imd_q5 + 
                          ethnicity_6 + bmi_cat + number_comorbidities_cat + 
                          previous_covid_hosp + cov_covid_vax_n_cat, 
                    offset = log(follow_up),
                    data = sub_data,
                    zero.dist = "binomial",
                    dist = "negbin")
      
      output <- bind_cols((coef(reg) %>% exp() %>% as.data.frame()),
                          (confint(reg) %>% exp() %>% as.data.frame()))
      
      output$terms <- rownames(output)
      output <- output %>% filter(terms != "count_(Intercept)" & terms != "zero_(Intercept)")
      output <- rename(output, estimates = .) %>% relocate(terms) %>% 
            mutate(subgroup = sub_group_name) %>% relocate(subgroup)
      return(output)
}

# Split function 
adj_sex_subgroup_fn <- function(data, sex_var){
      # split the data by covid hospitalisation
      sex_g <-split(data, sex_var)
      sex_m <- sex_g$male
      sex_f <- sex_g$female
      
      results <- bind_rows(
            sex_hurdle_reg_adj_fn(sex_m, "Male"),
            sex_hurdle_reg_adj_fn(sex_f, "Female")
      ) 
      return(results)
}

# Combine results
adj_sex_subgroup <- bind_rows(
      adj_sex_subgroup_fn(adj_complete_3m, adj_complete_3m$sex) %>% mutate(time="3m"),
      adj_sex_subgroup_fn(adj_complete_6m, adj_complete_6m$sex) %>% mutate(time="6m"),
      adj_sex_subgroup_fn(adj_complete_12m, adj_complete_12m$sex) %>% mutate(time="12m")
)


# By age groups ------------
# set hurdle function for age group regression 
age_hurdle_reg_adj_fn <- function(sub_data, sub_group_name){
      
      reg <- hurdle(visits ~ exposure + sex+ region  + imd_q5 + 
                          ethnicity_6 + bmi_cat + number_comorbidities_cat + 
                          previous_covid_hosp + cov_covid_vax_n_cat, 
                    offset = log(follow_up),
                    data = sub_data,
                    zero.dist = "binomial",
                    dist = "negbin")
      
      output <- bind_cols((coef(reg) %>% exp() %>% as.data.frame()),
                          (confint(reg) %>% exp() %>% as.data.frame()))
      
      output$terms <- rownames(output)
      output <- output %>% filter(terms != "count_(Intercept)" & terms != "zero_(Intercept)")
      output <- rename(output, estimates = .) %>% relocate(terms) %>% 
            mutate(subgroup = sub_group_name) %>% relocate(subgroup)
      return(output)
}

adj_age_hurdle_subgroup_fn <- function(data, age_var){
      # split the data by covid hospitalisation
      age_c <-split(data, age_var)
      age_18_29 <- age_c$`18-29`
      age_30_39 <- age_c$`30-39`
      age_40_49 <- age_c$`40-49`
      age_50_59 <- age_c$`50-59`
      age_60_69 <- age_c$`60-69`
      age_over_70 <- age_c$`70+`
      
      results <- bind_rows(
            age_hurdle_reg_adj_fn(age_18_29, "Age 28-29"),
            age_hurdle_reg_adj_fn(age_30_39, "Age 30-39"),
            age_hurdle_reg_adj_fn(age_40_49, "Age 40-49"),
            age_hurdle_reg_adj_fn(age_50_59, "Age 50-59"),
            age_hurdle_reg_adj_fn(age_60_69, "Age 60-69"),
            age_hurdle_reg_adj_fn(age_over_70, "Age over 70")
      ) 
      return(results)
}



# Combine results
adj_age_subgroup <- bind_rows(
      adj_age_hurdle_subgroup_fn(adj_complete_3m, adj_complete_3m$age_cat) %>% mutate(time="3m"),
      adj_age_hurdle_subgroup_fn(adj_complete_6m, adj_complete_6m$age_cat) %>% mutate(time="6m"),
      adj_age_hurdle_subgroup_fn(adj_complete_12m, adj_complete_12m$age_cat) %>% mutate(time="12m")
)





# Save outputs

bind_rows(
      (bind_rows(crude_hos_subgroup, crude_sex_subgroup, crude_age_subgroup) %>% mutate(model_type = "Crude")),
      (bind_rows(adj_hos_subgroup, adj_sex_subgroup, adj_age_subgroup) %>% mutate(model_type = "Crude"))) %>% 
      write_csv(here("output", "st_03_05_hurdle_subgroup.csv"))