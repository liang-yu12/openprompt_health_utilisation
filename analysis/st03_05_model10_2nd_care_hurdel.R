source("analysis/dm03_10_pivot_second_care_long.R")

# Run hurdle model of the secondary care data

# Data management: 
# # 3 months
matched_3m_2nd <- matched_data_2nd_ts %>% 
      filter(month %in% c(1,2,3) & !is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_secondary_care_visits, na.rm = T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup()

# # 6 months
matched_6m_2nd <- matched_data_2nd_ts %>% 
      filter(month %in%  c(1,2,3,4,5,6) & !is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_secondary_care_visits, na.rm = T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup()

# # 12 months
matched_12m_2nd <- matched_data_2nd_ts %>% 
      filter(!is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            visits = sum(monthly_secondary_care_visits, na.rm = T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup()


# # Add covariates for adjustment
for_covariates <- matched_data_2nd_ts %>% distinct(patient_id, exposure, .keep_all = T) %>% 
      dplyr::select("patient_id",     
                    "exposure",           
                    "age",                 
                    "sex",                     
                    "bmi_cat",
                    "ethnicity_6",             
                    "imd_q5",                  
                    "region",      
                    "previous_covid_hosp",     
                    "cov_covid_vax_n_cat",     
                    "number_comorbidities_cat")

# Assign covariate levels before adding them back:
for_covariates$sex <- relevel(for_covariates$sex, ref = "male")
for_covariates$ethnicity_6 <- relevel(for_covariates$ethnicity_6, ref = "White")
for_covariates$imd_q5 <- relevel(for_covariates$imd_q5, ref = "least_deprived")
for_covariates$region <- relevel(for_covariates$region, ref = "London" )
for_covariates$previous_covid_hosp <- relevel(for_covariates$previous_covid_hosp, ref = "FALSE")
for_covariates$cov_covid_vax_n_cat <- relevel(for_covariates$cov_covid_vax_n_cat, ref = "0 dose")
for_covariates$number_comorbidities_cat <- relevel(for_covariates$number_comorbidities_cat, ref = "0")


matched_3m_2nd <- left_join(matched_3m_2nd, for_covariates,
                            by = c("patient_id" = "patient_id", "exposure" = "exposure"))

matched_6m_2nd <- left_join(matched_6m_2nd, for_covariates,
                            by = c("patient_id" = "patient_id", "exposure" = "exposure"))

matched_12m_2nd <- left_join(matched_12m_2nd, for_covariates,
                            by = c("patient_id" = "patient_id", "exposure" = "exposure"))

matched_3m_2nd$exposure <- relevel(matched_3m_2nd$exposure, ref = "Comparator")
matched_6m_2nd$exposure <- relevel(matched_3m_2nd$exposure, ref = "Comparator")
matched_12m_2nd$exposure <- relevel(matched_3m_2nd$exposure, ref = "Comparator")

# Data management for the hurdle model: -----
# # Crude model: keep complete dataset: ----
crude_vars <- c("visits", "exposure", "follow_up")

crude_complete_3m <- matched_3m_2nd %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

crude_complete_6m <- matched_6m_2nd %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

crude_complete_12m <- matched_12m_2nd %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Crude hurdle model: ----
# # 3 months
# binomial model: 
crude_binomial_3m <-  glm(visits_binary ~ exposure + offset(log(follow_up)), data = crude_complete_3m,
                          family=binomial(link="logit")) 
# Positive negative binomial (truncated)
crude_nb_3m <- vglm(visits ~ exposure + offset(log(follow_up)),
                       family = posnegbinomial(),
                       data = subset(crude_complete_3m, visits_binary > 0))

# # 6 months:
# binomial
crude_binomial_6m <-  glm(visits_binary ~ exposure + offset(log(follow_up)), data = crude_complete_6m,
                          family=binomial(link="logit")) 
# Positive negative binomial (truncated)
crude_nb_6m <- vglm(visits ~ exposure + offset(log(follow_up)),
                    family = posnegbinomial(),
                    data = subset(crude_complete_6m, visits_binary > 0))

# # 12 months
# binomial
crude_binomial_12m <-  glm(visits_binary ~ exposure + offset(log(follow_up)), data = crude_complete_12m,
                          family=binomial(link="logit")) 
# Positive negative binomial (truncated)
crude_nb_12m <- vglm(visits ~ exposure + offset(log(follow_up)),
                    family = posnegbinomial(),
                    data = subset(crude_complete_12m, visits_binary > 0))

# Use a function to organised the regression outputs to get RR and CI:
# Tidy binomial model:
binomial_tidy_fn <- function(bi_reg){
      bi_results <- bi_reg %>% tidy() %>% mutate(
            model = "binomial",
            lci = exp(estimate - 1.69*std.error),
            hci = exp(estimate + 1.69*std.error),
            estimate = exp(estimate)) %>% 
            dplyr::select(model, term, estimate, lci, hci, p.value)%>% 
            filter(term == "exposureLong covid exposure" )      
      return(bi_results)
}

# tidy vglm outputs:
positive_nb_tidy_fu <- function(vg_reg){
      
      t1 <- vg_reg %>%summary
      t2 <- t1@coef3 %>% as.data.frame()
      t2$term <- rownames(t2)
      t3 <- t2 %>% filter(term == "exposureLong covid exposure" )
      results <- t3 %>% mutate(
            lci = exp(Estimate - 1.69*`Std. Error`),
            hci = exp(Estimate + 1.69*`Std. Error`),
            estimate = exp(Estimate),
            p.value = `Pr(>|z|)`,
            model = "Positive Negative Bionomial") %>% 
            dplyr::select(model, term, estimate, lci, hci, p.value)
      return(results)
}

# Organise the first part outputs:
crude_binomial_outputs <-bind_rows(
      (binomial_tidy_fn(crude_binomial_3m) %>% mutate(time="3 months")),
      (binomial_tidy_fn(crude_binomial_6m) %>% mutate(time="6 months")),
      (binomial_tidy_fn(crude_binomial_12m) %>% mutate(time="12 months"))
)

# Organise the second part outputs:
crude_hurdle_outputs <- bind_rows(
      (positive_nb_tidy_fu(crude_nb_3m) %>% mutate(time="3 months")),
      (positive_nb_tidy_fu(crude_nb_6m) %>% mutate(time="6 months")),
      (positive_nb_tidy_fu(crude_nb_12m) %>% mutate(time="12 months"))
)

# Hurdle model adjusted for covariates
# First need to clean the data by excluding obs with NA in variables:
adj_complete_3m <- matched_3m_2nd[complete.cases(matched_3m_2nd),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

adj_complete_6m <- matched_6m_2nd[complete.cases(matched_6m_2nd),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

adj_complete_12m <- matched_12m_2nd[complete.cases(matched_12m_2nd),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))


# Hurdle model part 1: binomial model:
# 3 Months
adj_binomial_3m <- glm(visits_binary ~ exposure + offset(log(follow_up)) +
                        age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + 
                             previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat, 
                       data = adj_complete_3m,
                       family=binomial(link="logit")) 

# 6 Months
adj_binomial_6m <- glm(visits_binary ~ exposure + offset(log(follow_up)) +
                             age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + 
                             previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat, 
                       data = adj_complete_6m,
                       family=binomial(link="logit")) 

# 12 Months
adj_binomial_12m <- glm(visits_binary ~ exposure + offset(log(follow_up)) +
                             age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + 
                             previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat, 
                       data = adj_complete_12m,
                       family=binomial(link="logit")) 

# Hurdle model part 2: positive negative bionomial model:

# Positive negative binomial
adj_nb_3m <- vglm(visits ~ exposure + offset(log(follow_up))+
                        age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + 
                        previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat, 
                     family = posnegbinomial(),
                     data = subset(crude_complete_3m, visits_binary > 0))