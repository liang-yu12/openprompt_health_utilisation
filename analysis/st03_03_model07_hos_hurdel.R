# Load previous data management
source("analysis/dm03_7_pivot_hos_long.R")

# Data management for modeling:: --------
# Collapsing data by summarising the visits and follow-up time, and 
# generate three datasets for follow-up 12m


# follow 12 months 
matched_data_hos_12m <- matched_data_hos_ts %>% 
  filter(!is.na(follow_up_time)) %>% 
  group_by(patient_id, exposure) %>% 
  summarise(
    visits = sum(monthly_hos_visits),
    follow_up = sum(follow_up_time)) %>% 
  ungroup()


# # Add covariates for adjustment
for_covariates <- matched_data_hos_ts %>% distinct(patient_id, exposure, .keep_all = T) %>% 
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
for_covariates$sex <- relevel(for_covariates$sex, ref = "male")
for_covariates$bmi_cat <- relevel(for_covariates$bmi_cat, ref = "Normal Weight")
for_covariates$ethnicity_6 <- relevel(for_covariates$ethnicity_6, ref = "White")
for_covariates$imd_q5 <- relevel(for_covariates$imd_q5, ref = "least_deprived")
for_covariates$region <- relevel(for_covariates$region, ref = "London" )
for_covariates$previous_covid_hosp <- relevel(for_covariates$previous_covid_hosp, ref = "FALSE")
for_covariates$cov_covid_vax_n_cat <- relevel(for_covariates$cov_covid_vax_n_cat, ref = "0 dose")
for_covariates$number_comorbidities_cat <- relevel(for_covariates$number_comorbidities_cat, ref = "0")

# # add covariates back to the summarised data frame

matched_data_hos_12m <- left_join(matched_data_hos_12m, for_covariates,
                                  by = c("patient_id" = "patient_id", "exposure" = "exposure"))

# correct the level of exposure groups
matched_data_hos_12m$exposure <- relevel(matched_data_hos_12m$exposure, ref = "Comparator")


# Stats: two part (Hurdle) model -----
# first need to exclude rows with NA and create 1/0 outcomes:
crude_vars <- c("visits", "exposure", "follow_up")#for crude anaylsis

crude_hos_complete_12m <- matched_data_hos_12m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Crude Hurdle model: ------
# Part 1: binomial model: 
crude_binomial_12m <-  glm(visits_binary ~ exposure + offset(log(follow_up)), data = crude_hos_complete_12m,
                           family=binomial(link="logit")) 

# Part 2: Positive negative binomial (truncated)
crude_nb_12m <- vglm(visits ~ exposure + offset(log(follow_up)),
                     family = posnegbinomial(),
                     data = subset(crude_hos_complete_12m, visits_binary > 0))

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
      (binomial_tidy_fn(crude_binomial_12m) %>% mutate(time="12 months"))
) %>% mutate(Adjustment = "Crude GP")

# Organise the second part outputs:
crude_hurdle_outputs <- bind_rows(
      (positive_nb_tidy_fu(crude_nb_12m) %>% mutate(time="12 months"))
) %>% mutate(Adjustment = "Crude GP")


# Adjusted hurdle model:
# Data management: excluding NA:
adj_hos_complete_12m <- matched_data_hos_12m[complete.cases(matched_data_hos_12m),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Hurdle model part 1: binomial model:
# 12 Months
adj_binomial_12m <- glm(visits_binary ~ exposure + offset(log(follow_up)) +
                              age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + 
                              previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat, 
                        data = adj_hos_complete_12m,
                        family=binomial(link="logit")) 


# Hurdle model part 2: truncated negative binomial model:
# 12 months
adj_nb_12m <- vglm(visits ~ exposure + offset(log(follow_up))+
                         age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + 
                         previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat, 
                   family = posnegbinomial(),
                   data = subset(adj_hos_complete_12m, visits_binary > 0))


# Combine and organised regression outputs
adj_binomial_outputs <-bind_rows(
      (binomial_tidy_fn(adj_binomial_12m) %>% mutate(time="12 months"))
) %>% mutate(Adjustment = "Hospital Adjusted")

# Organise the second part outputs:
adj_hurdle_outputs <- bind_rows(
      (positive_nb_tidy_fu(adj_nb_12m) %>% mutate(time="12 months"))
) %>% mutate(Adjustment = "Hospital Adjusted")


# Combine outputs
# Save both outputs: # Combine total outputs and save:
st03_03_hos_binomial <- bind_rows(crude_binomial_outputs, adj_binomial_outputs)
st03_03_hos_binomial %>% write_csv(here("output", "st03_03_hos_binomial.csv"))

st03_03_hos_hurdle <- bind_rows(crude_hurdle_outputs, adj_hurdle_outputs)
st03_03_hos_hurdle %>% write_csv(here("output", "st03_03_hos_hurdle.csv"))



