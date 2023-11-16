# Load previous data management
source("analysis/dm02_01_now_pivot_total_visits_long.R")


# Show the factors associated with increased outcomes among:
# 1. All participants 

# 0. Common part:  -----

# Stats: two part (Hurdle) model -----

# Use a function to organised the regression outputs to get RR and CI:
# Tidy binomial model:
binomial_tidy_fn <- function(bi_reg){
      bi_results <- bi_reg %>% tidy() %>% mutate(
            model = "binomial",
            lci = exp(estimate - 1.96*std.error),
            hci = exp(estimate + 1.96*std.error),
            estimate = exp(estimate)) %>% 
            dplyr::select(model, term, estimate, lci, hci, p.value)
      return(bi_results)
}

# tidy vglm outputs:
positive_nb_tidy_fu <- function(vg_reg){
      
      t1 <- vg_reg %>%summary
      t2 <- t1@coef3 %>% as.data.frame()
      t2$term <- rownames(t2)
      results <- t2 %>% mutate(
            lci = exp(Estimate - 1.96*`Std. Error`),
            hci = exp(Estimate + 1.96*`Std. Error`),
            estimate = exp(Estimate),
            p.value = `Pr(>|z|)`,
            model = "Positive Negative Bionomial") %>% 
            dplyr::select(model, term, estimate, lci, hci, p.value)
      return(results)
}

# 1. All partricipants -------------------

# set binary visit outcomes
all_complete_12m <- matched_data_12m[complete.cases(matched_data_12m),] %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# Model:

# 12 Months
all_binomial_12m <- glm(visits_binary ~ exposure + offset(log(follow_up)) +
                              age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                              previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat, 
                        data = all_complete_12m,
                        family=binomial(link="logit")) 

# 12 months
all_nb_12m <- vglm(visits ~ exposure+ offset(log(follow_up))+
                         age + sex + bmi_cat + ethnicity_6 + imd_q5 + region + cov_asthma + cov_mental_health +
                         previous_covid_hosp + cov_covid_vax_n_cat +number_comorbidities_cat, 
                   family = posnegbinomial(),
                   data = subset(all_complete_12m, visits_binary > 0))

# Combine and organised regression outputs
all_binomial_outputs <- binomial_tidy_fn(all_binomial_12m) %>% 
      mutate(time="12 months") %>% 
      mutate(Adjustment = "Adjusted",
             Data = "Total")

# Organise the second part outputs:
all_hurdle_outputs <- positive_nb_tidy_fu(all_nb_12m) %>% 
      mutate(time="12 months") %>% 
      mutate(Adjustment = "Adjusted",
             Data = "Total")

# save outputs:
all_binomial_outputs %>% write_csv(here("output", "st04_02_all_factors_binomial.csv"))
all_hurdle_outputs %>% write_csv(here("output", "st04_02_all_factors_hurdle.csv"))


