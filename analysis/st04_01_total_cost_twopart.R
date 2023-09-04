source("analysis/dm04_03_pivot_long.R")

# Goal: analysing long COVID exposure and the cost outcomes
# Model: two-part model 

# Data management: colllapsing data by different follow-up time. -------
# 3 months 
matched_cost_3m <- matched_cost_ts %>% 
      filter(month %in% c(1,2,3)& !is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            total_cost = sum(monthly_total_cost, na.rm =T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup()

# 6 months
matched_cost_6m <- matched_cost_ts %>% 
      filter(month %in% c(1,2,3,4,5,6)& !is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            total_cost = sum(monthly_total_cost, na.rm =T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup()

# 12 months
matched_cost_12m <- matched_cost_ts %>% 
      filter(!is.na(follow_up_time)) %>% 
      group_by(patient_id, exposure) %>% 
      summarise(
            total_cost = sum(monthly_total_cost, na.rm =T),
            follow_up = sum(follow_up_time, na.rm = T)) %>% 
      ungroup()


# # Add covariates for adjustment
for_covariates <- matched_cost_ts %>% distinct(patient_id, exposure, .keep_all = T) %>% 
      dplyr::select("patient_id",     
                    "exposure",           
                    "age_cat",                 
                    "sex",                     
                    "bmi_cat",
                    "ethnicity_6",             
                    "imd_q5",                  
                    "region",      
                    "previous_covid_hosp",     
                    "cov_covid_vax_n_cat",     
                    "number_comorbidities_cat")

levels_check <- c("exposure", "age_cat", "sex", "bmi_cat", "ethnicity_6", "imd_q5",                  
                  "region", "previous_covid_hosp", "cov_covid_vax_n_cat", "number_comorbidities_cat")


for_covariates$sex <- relevel(for_covariates$sex, ref = "male")
for_covariates$bmi_cat <- relevel(for_covariates$bmi_cat, ref = "Normal Weight")
for_covariates$ethnicity_6 <- relevel(for_covariates$ethnicity_6, ref = "White")
for_covariates$imd_q5 <- relevel(for_covariates$imd_q5, ref = "least_deprived")
for_covariates$region <- relevel(for_covariates$region, ref = "London" )
for_covariates$previous_covid_hosp <- relevel(for_covariates$previous_covid_hosp, ref = "FALSE")
for_covariates$cov_covid_vax_n_cat <- relevel(for_covariates$cov_covid_vax_n_cat, ref = "0 dose")
for_covariates$number_comorbidities_cat <- relevel(for_covariates$number_comorbidities_cat, ref = "0")

lapply(for_covariates[levels_check], levels) # need to correct some levels

# # add covariates back to the summarised data frame
matched_cost_3m <- left_join(matched_cost_3m, for_covariates,
                             by = c("patient_id" = "patient_id", "exposure" = "exposure"))

matched_cost_6m <- left_join(matched_cost_6m, for_covariates,
                             by = c("patient_id" = "patient_id", "exposure" = "exposure"))

matched_cost_12m <- left_join(matched_cost_12m, for_covariates,
                              by = c("patient_id" = "patient_id", "exposure" = "exposure"))

# Make sure the exposure level is correct
matched_cost_3m$exposure <- relevel(matched_cost_3m$exposure, ref = "Comparator")
matched_cost_6m$exposure <- relevel(matched_cost_6m$exposure, ref = "Comparator")
matched_cost_12m$exposure <- relevel(matched_cost_12m$exposure, ref = "Comparator")

# Stats: Two-part model: -----
# Data management: exclude rows with NA in the model: 
crude_vars <- c("total_cost", "exposure", "follow_up")

crude_cost_complete_3m <- matched_cost_3m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(cost_binary = ifelse(total_cost>0, 1, 0))
crude_cost_complete_6m <- matched_cost_3m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(cost_binary = ifelse(total_cost>0, 1, 0))
crude_cost_complete_12m <- matched_cost_3m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(cost_binary = ifelse(total_cost>0, 1, 0)) 

# Two-part model:

# # use twopm: same results
# crude_twopm_3m <- tpm(total_cost ~ exposure + offset(log(follow_up)),
#                       data = crude_cost_complete_3m,
#                       link_part1 = "logit", family_part2 = Gamma(link = "log"))
# summary(crude_twopm_3m)


# Write a function to organise the crude model outputs:
crude_twopart_fn <- function(dataset, time_length){
      
      # Binomial part: 
      curde_binomial_3m <- glm(cost_binary ~ exposure + offset(log(follow_up)),
                               data = dataset,
                               family = binomial(link="logit"))
      
      part_binomial <-  tidy(curde_binomial_3m) %>% mutate(
            model = "binomial",
            lci = exp(estimate - 1.69*std.error),
            hci = exp(estimate + 1.69*std.error),
            estimate = exp(estimate)) %>% 
            dplyr::select(model, term, estimate, lci, hci, p.value)%>% 
        filter(term == "exposureLong covid exposure" )
      
      # Gamma glm part: 
      crude_gamma_3m <-  glm(total_cost ~ exposure + offset(log(follow_up)),
                             data = subset(dataset, cost_binary>0),
                             family = Gamma(link="log"))
      
      part_gaama <- tidy(crude_gamma_3m) %>% mutate(
            model = "Gamma GLM",
            lci = exp(estimate - 1.69*std.error),
            hci = exp(estimate + 1.69*std.error),
            estimate = exp(estimate)) %>% 
            dplyr::select(model, term, estimate, lci, hci, p.value)%>% 
        filter(term == "exposureLong covid exposure" )
      
      results <- bind_rows(part_binomial, part_gaama) %>% 
            mutate(time=time_length) %>% relocate(time)
      
      return(results)
      
}

# Organised crude model output: -------
crude_tpm <- bind_rows(crude_twopart_fn(crude_cost_complete_3m, "3 months"),
                       crude_twopart_fn(crude_cost_complete_6m, "6 months"),
                       crude_twopart_fn(crude_cost_complete_12m, "12 months")) %>% 
  mutate(adjustement = "Crude")


# Adjusted two-part model: 
# Data management: keep complete data
adj_cost_complete_3m <- matched_cost_3m[complete.cases(matched_cost_3m),] %>% 
  mutate(cost_binary = ifelse(total_cost>0, 1, 0))
adj_cost_complete_6m <- matched_cost_6m[complete.cases(matched_cost_6m),] %>% 
  mutate(cost_binary = ifelse(total_cost>0, 1, 0))
adj_cost_complete_12m <- matched_cost_12m[complete.cases(matched_cost_12m),] %>% 
  mutate(cost_binary = ifelse(total_cost>0, 1, 0))

# Write a function to organise the adjusted model outputs:
adj_twopart_fn <- function(dataset, time_length){
  
  # Binomial part: 
  curde_binomial_3m <- glm(cost_binary ~ exposure + offset(log(follow_up))+ age_cat + sex  + cov_covid_vax_n_cat + 
                             bmi_cat + imd_q5 + ethnicity_6 + region + number_comorbidities_cat,
                           data = dataset,
                           family = binomial(link="logit"))
  
  part_binomial <-  tidy(curde_binomial_3m) %>% mutate(
    model = "binomial",
    lci = exp(estimate - 1.69*std.error),
    hci = exp(estimate + 1.69*std.error),
    estimate = exp(estimate)) %>% 
    dplyr::select(model, term, estimate, lci, hci, p.value) %>% 
    filter(term == "exposureLong covid exposure" )
  
  # Gamma glm part: 
  crude_gamma_3m <-  glm(total_cost ~ exposure + offset(log(follow_up))+ age_cat + sex  + cov_covid_vax_n_cat + 
                           bmi_cat + imd_q5 + ethnicity_6 + region + number_comorbidities_cat,
                         data = subset(dataset, cost_binary>0),
                         family = Gamma(link="log"))
  
  part_gaama <- tidy(crude_gamma_3m) %>% mutate(
    model = "Gamma GLM",
    lci = exp(estimate - 1.69*std.error),
    hci = exp(estimate + 1.69*std.error),
    estimate = exp(estimate)) %>% 
    dplyr::select(model, term, estimate, lci, hci, p.value)%>% 
    filter(term == "exposureLong covid exposure" )
  
  results <- bind_rows(part_binomial, part_gaama) %>% 
    mutate(time=time_length) %>% relocate(time)
  
  return(results)
  
}

# Organised crude model output: -------
adj_tpm <- bind_rows(crude_twopart_fn(adj_cost_complete_3m, "3 months"),
                       crude_twopart_fn(adj_cost_complete_6m, "6 months"),
                       crude_twopart_fn(adj_cost_complete_12m, "12 months")) %>% 
  mutate(adjustement = "Adjusted")

# Combine outputs
bind_rows(crude_tpm, adj_tpm) %>% write_csv(here("output, st_04_01_total_cost_tpm.csv"))