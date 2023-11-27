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


# correct the level of exposure groups
matched_data_hos_12m$exposure <- relevel(matched_data_hos_12m$exposure, ref = "Comparator")

# Exclude rows with NA and create 1/0 outcomes:
crude_vars <- c("visits", "exposure", "follow_up")#for crude anaylsis

crude_hos_complete_12m <- matched_data_hos_12m %>% drop_na(any_of(crude_vars)) %>% 
      mutate(visits_binary = ifelse(visits>0, 1, 0))

# 1. Exploring data distribution: -----
# Plots distribution:
svg(file=here("output", "qc05_hos_distribution_compare.svg"),
    width=1800, height=900)

matched_data_hos_12m %>% 
      ggplot(aes(x=visits, color = exposure, fill = exposure)) + 
      geom_histogram(position = "dodge") +
      ggtitle("Current OPA visits")

dev.off()

# 2. Check over dispersion: ------
# Fit a poisson model to the data
mod <- glm(visits ~ exposure + offset(log(follow_up)),
           family = poisson,
           data = crude_hos_complete_12m)

# Calculate the chisq and deviance / degree of freedom
with(mod, cbind(res.deviance = deviance, 
                df = df.residual,
                p = pchisq(deviance, df.residual, lower.tail=FALSE),
                dev_df_ratio = deviance/df.residual)) %>% as.data.frame() %>% 
      write_csv(here("output", "qc05_hos_vist_overdispersion_test.csv"))


# 3. Stats: model comparison -----
# Part 2: Positive negative binomial (truncated)
pos_negbinomial <- vglm(visits ~ exposure + offset(log(follow_up)),
                     family = posnegbinomial(),
                     data = subset(crude_hos_complete_12m, visits_binary > 0))

pos_poisson <- vglm(visits ~ exposure + offset(log(follow_up)),
                    family = pospoisson,
                    data = subset(crude_hos_complete_12m, visits_binary > 0))
# Compare models:
bind_rows(
      data.frame(
            Model = "Positive negative binomial",
            AIC = AICvlm(pos_negbinomial),
            BIC = BICvlm(pos_negbinomial)
            ),
      data.frame(
            Model = "Positive Poisson",
            AIC = AICvlm(pos_poisson),
            BIC = BICvlm(pos_poisson))
) %>% write_csv(here("output", "qc05_hos_vist_model_compare.csv"))
