source("analysis/dm04_02_combine_costs.R")

# Total healthcare visits vs total healthcare costs

lc_exp_matched <- lc_exp_matched %>%
      mutate(
            bi_visit_m1 = ifelse(all_month_m1>0, 1, 0),
            bi_visit_m2 = ifelse(all_month_m2>0, 1, 0),
            bi_visit_m3 = ifelse(all_month_m3>0, 1, 0),
            bi_visit_m4 = ifelse(all_month_m4>0, 1, 0),
            bi_visit_m5 = ifelse(all_month_m5>0, 1, 0),
            bi_visit_m6 = ifelse(all_month_m6>0, 1, 0),
            bi_visit_m7 = ifelse(all_month_m7>0, 1, 0),
            bi_visit_m8 = ifelse(all_month_m8>0, 1, 0),
            bi_visit_m9 = ifelse(all_month_m9>0, 1, 0),
            bi_visit_m10 = ifelse(all_month_m10>0, 1, 0),
            bi_visit_m11 = ifelse(all_month_m11>0, 1, 0),
            bi_visit_m12 = ifelse(all_month_m12>0, 1, 0)
      )

lc_exp_matched <- lc_exp_matched %>% 
      mutate(
            bi_cost_m1 = ifelse(total_cost_1>0, 1, 0),
            bi_cost_m2 = ifelse(total_cost_2>0, 1, 0),
            bi_cost_m3 = ifelse(total_cost_3>0, 1, 0),
            bi_cost_m4 = ifelse(total_cost_4>0, 1, 0),
            bi_cost_m5 = ifelse(total_cost_5>0, 1, 0),
            bi_cost_m6 = ifelse(total_cost_6>0, 1, 0),
            bi_cost_m7 = ifelse(total_cost_7>0, 1, 0),
            bi_cost_m8 = ifelse(total_cost_8>0, 1, 0),
            bi_cost_m9 = ifelse(total_cost_9>0, 1, 0),
            bi_cost_m10 = ifelse(total_cost_10>0, 1, 0),
            bi_cost_m11 = ifelse(total_cost_11>0, 1, 0),
            bi_cost_m12 = ifelse(total_cost_12>0, 1, 0)
      )


# Function to tabulate the visit(0/1) and cost(0/1)

compare_bi_visit_cost_fn <- function(data, visits_m, cost_m, mont_n){
      visits_m <- rlang::sym(visits_m)
      cost_m <- rlang::sym(cost_m)
      
      result <- data %>% group_by(!!visits_m) %>% 
            summarise(have_cost = sum(!!cost_m==1, na.rm = TRUE),
                      no_cost = sum(!!cost_m==0, na.rm = TRUE)) %>% 
            mutate(month = mont_n, 
                   visit = ifelse(!!visits_m==1, "Yes", "No")) %>% 
            dplyr::select(month, visit, have_cost,no_cost)
      return(result)
}

# Initialize an empty list to store the results
exp_total <- list()

# Loop over the months
for(i in 1:12) {
      # Create the column names
      visits_m <- paste0("bi_visit_m", i)
      cost_m <- paste0("bi_cost_m", i)
      
      # Call the function and store the result in the list
      exp_total[[i]] <- compare_bi_visit_cost_fn(data = lc_exp_matched, 
                                             visits_m = visits_m, 
                                             cost_m = cost_m, 
                                             mont_n = i)
}

# Combine all data frames in the list
exp_total_compare <- bind_rows(total) %>% mutate(exposure = "Long COVID exposure")



# Comparator group:
com_matched <- com_matched %>%
      mutate(
            bi_visit_m1 = ifelse(all_month_m1>0, 1, 0),
            bi_visit_m2 = ifelse(all_month_m2>0, 1, 0),
            bi_visit_m3 = ifelse(all_month_m3>0, 1, 0),
            bi_visit_m4 = ifelse(all_month_m4>0, 1, 0),
            bi_visit_m5 = ifelse(all_month_m5>0, 1, 0),
            bi_visit_m6 = ifelse(all_month_m6>0, 1, 0),
            bi_visit_m7 = ifelse(all_month_m7>0, 1, 0),
            bi_visit_m8 = ifelse(all_month_m8>0, 1, 0),
            bi_visit_m9 = ifelse(all_month_m9>0, 1, 0),
            bi_visit_m10 = ifelse(all_month_m10>0, 1, 0),
            bi_visit_m11 = ifelse(all_month_m11>0, 1, 0),
            bi_visit_m12 = ifelse(all_month_m12>0, 1, 0)
      )


com_matched <- com_matched %>% 
      mutate(
            bi_cost_m1 = ifelse(total_cost_1>0, 1, 0),
            bi_cost_m2 = ifelse(total_cost_2>0, 1, 0),
            bi_cost_m3 = ifelse(total_cost_3>0, 1, 0),
            bi_cost_m4 = ifelse(total_cost_4>0, 1, 0),
            bi_cost_m5 = ifelse(total_cost_5>0, 1, 0),
            bi_cost_m6 = ifelse(total_cost_6>0, 1, 0),
            bi_cost_m7 = ifelse(total_cost_7>0, 1, 0),
            bi_cost_m8 = ifelse(total_cost_8>0, 1, 0),
            bi_cost_m9 = ifelse(total_cost_9>0, 1, 0),
            bi_cost_m10 = ifelse(total_cost_10>0, 1, 0),
            bi_cost_m11 = ifelse(total_cost_11>0, 1, 0),
            bi_cost_m12 = ifelse(total_cost_12>0, 1, 0)
      )


# Initialize an empty list to store the results
com_total <- list()

# Loop over the months
for(i in 1:12) {
      # Create the column names
      visits_m <- paste0("bi_visit_m", i)
      cost_m <- paste0("bi_cost_m", i)
      
      # Call the function and store the result in the list
      com_total[[i]] <- compare_bi_visit_cost_fn(data = lc_exp_matched, 
                                             visits_m = visits_m, 
                                             cost_m = cost_m, 
                                             mont_n = i)
}

# Combine all data frames in the list
com_total_compare <- bind_rows(com_total) %>% mutate(exposure = "Comparator")

# Line graph showing the trend of the inconsistent pair:

exp_inconsistent <- ggplot() + 
      geom_line(data = (filter(exp_total_compare, visit =="No")),
                aes(x = month, y = have_cost, color = visit)) +
      geom_line(data = (filter(exp_total_compare, visit =="Yes")),
                aes(x = month, y = no_cost, color = visit)) +
      ylab("Inconsistent pair counts") + xlab("Month") + theme_bw() + 
      scale_colour_discrete(labels=c("No visit but have costs", "Visited without costs")) +
      theme(legend.title = element_blank()) +
      scale_x_continuous(breaks = seq(1, 12))

com_inconsistent <- ggplot() + 
      geom_line(data = (filter(com_total_compare, visit =="No")),
                aes(x = month, y = have_cost, color = visit)) +
      geom_line(data = (filter(com_total_compare, visit =="Yes")),
                aes(x = month, y = no_cost, color = visit)) +
      ylab("Inconsistent pair counts") + xlab("Month") + theme_bw() +
      scale_colour_discrete(labels=c("No visit but have costs", "Visited without costs")) +
      theme(legend.title = element_blank()) +
      scale_x_continuous(breaks = seq(1, 12))

ggarrange(exp_inconsistent, com_inconsistent, common.legend = T,
          ncol = 2, labels = c("Long COVID group", "Comparator group")
          )
# save outputs:
ggsave(file = "output/qc09_non_zero_counts_comparison.png", width = 12, height = 4)
