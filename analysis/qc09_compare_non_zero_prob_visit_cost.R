source("analysis/dm04_02_combine_costs.R")

# Goal: Compare total healthcare visits vs total healthcare costs

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
# 1. Total visits comparison: ---------
# # Long COVID exposure group: -----

# create binomial outcomes in each months
lc_exp_matched <- lc_exp_matched %>%
      mutate(
            bi_visit_m1 = ifelse(all_month_m1>0 & !is.na(all_month_m1), 1, 0),
            bi_visit_m2 = ifelse(all_month_m2>0 & !is.na(all_month_m2), 1, 0),
            bi_visit_m3 = ifelse(all_month_m3>0 & !is.na(all_month_m3), 1, 0),
            bi_visit_m4 = ifelse(all_month_m4>0 & !is.na(all_month_m4), 1, 0),
            bi_visit_m5 = ifelse(all_month_m5>0 & !is.na(all_month_m5), 1, 0),
            bi_visit_m6 = ifelse(all_month_m6>0 & !is.na(all_month_m6), 1, 0),
            bi_visit_m7 = ifelse(all_month_m7>0 & !is.na(all_month_m7), 1, 0),
            bi_visit_m8 = ifelse(all_month_m8>0 & !is.na(all_month_m8), 1, 0),
            bi_visit_m9 = ifelse(all_month_m9>0 & !is.na(all_month_m9), 1, 0),
            bi_visit_m10 = ifelse(all_month_m10>0 & !is.na(all_month_m10), 1, 0),
            bi_visit_m11 = ifelse(all_month_m11>0 & !is.na(all_month_m11), 1, 0),
            bi_visit_m12 = ifelse(all_month_m12>0 & !is.na(all_month_m12), 1, 0)
      )


lc_exp_matched <- lc_exp_matched %>% 
      mutate(
            bi_cost_m1 = ifelse(total_cost_1>0 & !is.na(total_cost_1), 1, 0),
            bi_cost_m2 = ifelse(total_cost_2>0 & !is.na(total_cost_2), 1, 0),
            bi_cost_m3 = ifelse(total_cost_3>0 & !is.na(total_cost_3), 1, 0),
            bi_cost_m4 = ifelse(total_cost_4>0 & !is.na(total_cost_4), 1, 0),
            bi_cost_m5 = ifelse(total_cost_5>0 & !is.na(total_cost_5), 1, 0),
            bi_cost_m6 = ifelse(total_cost_6>0 & !is.na(total_cost_6), 1, 0),
            bi_cost_m7 = ifelse(total_cost_7>0 & !is.na(total_cost_7), 1, 0),
            bi_cost_m8 = ifelse(total_cost_8>0 & !is.na(total_cost_8), 1, 0),
            bi_cost_m9 = ifelse(total_cost_9>0 & !is.na(total_cost_9), 1, 0),
            bi_cost_m10 = ifelse(total_cost_10>0 & !is.na(total_cost_10), 1, 0),
            bi_cost_m11 = ifelse(total_cost_11>0 & !is.na(total_cost_11), 1, 0),
            bi_cost_m12 = ifelse(total_cost_12>0 & !is.na(total_cost_12), 1, 0)
      )


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
exp_total_compare <- bind_rows(exp_total) %>% mutate(exposure = "Long COVID exposure")

# combine all visits 
all_visit_12m <- c()
for(i in 1:12){
      all_visit_12m <- c(all_visit_12m, paste0("bi_visit_m", i))
}

lc_exp_matched$all_visit_12m <- rowSums(lc_exp_matched[,all_visit_12m],na.rm = T) # add them together
lc_exp_matched <-lc_exp_matched %>% mutate(all_visit_12m = ifelse(all_visit_12m>0, 1,0)) # recode

# combine all costs
all_cost_12m <- c()
for(i in 1:12){
      all_cost_12m <- c(all_cost_12m, paste0("bi_cost_m", i))
}
lc_exp_matched$all_cost_12m <- rowSums(lc_exp_matched[,all_cost_12m],na.rm = T) 
lc_exp_matched <- lc_exp_matched %>% mutate(all_cost_12m = ifelse(all_cost_12m>0, 1,0)) # recode


exp_12m_total <- compare_bi_visit_cost_fn(data = lc_exp_matched, 
                                          visits_m = "all_visit_12m", 
                                          cost_m = "all_cost_12m", 
                                          mont_n = "total 12 months") %>% 
      mutate(exposure = "Long COVID exposure")

# Combine outputs for saving later: 

exp_tabulate <- bind_rows(exp_total_compare %>% mutate(month = as.character(month)), 
          exp_12m_total)


# # Comparator group: ------
# Create binomial outcomes
com_matched <- com_matched %>%
      mutate(
            bi_visit_m1 = ifelse(all_month_m1>0 & !is.na(all_month_m1), 1, 0),
            bi_visit_m2 = ifelse(all_month_m2>0 & !is.na(all_month_m2), 1, 0),
            bi_visit_m3 = ifelse(all_month_m3>0 & !is.na(all_month_m3), 1, 0),
            bi_visit_m4 = ifelse(all_month_m4>0 & !is.na(all_month_m4), 1, 0),
            bi_visit_m5 = ifelse(all_month_m5>0 & !is.na(all_month_m5), 1, 0),
            bi_visit_m6 = ifelse(all_month_m6>0 & !is.na(all_month_m6), 1, 0),
            bi_visit_m7 = ifelse(all_month_m7>0 & !is.na(all_month_m7), 1, 0),
            bi_visit_m8 = ifelse(all_month_m8>0 & !is.na(all_month_m8), 1, 0),
            bi_visit_m9 = ifelse(all_month_m9>0 & !is.na(all_month_m9), 1, 0),
            bi_visit_m10 = ifelse(all_month_m10>0 & !is.na(all_month_m10), 1, 0),
            bi_visit_m11 = ifelse(all_month_m11>0 & !is.na(all_month_m11), 1, 0),
            bi_visit_m12 = ifelse(all_month_m12>0 & !is.na(all_month_m12), 1, 0)
      )


com_matched <- com_matched %>% 
      mutate(
            bi_cost_m1 = ifelse(total_cost_1>0 & !is.na(total_cost_1), 1, 0),
            bi_cost_m2 = ifelse(total_cost_2>0 & !is.na(total_cost_2), 1, 0),
            bi_cost_m3 = ifelse(total_cost_3>0 & !is.na(total_cost_3), 1, 0),
            bi_cost_m4 = ifelse(total_cost_4>0 & !is.na(total_cost_4), 1, 0),
            bi_cost_m5 = ifelse(total_cost_5>0 & !is.na(total_cost_5), 1, 0),
            bi_cost_m6 = ifelse(total_cost_6>0 & !is.na(total_cost_6), 1, 0),
            bi_cost_m7 = ifelse(total_cost_7>0 & !is.na(total_cost_7), 1, 0),
            bi_cost_m8 = ifelse(total_cost_8>0 & !is.na(total_cost_8), 1, 0),
            bi_cost_m9 = ifelse(total_cost_9>0 & !is.na(total_cost_9), 1, 0),
            bi_cost_m10 = ifelse(total_cost_10>0 & !is.na(total_cost_10), 1, 0),
            bi_cost_m11 = ifelse(total_cost_11>0 & !is.na(total_cost_11), 1, 0),
            bi_cost_m12 = ifelse(total_cost_12>0 & !is.na(total_cost_12), 1, 0)
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



# Combine visits and const in 12 months
# visits
com_matched$all_visit_12m <- rowSums(com_matched[,all_visit_12m],na.rm = T) # add them together
com_matched <-com_matched %>% mutate(all_visit_12m = ifelse(all_visit_12m>0, 1,0)) # recode

# costs
com_matched$all_cost_12m <- rowSums(com_matched[,all_cost_12m],na.rm = T) 
com_matched <- com_matched %>% mutate(all_cost_12m = ifelse(all_cost_12m>0, 1,0)) # recode


com_12m_total <- compare_bi_visit_cost_fn(data = com_matched, 
                                          visits_m = "all_visit_12m", 
                                          cost_m = "all_cost_12m", 
                                          mont_n = "total 12 months") %>% 
      mutate(exposure = "Comparator")

# Combine outputs for saving later: 

com_tabulate <- bind_rows(com_total_compare %>% mutate(month = as.character(month)), 
                          com_12m_total)


# Combine and saving
bind_rows(exp_tabulate, com_tabulate) %>% relocate(exposure) %>% 
      write_csv(here("output", "qc09_total_inconsistent_visit_cost.csv"))

# # Line graph showing the trend of the inconsistent pair: -----

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
          )%>% annotate_figure(top = text_grob("Total visits and costs comparison"))
# save outputs:
ggsave(file = "output/qc09_non_zero_counts_comparison.png", width = 12, height = 4)


# 2. GP visits comparisons -----

# # Long COVID exposure group: -----

# create binomial outcomes in each months
lc_exp_matched <- lc_exp_matched %>%
      mutate(
            bi_gp_visit_m1 = ifelse(gp_visit_m1>0 & !is.na(gp_visit_m1), 1, 0),
            bi_gp_visit_m2 = ifelse(gp_visit_m2>0 & !is.na(gp_visit_m2), 1, 0),
            bi_gp_visit_m3 = ifelse(gp_visit_m3>0 & !is.na(gp_visit_m3), 1, 0),
            bi_gp_visit_m4 = ifelse(gp_visit_m4>0 & !is.na(gp_visit_m4), 1, 0),
            bi_gp_visit_m5 = ifelse(gp_visit_m5>0 & !is.na(gp_visit_m5), 1, 0),
            bi_gp_visit_m6 = ifelse(gp_visit_m6>0 & !is.na(gp_visit_m6), 1, 0),
            bi_gp_visit_m7 = ifelse(gp_visit_m7>0 & !is.na(gp_visit_m7), 1, 0),
            bi_gp_visit_m8 = ifelse(gp_visit_m8>0 & !is.na(gp_visit_m8), 1, 0),
            bi_gp_visit_m9 = ifelse(gp_visit_m9>0 & !is.na(gp_visit_m9), 1, 0),
            bi_gp_visit_m10 = ifelse(gp_visit_m10>0 & !is.na(gp_visit_m10), 1, 0),
            bi_gp_visit_m11 = ifelse(gp_visit_m11>0 & !is.na(gp_visit_m11), 1, 0),
            bi_gp_visit_m12 = ifelse(gp_visit_m12>0 & !is.na(gp_visit_m12), 1, 0)
      )

lc_exp_matched <- lc_exp_matched %>% 
      mutate(
            bi_gp_cost_m1 = ifelse(primary_cost_1>0 & !is.na(primary_cost_1), 1, 0),
            bi_gp_cost_m2 = ifelse(primary_cost_2>0 & !is.na(primary_cost_2), 1, 0),
            bi_gp_cost_m3 = ifelse(primary_cost_3>0 & !is.na(primary_cost_3), 1, 0),
            bi_gp_cost_m4 = ifelse(primary_cost_4>0 & !is.na(primary_cost_4), 1, 0),
            bi_gp_cost_m5 = ifelse(primary_cost_5>0 & !is.na(primary_cost_5), 1, 0),
            bi_gp_cost_m6 = ifelse(primary_cost_6>0 & !is.na(primary_cost_6), 1, 0),
            bi_gp_cost_m7 = ifelse(primary_cost_7>0 & !is.na(primary_cost_7), 1, 0),
            bi_gp_cost_m8 = ifelse(primary_cost_8>0 & !is.na(primary_cost_8), 1, 0),
            bi_gp_cost_m9 = ifelse(primary_cost_9>0 & !is.na(primary_cost_9), 1, 0),
            bi_gp_cost_m10 = ifelse(primary_cost_10>0 & !is.na(primary_cost_10), 1, 0),
            bi_gp_cost_m11 = ifelse(primary_cost_11>0 & !is.na(primary_cost_11), 1, 0),
            bi_gp_cost_m12 = ifelse(primary_cost_12>0 & !is.na(primary_cost_12), 1, 0)
      )


# vectors of gp visits and costs

# Initialize an empty list to store the results
exp_gp <- list()

# Loop over the months
for(i in 1:12) {
      # Create the column names
      gp_visits_m <- paste0("bi_gp_visit_m", i)
      gp_cost_m <- paste0("bi_gp_cost_m", i)
      
      # Call the function and store the result in the list
      exp_gp[[i]] <- compare_bi_visit_cost_fn(data = lc_exp_matched, 
                                                 visits_m = gp_visits_m, 
                                                 cost_m = gp_cost_m, 
                                                 mont_n = i)
}

# Combine all data frames in the list
exp_gp_compare <- bind_rows(exp_gp) %>% mutate(exposure = "Long COVID exposure")


# combine all gp visits in 12 months
gp_visit_12m <- c()
for(i in 1:12){
      gp_visit_12m <- c(gp_visit_12m, paste0("bi_gp_visit_m", i))
}

lc_exp_matched$gp_visit_12m <- rowSums(lc_exp_matched[,gp_visit_12m],na.rm = T) # add them together
lc_exp_matched <-lc_exp_matched %>% mutate(gp_visit_12m = ifelse(gp_visit_12m>0, 1,0)) # recode

# combine all costs
gp_cost_12m <- c()
for(i in 1:12){
      gp_cost_12m <- c(gp_cost_12m, paste0("bi_gp_cost_m", i))
}
lc_exp_matched$gp_cost_12m <- rowSums(lc_exp_matched[,gp_cost_12m],na.rm = T) 
lc_exp_matched <- lc_exp_matched %>% mutate(gp_cost_12m = ifelse(gp_cost_12m>0, 1,0)) # recode


exp_12m_gp <- compare_bi_visit_cost_fn(data = lc_exp_matched, 
                                          visits_m = "gp_visit_12m", 
                                          cost_m = "gp_cost_12m", 
                                          mont_n = "total 12 months") %>% 
      mutate(exposure = "Long COVID exposure")

# Combine outputs for saving later: 

exp_gp_tabulate <- bind_rows(exp_gp_compare %>% mutate(month = as.character(month)), 
                             exp_12m_gp)


# # Comparator: ----
# create binomial outcomes in each months
com_matched <- com_matched %>%
      mutate(
            bi_gp_visit_m1 = ifelse(gp_visit_m1>0 & !is.na(gp_visit_m1), 1, 0),
            bi_gp_visit_m2 = ifelse(gp_visit_m2>0 & !is.na(gp_visit_m2), 1, 0),
            bi_gp_visit_m3 = ifelse(gp_visit_m3>0 & !is.na(gp_visit_m3), 1, 0),
            bi_gp_visit_m4 = ifelse(gp_visit_m4>0 & !is.na(gp_visit_m4), 1, 0),
            bi_gp_visit_m5 = ifelse(gp_visit_m5>0 & !is.na(gp_visit_m5), 1, 0),
            bi_gp_visit_m6 = ifelse(gp_visit_m6>0 & !is.na(gp_visit_m6), 1, 0),
            bi_gp_visit_m7 = ifelse(gp_visit_m7>0 & !is.na(gp_visit_m7), 1, 0),
            bi_gp_visit_m8 = ifelse(gp_visit_m8>0 & !is.na(gp_visit_m8), 1, 0),
            bi_gp_visit_m9 = ifelse(gp_visit_m9>0 & !is.na(gp_visit_m9), 1, 0),
            bi_gp_visit_m10 = ifelse(gp_visit_m10>0 & !is.na(gp_visit_m10), 1, 0),
            bi_gp_visit_m11 = ifelse(gp_visit_m11>0 & !is.na(gp_visit_m11), 1, 0),
            bi_gp_visit_m12 = ifelse(gp_visit_m12>0 & !is.na(gp_visit_m12), 1, 0)
      )

com_matched <- com_matched %>% 
      mutate(
            bi_gp_cost_m1 = ifelse(primary_cost_1>0 & !is.na(primary_cost_1), 1, 0),
            bi_gp_cost_m2 = ifelse(primary_cost_2>0 & !is.na(primary_cost_2), 1, 0),
            bi_gp_cost_m3 = ifelse(primary_cost_3>0 & !is.na(primary_cost_3), 1, 0),
            bi_gp_cost_m4 = ifelse(primary_cost_4>0 & !is.na(primary_cost_4), 1, 0),
            bi_gp_cost_m5 = ifelse(primary_cost_5>0 & !is.na(primary_cost_5), 1, 0),
            bi_gp_cost_m6 = ifelse(primary_cost_6>0 & !is.na(primary_cost_6), 1, 0),
            bi_gp_cost_m7 = ifelse(primary_cost_7>0 & !is.na(primary_cost_7), 1, 0),
            bi_gp_cost_m8 = ifelse(primary_cost_8>0 & !is.na(primary_cost_8), 1, 0),
            bi_gp_cost_m9 = ifelse(primary_cost_9>0 & !is.na(primary_cost_9), 1, 0),
            bi_gp_cost_m10 = ifelse(primary_cost_10>0 & !is.na(primary_cost_10), 1, 0),
            bi_gp_cost_m11 = ifelse(primary_cost_11>0 & !is.na(primary_cost_11), 1, 0),
            bi_gp_cost_m12 = ifelse(primary_cost_12>0 & !is.na(primary_cost_12), 1, 0)
      )




# Initialize an empty list to store the results
com_gp <- list()

# Loop over the months
for(i in 1:12) {
      # Create the column names
      gp_visits_m <- paste0("bi_gp_visit_m", i)
      gp_cost_m <- paste0("bi_gp_cost_m", i)
      
      # Call the function and store the result in the list
      com_gp[[i]] <- compare_bi_visit_cost_fn(data = com_matched, 
                                              visits_m = gp_visits_m, 
                                              cost_m = gp_cost_m, 
                                              mont_n = i)
}

# Combine all data frames in the list
com_gp_compare <- bind_rows(com_gp) %>% mutate(exposure = "Comparator")



# combine all gp visits in 12 months
com_matched$gp_visit_12m <- rowSums(com_matched[,gp_visit_12m],na.rm = T) # add them together
com_matched <-com_matched %>% mutate(gp_visit_12m = ifelse(gp_visit_12m>0, 1,0)) # recode

# combine all costs
com_matched$gp_cost_12m <- rowSums(com_matched[,gp_cost_12m],na.rm = T) 
com_matched <- com_matched %>% mutate(gp_cost_12m = ifelse(gp_cost_12m>0, 1,0)) # recode


com_12m_gp <- compare_bi_visit_cost_fn(data = com_matched, 
                                       visits_m = "gp_visit_12m", 
                                       cost_m = "gp_cost_12m", 
                                       mont_n = "total 12 months") %>% 
      mutate(exposure = "Comparator")

# Combine outputs for saving later: 

com_gp_tabulate <- bind_rows(com_gp_compare %>% mutate(month = as.character(month)), 
                             com_12m_gp)

# Combine and saving
bind_rows(exp_gp_tabulate, com_gp_tabulate) %>% relocate(exposure) %>% 
      write_csv(here("output", "qc09_gp_inconsistent_visit_cost.csv"))

# # Line graph showing the trend of the inconsistent pair: -----

exp_gp_inconsistent <- ggplot() + 
      geom_line(data = (filter(exp_gp_compare, visit =="No")),
                aes(x = month, y = have_cost, color = visit)) +
      geom_line(data = (filter(exp_gp_compare, visit =="Yes")),
                aes(x = month, y = no_cost, color = visit)) +
      ylab("Inconsistent pair counts") + xlab("Month") + theme_bw() + 
      scale_colour_discrete(labels=c("No visit but have costs", "Visited without costs")) +
      theme(legend.title = element_blank()) +
      scale_x_continuous(breaks = seq(1, 12))

com_gp_inconsistent <- ggplot() + 
      geom_line(data = (filter(com_gp_compare, visit =="No")),
                aes(x = month, y = have_cost, color = visit)) +
      geom_line(data = (filter(com_gp_compare, visit =="Yes")),
                aes(x = month, y = no_cost, color = visit)) +
      ylab("Inconsistent pair counts") + xlab("Month") + theme_bw() +
      scale_colour_discrete(labels=c("No visit but have costs", "Visited without costs")) +
      theme(legend.title = element_blank()) +
      scale_x_continuous(breaks = seq(1, 12))

ggarrange(exp_gp_inconsistent, com_gp_inconsistent, common.legend = T,
          ncol = 2, labels = c("Long COVID group", "Comparator group")
)%>% annotate_figure(top = text_grob("GP visits and costs comparison"))
# save outputs:
ggsave(file = "output/qc09_gp_non_zero_counts_comparison.png", width = 12, height = 4)


# 3. Hospitalisation comparison: -------

# # Long COVID exposure group: -----

# create binomial outcomes in each months
lc_exp_matched <- lc_exp_matched %>%
      mutate(
            bi_hos_visit_m1 = ifelse(hos_visit_m1>0 & !is.na(hos_visit_m1), 1, 0),
            bi_hos_visit_m2 = ifelse(hos_visit_m2>0 & !is.na(hos_visit_m2), 1, 0),
            bi_hos_visit_m3 = ifelse(hos_visit_m3>0 & !is.na(hos_visit_m3), 1, 0),
            bi_hos_visit_m4 = ifelse(hos_visit_m4>0 & !is.na(hos_visit_m4), 1, 0),
            bi_hos_visit_m5 = ifelse(hos_visit_m5>0 & !is.na(hos_visit_m5), 1, 0),
            bi_hos_visit_m6 = ifelse(hos_visit_m6>0 & !is.na(hos_visit_m6), 1, 0),
            bi_hos_visit_m7 = ifelse(hos_visit_m7>0 & !is.na(hos_visit_m7), 1, 0),
            bi_hos_visit_m8 = ifelse(hos_visit_m8>0 & !is.na(hos_visit_m8), 1, 0),
            bi_hos_visit_m9 = ifelse(hos_visit_m9>0 & !is.na(hos_visit_m9), 1, 0),
            bi_hos_visit_m10 = ifelse(hos_visit_m10>0 & !is.na(hos_visit_m10), 1, 0),
            bi_hos_visit_m11 = ifelse(hos_visit_m11>0 & !is.na(hos_visit_m11), 1, 0),
            bi_hos_visit_m12 = ifelse(hos_visit_m12>0 & !is.na(hos_visit_m12), 1, 0)
      )

lc_exp_matched <- lc_exp_matched %>% 
      mutate(
            bi_hos_cost_m1 = ifelse(apc_cost_m1>0 & !is.na(apc_cost_m1), 1, 0),
            bi_hos_cost_m2 = ifelse(apc_cost_m2>0 & !is.na(apc_cost_m2), 1, 0),
            bi_hos_cost_m3 = ifelse(apc_cost_m3>0 & !is.na(apc_cost_m3), 1, 0),
            bi_hos_cost_m4 = ifelse(apc_cost_m4>0 & !is.na(apc_cost_m4), 1, 0),
            bi_hos_cost_m5 = ifelse(apc_cost_m5>0 & !is.na(apc_cost_m5), 1, 0),
            bi_hos_cost_m6 = ifelse(apc_cost_m6>0 & !is.na(apc_cost_m6), 1, 0),
            bi_hos_cost_m7 = ifelse(apc_cost_m7>0 & !is.na(apc_cost_m7), 1, 0),
            bi_hos_cost_m8 = ifelse(apc_cost_m8>0 & !is.na(apc_cost_m8), 1, 0),
            bi_hos_cost_m9 = ifelse(apc_cost_m9>0 & !is.na(apc_cost_m9), 1, 0),
            bi_hos_cost_m10 = ifelse(apc_cost_m10>0 & !is.na(apc_cost_m10), 1, 0),
            bi_hos_cost_m11 = ifelse(apc_cost_m11>0 & !is.na(apc_cost_m11), 1, 0),
            bi_hos_cost_m12 = ifelse(apc_cost_m12>0 & !is.na(apc_cost_m12), 1, 0)
      )


# vectors of hos visits and costs

# Initialize an empty list to store the results
exp_hos <- list()

# Loop over the months
for(i in 1:12) {
      # Create the column names
      hos_visits_m <- paste0("bi_hos_visit_m", i)
      hos_cost_m <- paste0("bi_hos_cost_m", i)
      
      # Call the function and store the result in the list
      exp_hos[[i]] <- compare_bi_visit_cost_fn(data = lc_exp_matched, 
                                               visits_m = hos_visits_m, 
                                               cost_m = hos_cost_m, 
                                               mont_n = i)
}

# Combine all data frames in the list
exp_hos_compare <- bind_rows(exp_hos) %>% mutate(exposure = "Long COVID exposure")


# combine all hos visits in 12 months
hos_visit_12m <- c()
for(i in 1:12){
      hos_visit_12m <- c(hos_visit_12m, paste0("bi_hos_visit_m", i))
}

lc_exp_matched$hos_visit_12m <- rowSums(lc_exp_matched[,hos_visit_12m],na.rm = T) # add them together
lc_exp_matched <-lc_exp_matched %>% mutate(hos_visit_12m = ifelse(hos_visit_12m>0, 1,0)) # recode

# combine all costs
hos_cost_12m <- c()
for(i in 1:12){
      hos_cost_12m <- c(hos_cost_12m, paste0("bi_hos_cost_m", i))
}
lc_exp_matched$hos_cost_12m <- rowSums(lc_exp_matched[,hos_cost_12m],na.rm = T) 
lc_exp_matched <- lc_exp_matched %>% mutate(hos_cost_12m = ifelse(hos_cost_12m>0, 1,0)) # recode


exp_12m_hos <- compare_bi_visit_cost_fn(data = lc_exp_matched, 
                                        visits_m = "hos_visit_12m", 
                                        cost_m = "hos_cost_12m", 
                                        mont_n = "total 12 months") %>% 
      mutate(exposure = "Long COVID exposure")

# Combine outputs for saving later: 

exp_hos_tabulate <- bind_rows(exp_hos_compare %>% mutate(month = as.character(month)), 
                              exp_12m_hos)




# # Comparator: ----
# create binomial outcomes in each months
com_matched <- com_matched %>%
      mutate(
            bi_hos_visit_m1 = ifelse(hos_visit_m1>0 & !is.na(hos_visit_m1), 1, 0),
            bi_hos_visit_m2 = ifelse(hos_visit_m2>0 & !is.na(hos_visit_m2), 1, 0),
            bi_hos_visit_m3 = ifelse(hos_visit_m3>0 & !is.na(hos_visit_m3), 1, 0),
            bi_hos_visit_m4 = ifelse(hos_visit_m4>0 & !is.na(hos_visit_m4), 1, 0),
            bi_hos_visit_m5 = ifelse(hos_visit_m5>0 & !is.na(hos_visit_m5), 1, 0),
            bi_hos_visit_m6 = ifelse(hos_visit_m6>0 & !is.na(hos_visit_m6), 1, 0),
            bi_hos_visit_m7 = ifelse(hos_visit_m7>0 & !is.na(hos_visit_m7), 1, 0),
            bi_hos_visit_m8 = ifelse(hos_visit_m8>0 & !is.na(hos_visit_m8), 1, 0),
            bi_hos_visit_m9 = ifelse(hos_visit_m9>0 & !is.na(hos_visit_m9), 1, 0),
            bi_hos_visit_m10 = ifelse(hos_visit_m10>0 & !is.na(hos_visit_m10), 1, 0),
            bi_hos_visit_m11 = ifelse(hos_visit_m11>0 & !is.na(hos_visit_m11), 1, 0),
            bi_hos_visit_m12 = ifelse(hos_visit_m12>0 & !is.na(hos_visit_m12), 1, 0)
      )


com_matched <- com_matched %>% 
      mutate(
            bi_hos_cost_m1 = ifelse(apc_cost_m1>0 & !is.na(apc_cost_m1), 1, 0),
            bi_hos_cost_m2 = ifelse(apc_cost_m2>0 & !is.na(apc_cost_m2), 1, 0),
            bi_hos_cost_m3 = ifelse(apc_cost_m3>0 & !is.na(apc_cost_m3), 1, 0),
            bi_hos_cost_m4 = ifelse(apc_cost_m4>0 & !is.na(apc_cost_m4), 1, 0),
            bi_hos_cost_m5 = ifelse(apc_cost_m5>0 & !is.na(apc_cost_m5), 1, 0),
            bi_hos_cost_m6 = ifelse(apc_cost_m6>0 & !is.na(apc_cost_m6), 1, 0),
            bi_hos_cost_m7 = ifelse(apc_cost_m7>0 & !is.na(apc_cost_m7), 1, 0),
            bi_hos_cost_m8 = ifelse(apc_cost_m8>0 & !is.na(apc_cost_m8), 1, 0),
            bi_hos_cost_m9 = ifelse(apc_cost_m9>0 & !is.na(apc_cost_m9), 1, 0),
            bi_hos_cost_m10 = ifelse(apc_cost_m10>0 & !is.na(apc_cost_m10), 1, 0),
            bi_hos_cost_m11 = ifelse(apc_cost_m11>0 & !is.na(apc_cost_m11), 1, 0),
            bi_hos_cost_m12 = ifelse(apc_cost_m12>0 & !is.na(apc_cost_m12), 1, 0)
      )


# Initialize an empty list to store the results
com_hos <- list()

# Loop over the months
for(i in 1:12) {
      # Create the column names
      hos_visits_m <- paste0("bi_hos_visit_m", i)
      hos_cost_m <- paste0("bi_hos_cost_m", i)
      
      # Call the function and store the result in the list
      com_hos[[i]] <- compare_bi_visit_cost_fn(data = com_matched, 
                                               visits_m = hos_visits_m, 
                                               cost_m = hos_cost_m, 
                                               mont_n = i)
}

# Combine all data frames in the list
com_hos_compare <- bind_rows(com_hos) %>% mutate(exposure = "Comparator")



# combine all hos visits in 12 months
com_matched$hos_visit_12m <- rowSums(com_matched[,hos_visit_12m],na.rm = T) # add them together
com_matched <-com_matched %>% mutate(hos_visit_12m = ifelse(hos_visit_12m>0, 1,0)) # recode

# combine all costs
com_matched$hos_cost_12m <- rowSums(com_matched[,hos_cost_12m],na.rm = T) 
com_matched <- com_matched %>% mutate(hos_cost_12m = ifelse(hos_cost_12m>0, 1,0)) # recode


com_12m_hos <- compare_bi_visit_cost_fn(data = com_matched, 
                                        visits_m = "hos_visit_12m", 
                                        cost_m = "hos_cost_12m", 
                                        mont_n = "total 12 months") %>% 
      mutate(exposure = "Comparator")

# Combine outputs for saving later: 

com_hos_tabulate <- bind_rows(com_hos_compare %>% mutate(month = as.character(month)), 
                              com_12m_hos)


# Combine and saving
bind_rows(exp_hos_tabulate, com_hos_tabulate) %>% relocate(exposure) %>% 
      write_csv(here("output", "qc09_hos_inconsistent_visit_cost.csv"))

# # Line graph showing the trend of the inconsistent pair: -----
exp_hos_inconsistent <- ggplot() + 
      geom_line(data = (filter(exp_hos_compare, visit =="No")),
                aes(x = month, y = have_cost, color = visit)) +
      geom_line(data = (filter(exp_hos_compare, visit =="Yes")),
                aes(x = month, y = no_cost, color = visit)) +
      ylab("Inconsistent pair counts") + xlab("Month") + theme_bw() + 
      scale_colour_discrete(labels=c("No visit but have costs", "Visited without costs")) +
      theme(legend.title = element_blank()) +
      scale_x_continuous(breaks = seq(1, 12))

com_hos_inconsistent <- ggplot() + 
      geom_line(data = (filter(com_hos_compare, visit =="No")),
                aes(x = month, y = have_cost, color = visit)) +
      geom_line(data = (filter(com_hos_compare, visit =="Yes")),
                aes(x = month, y = no_cost, color = visit)) +
      ylab("Inconsistent pair counts") + xlab("Month") + theme_bw() +
      scale_colour_discrete(labels=c("No visit but have costs", "Visited without costs")) +
      theme(legend.title = element_blank()) +
      scale_x_continuous(breaks = seq(1, 12))

ggarrange(exp_hos_inconsistent, com_hos_inconsistent, common.legend = T,
          ncol = 2, labels = c("Long COVID group", "Comparator group")
)%>% annotate_figure(top = text_grob("Hospital admission and costs comparison"))
# save outputs:
ggsave(file = "output/qc09_hos_non_zero_counts_comparison.png", width = 12, height = 4)


# 4. A&E visit/costs: ----------


# # Long COVID exposure group: -----

# create binomial outcomes in each months
lc_exp_matched <- lc_exp_matched %>%
      mutate(
            bi_ae_visit_m1 = ifelse(ae_visit_m1>0 & !is.na(ae_visit_m1), 1, 0),
            bi_ae_visit_m2 = ifelse(ae_visit_m2>0 & !is.na(ae_visit_m2), 1, 0),
            bi_ae_visit_m3 = ifelse(ae_visit_m3>0 & !is.na(ae_visit_m3), 1, 0),
            bi_ae_visit_m4 = ifelse(ae_visit_m4>0 & !is.na(ae_visit_m4), 1, 0),
            bi_ae_visit_m5 = ifelse(ae_visit_m5>0 & !is.na(ae_visit_m5), 1, 0),
            bi_ae_visit_m6 = ifelse(ae_visit_m6>0 & !is.na(ae_visit_m6), 1, 0),
            bi_ae_visit_m7 = ifelse(ae_visit_m7>0 & !is.na(ae_visit_m7), 1, 0),
            bi_ae_visit_m8 = ifelse(ae_visit_m8>0 & !is.na(ae_visit_m8), 1, 0),
            bi_ae_visit_m9 = ifelse(ae_visit_m9>0 & !is.na(ae_visit_m9), 1, 0),
            bi_ae_visit_m10 = ifelse(ae_visit_m10>0 & !is.na(ae_visit_m10), 1, 0),
            bi_ae_visit_m11 = ifelse(ae_visit_m11>0 & !is.na(ae_visit_m11), 1, 0),
            bi_ae_visit_m12 = ifelse(ae_visit_m12>0 & !is.na(ae_visit_m12), 1, 0)
      )

lc_exp_matched <- lc_exp_matched %>% 
      mutate(
            bi_ae_cost_m1 = ifelse(er_cost_m1>0 & !is.na(er_cost_m1), 1, 0),
            bi_ae_cost_m2 = ifelse(er_cost_m2>0 & !is.na(er_cost_m2), 1, 0),
            bi_ae_cost_m3 = ifelse(er_cost_m3>0 & !is.na(er_cost_m3), 1, 0),
            bi_ae_cost_m4 = ifelse(er_cost_m4>0 & !is.na(er_cost_m4), 1, 0),
            bi_ae_cost_m5 = ifelse(er_cost_m5>0 & !is.na(er_cost_m5), 1, 0),
            bi_ae_cost_m6 = ifelse(er_cost_m6>0 & !is.na(er_cost_m6), 1, 0),
            bi_ae_cost_m7 = ifelse(er_cost_m7>0 & !is.na(er_cost_m7), 1, 0),
            bi_ae_cost_m8 = ifelse(er_cost_m8>0 & !is.na(er_cost_m8), 1, 0),
            bi_ae_cost_m9 = ifelse(er_cost_m9>0 & !is.na(er_cost_m9), 1, 0),
            bi_ae_cost_m10 = ifelse(er_cost_m10>0 & !is.na(er_cost_m10), 1, 0),
            bi_ae_cost_m11 = ifelse(er_cost_m11>0 & !is.na(er_cost_m11), 1, 0),
            bi_ae_cost_m12 = ifelse(er_cost_m12>0 & !is.na(er_cost_m12), 1, 0)
      )

# vectors of ae visits and costs

# Initialize an empty list to store the results
exp_ae <- list()

# Loop over the months
for(i in 1:12) {
      # Create the column names
      ae_visits_m <- paste0("bi_ae_visit_m", i)
      ae_cost_m <- paste0("bi_ae_cost_m", i)
      
      # Call the function and store the result in the list
      exp_ae[[i]] <- compare_bi_visit_cost_fn(data = lc_exp_matched, 
                                              visits_m = ae_visits_m, 
                                              cost_m = ae_cost_m, 
                                              mont_n = i)
}

# Combine all data frames in the list
exp_ae_compare <- bind_rows(exp_ae) %>% mutate(exposure = "Long COVID exposure")


# combine all ae visits in 12 months
ae_visit_12m <- c()
for(i in 1:12){
      ae_visit_12m <- c(ae_visit_12m, paste0("bi_ae_visit_m", i))
}

lc_exp_matched$ae_visit_12m <- rowSums(lc_exp_matched[,ae_visit_12m],na.rm = T) # add them together
lc_exp_matched <-lc_exp_matched %>% mutate(ae_visit_12m = ifelse(ae_visit_12m>0, 1,0)) # recode

# combine all costs
ae_cost_12m <- c()
for(i in 1:12){
      ae_cost_12m <- c(ae_cost_12m, paste0("bi_ae_cost_m", i))
}
lc_exp_matched$ae_cost_12m <- rowSums(lc_exp_matched[,ae_cost_12m],na.rm = T) 
lc_exp_matched <- lc_exp_matched %>% mutate(ae_cost_12m = ifelse(ae_cost_12m>0, 1,0)) # recode


exp_12m_ae <- compare_bi_visit_cost_fn(data = lc_exp_matched, 
                                       visits_m = "ae_visit_12m", 
                                       cost_m = "ae_cost_12m", 
                                       mont_n = "total 12 months") %>% 
      mutate(exposure = "Long COVID exposure")

# Combine outputs for saving later: 

exp_ae_tabulate <- bind_rows(exp_ae_compare %>% mutate(month = as.character(month)), 
                             exp_12m_ae)




# # Comparator: ----
# create binomial outcomes in each months
com_matched <- com_matched %>%
      mutate(
            bi_ae_visit_m1 = ifelse(ae_visit_m1>0 & !is.na(ae_visit_m1), 1, 0),
            bi_ae_visit_m2 = ifelse(ae_visit_m2>0 & !is.na(ae_visit_m2), 1, 0),
            bi_ae_visit_m3 = ifelse(ae_visit_m3>0 & !is.na(ae_visit_m3), 1, 0),
            bi_ae_visit_m4 = ifelse(ae_visit_m4>0 & !is.na(ae_visit_m4), 1, 0),
            bi_ae_visit_m5 = ifelse(ae_visit_m5>0 & !is.na(ae_visit_m5), 1, 0),
            bi_ae_visit_m6 = ifelse(ae_visit_m6>0 & !is.na(ae_visit_m6), 1, 0),
            bi_ae_visit_m7 = ifelse(ae_visit_m7>0 & !is.na(ae_visit_m7), 1, 0),
            bi_ae_visit_m8 = ifelse(ae_visit_m8>0 & !is.na(ae_visit_m8), 1, 0),
            bi_ae_visit_m9 = ifelse(ae_visit_m9>0 & !is.na(ae_visit_m9), 1, 0),
            bi_ae_visit_m10 = ifelse(ae_visit_m10>0 & !is.na(ae_visit_m10), 1, 0),
            bi_ae_visit_m11 = ifelse(ae_visit_m11>0 & !is.na(ae_visit_m11), 1, 0),
            bi_ae_visit_m12 = ifelse(ae_visit_m12>0 & !is.na(ae_visit_m12), 1, 0)
      )

com_matched <- com_matched %>% 
      mutate(
            bi_ae_cost_m1 = ifelse(er_cost_m1>0 & !is.na(er_cost_m1), 1, 0),
            bi_ae_cost_m2 = ifelse(er_cost_m2>0 & !is.na(er_cost_m2), 1, 0),
            bi_ae_cost_m3 = ifelse(er_cost_m3>0 & !is.na(er_cost_m3), 1, 0),
            bi_ae_cost_m4 = ifelse(er_cost_m4>0 & !is.na(er_cost_m4), 1, 0),
            bi_ae_cost_m5 = ifelse(er_cost_m5>0 & !is.na(er_cost_m5), 1, 0),
            bi_ae_cost_m6 = ifelse(er_cost_m6>0 & !is.na(er_cost_m6), 1, 0),
            bi_ae_cost_m7 = ifelse(er_cost_m7>0 & !is.na(er_cost_m7), 1, 0),
            bi_ae_cost_m8 = ifelse(er_cost_m8>0 & !is.na(er_cost_m8), 1, 0),
            bi_ae_cost_m9 = ifelse(er_cost_m9>0 & !is.na(er_cost_m9), 1, 0),
            bi_ae_cost_m10 = ifelse(er_cost_m10>0 & !is.na(er_cost_m10), 1, 0),
            bi_ae_cost_m11 = ifelse(er_cost_m11>0 & !is.na(er_cost_m11), 1, 0),
            bi_ae_cost_m12 = ifelse(er_cost_m12>0 & !is.na(er_cost_m12), 1, 0)
      )



# Initialize an empty list to store the results
com_ae <- list()

# Loop over the months
for(i in 1:12) {
      # Create the column names
      ae_visits_m <- paste0("bi_ae_visit_m", i)
      ae_cost_m <- paste0("bi_ae_cost_m", i)
      
      # Call the function and store the result in the list
      com_ae[[i]] <- compare_bi_visit_cost_fn(data = com_matched, 
                                              visits_m = ae_visits_m, 
                                              cost_m = ae_cost_m, 
                                              mont_n = i)
}

# Combine all data frames in the list
com_ae_compare <- bind_rows(com_ae) %>% mutate(exposure = "Comparator")



# combine all ae visits in 12 months
com_matched$ae_visit_12m <- rowSums(com_matched[,ae_visit_12m],na.rm = T) # add them together
com_matched <-com_matched %>% mutate(ae_visit_12m = ifelse(ae_visit_12m>0, 1,0)) # recode

# combine all costs
com_matched$ae_cost_12m <- rowSums(com_matched[,ae_cost_12m],na.rm = T) 
com_matched <- com_matched %>% mutate(ae_cost_12m = ifelse(ae_cost_12m>0, 1,0)) # recode


com_12m_ae <- compare_bi_visit_cost_fn(data = com_matched, 
                                       visits_m = "ae_visit_12m", 
                                       cost_m = "ae_cost_12m", 
                                       mont_n = "total 12 months") %>% 
      mutate(exposure = "Comparator")

# Combine outputs for saving later: 

com_ae_tabulate <- bind_rows(com_ae_compare %>% mutate(month = as.character(month)), 
                             com_12m_ae)


# Combine and saving
bind_rows(exp_ae_tabulate, com_ae_tabulate) %>% relocate(exposure) %>% 
      write_csv(here("output", "qc09_ae_inconsistent_visit_cost.csv"))

# # Line graph showing the trend of the inconsistent pair: -----
exp_ae_inconsistent <- ggplot() + 
      geom_line(data = (filter(exp_ae_compare, visit =="No")),
                aes(x = month, y = have_cost, color = visit)) +
      geom_line(data = (filter(exp_ae_compare, visit =="Yes")),
                aes(x = month, y = no_cost, color = visit)) +
      ylab("Inconsistent pair counts") + xlab("Month") + theme_bw() + 
      scale_colour_discrete(labels=c("No visit but have costs", "Visited without costs")) +
      theme(legend.title = element_blank()) +
      scale_x_continuous(breaks = seq(1, 12))

com_ae_inconsistent <- ggplot() + 
      geom_line(data = (filter(com_ae_compare, visit =="No")),
                aes(x = month, y = have_cost, color = visit)) +
      geom_line(data = (filter(com_ae_compare, visit =="Yes")),
                aes(x = month, y = no_cost, color = visit)) +
      ylab("Inconsistent pair counts") + xlab("Month") + theme_bw() +
      scale_colour_discrete(labels=c("No visit but have costs", "Visited without costs")) +
      theme(legend.title = element_blank()) +
      scale_x_continuous(breaks = seq(1, 12))

ggarrange(exp_ae_inconsistent, com_ae_inconsistent, common.legend = T,
          ncol = 2, labels = c("Long COVID group", "Comparator group")
) %>% annotate_figure(top = text_grob("A&E visits and costs comparison"))
# save outputs:
ggsave(file = "output/qc09_ae_non_zero_counts_comparison.png", width = 12, height = 4)

# 5. Outpatient clinic visit/costs -----
# # Long COVID exposure group: -----

# create binomial outcomes in each months
lc_exp_matched <- lc_exp_matched %>%
      mutate(
            bi_opa_visit_m1 = ifelse(opa_visit_m1>0 & !is.na(opa_visit_m1), 1, 0),
            bi_opa_visit_m2 = ifelse(opa_visit_m2>0 & !is.na(opa_visit_m2), 1, 0),
            bi_opa_visit_m3 = ifelse(opa_visit_m3>0 & !is.na(opa_visit_m3), 1, 0),
            bi_opa_visit_m4 = ifelse(opa_visit_m4>0 & !is.na(opa_visit_m4), 1, 0),
            bi_opa_visit_m5 = ifelse(opa_visit_m5>0 & !is.na(opa_visit_m5), 1, 0),
            bi_opa_visit_m6 = ifelse(opa_visit_m6>0 & !is.na(opa_visit_m6), 1, 0),
            bi_opa_visit_m7 = ifelse(opa_visit_m7>0 & !is.na(opa_visit_m7), 1, 0),
            bi_opa_visit_m8 = ifelse(opa_visit_m8>0 & !is.na(opa_visit_m8), 1, 0),
            bi_opa_visit_m9 = ifelse(opa_visit_m9>0 & !is.na(opa_visit_m9), 1, 0),
            bi_opa_visit_m10 = ifelse(opa_visit_m10>0 & !is.na(opa_visit_m10), 1, 0),
            bi_opa_visit_m11 = ifelse(opa_visit_m11>0 & !is.na(opa_visit_m11), 1, 0),
            bi_opa_visit_m12 = ifelse(opa_visit_m12>0 & !is.na(opa_visit_m12), 1, 0)
      )

lc_exp_matched <- lc_exp_matched %>% 
      mutate(
            bi_opa_cost_m1 = ifelse(opd_cost_m1>0 & !is.na(opd_cost_m1), 1, 0),
            bi_opa_cost_m2 = ifelse(opd_cost_m2>0 & !is.na(opd_cost_m2), 1, 0),
            bi_opa_cost_m3 = ifelse(opd_cost_m3>0 & !is.na(opd_cost_m3), 1, 0),
            bi_opa_cost_m4 = ifelse(opd_cost_m4>0 & !is.na(opd_cost_m4), 1, 0),
            bi_opa_cost_m5 = ifelse(opd_cost_m5>0 & !is.na(opd_cost_m5), 1, 0),
            bi_opa_cost_m6 = ifelse(opd_cost_m6>0 & !is.na(opd_cost_m6), 1, 0),
            bi_opa_cost_m7 = ifelse(opd_cost_m7>0 & !is.na(opd_cost_m7), 1, 0),
            bi_opa_cost_m8 = ifelse(opd_cost_m8>0 & !is.na(opd_cost_m8), 1, 0),
            bi_opa_cost_m9 = ifelse(opd_cost_m9>0 & !is.na(opd_cost_m9), 1, 0),
            bi_opa_cost_m10 = ifelse(opd_cost_m10>0 & !is.na(opd_cost_m10), 1, 0),
            bi_opa_cost_m11 = ifelse(opd_cost_m11>0 & !is.na(opd_cost_m11), 1, 0),
            bi_opa_cost_m12 = ifelse(opd_cost_m12>0 & !is.na(opd_cost_m12), 1, 0)
      )


# vectors of opa visits and costs

# Initialize an empty list to store the results
exp_opa <- list()

# Loop over the months
for(i in 1:12) {
      # Create the column names
      opa_visits_m <- paste0("bi_opa_visit_m", i)
      opa_cost_m <- paste0("bi_opa_cost_m", i)
      
      # Call the function and store the result in the list
      exp_opa[[i]] <- compare_bi_visit_cost_fn(data = lc_exp_matched, 
                                               visits_m = opa_visits_m, 
                                               cost_m = opa_cost_m, 
                                               mont_n = i)
}

# Combine all data frames in the list
exp_opa_compare <- bind_rows(exp_opa) %>% mutate(exposure = "Long COVID exposure")


# combine all opa visits in 12 months
opa_visit_12m <- c()
for(i in 1:12){
      opa_visit_12m <- c(opa_visit_12m, paste0("bi_opa_visit_m", i))
}

lc_exp_matched$opa_visit_12m <- rowSums(lc_exp_matched[,opa_visit_12m],na.rm = T) # add them together
lc_exp_matched <-lc_exp_matched %>% mutate(opa_visit_12m = ifelse(opa_visit_12m>0, 1,0)) # recode

# combine all costs
opa_cost_12m <- c()
for(i in 1:12){
      opa_cost_12m <- c(opa_cost_12m, paste0("bi_opa_cost_m", i))
}
lc_exp_matched$opa_cost_12m <- rowSums(lc_exp_matched[,opa_cost_12m],na.rm = T) 
lc_exp_matched <- lc_exp_matched %>% mutate(opa_cost_12m = ifelse(opa_cost_12m>0, 1,0)) # recode


exp_12m_opa <- compare_bi_visit_cost_fn(data = lc_exp_matched, 
                                        visits_m = "opa_visit_12m", 
                                        cost_m = "opa_cost_12m", 
                                        mont_n = "total 12 months") %>% 
      mutate(exposure = "Long COVID exposure")

# Combine outputs for saving later: 

exp_opa_tabulate <- bind_rows(exp_opa_compare %>% mutate(month = as.character(month)), 
                              exp_12m_opa)

# # Comparator: ----
# create binomial outcomes in each months
com_matched <- com_matched %>%
      mutate(
            bi_opa_visit_m1 = ifelse(opa_visit_m1>0 & !is.na(opa_visit_m1), 1, 0),
            bi_opa_visit_m2 = ifelse(opa_visit_m2>0 & !is.na(opa_visit_m2), 1, 0),
            bi_opa_visit_m3 = ifelse(opa_visit_m3>0 & !is.na(opa_visit_m3), 1, 0),
            bi_opa_visit_m4 = ifelse(opa_visit_m4>0 & !is.na(opa_visit_m4), 1, 0),
            bi_opa_visit_m5 = ifelse(opa_visit_m5>0 & !is.na(opa_visit_m5), 1, 0),
            bi_opa_visit_m6 = ifelse(opa_visit_m6>0 & !is.na(opa_visit_m6), 1, 0),
            bi_opa_visit_m7 = ifelse(opa_visit_m7>0 & !is.na(opa_visit_m7), 1, 0),
            bi_opa_visit_m8 = ifelse(opa_visit_m8>0 & !is.na(opa_visit_m8), 1, 0),
            bi_opa_visit_m9 = ifelse(opa_visit_m9>0 & !is.na(opa_visit_m9), 1, 0),
            bi_opa_visit_m10 = ifelse(opa_visit_m10>0 & !is.na(opa_visit_m10), 1, 0),
            bi_opa_visit_m11 = ifelse(opa_visit_m11>0 & !is.na(opa_visit_m11), 1, 0),
            bi_opa_visit_m12 = ifelse(opa_visit_m12>0 & !is.na(opa_visit_m12), 1, 0)
      )

com_matched <- com_matched %>% 
      mutate(
            bi_opa_cost_m1 = ifelse(opd_cost_m1>0 & !is.na(opd_cost_m1), 1, 0),
            bi_opa_cost_m2 = ifelse(opd_cost_m2>0 & !is.na(opd_cost_m2), 1, 0),
            bi_opa_cost_m3 = ifelse(opd_cost_m3>0 & !is.na(opd_cost_m3), 1, 0),
            bi_opa_cost_m4 = ifelse(opd_cost_m4>0 & !is.na(opd_cost_m4), 1, 0),
            bi_opa_cost_m5 = ifelse(opd_cost_m5>0 & !is.na(opd_cost_m5), 1, 0),
            bi_opa_cost_m6 = ifelse(opd_cost_m6>0 & !is.na(opd_cost_m6), 1, 0),
            bi_opa_cost_m7 = ifelse(opd_cost_m7>0 & !is.na(opd_cost_m7), 1, 0),
            bi_opa_cost_m8 = ifelse(opd_cost_m8>0 & !is.na(opd_cost_m8), 1, 0),
            bi_opa_cost_m9 = ifelse(opd_cost_m9>0 & !is.na(opd_cost_m9), 1, 0),
            bi_opa_cost_m10 = ifelse(opd_cost_m10>0 & !is.na(opd_cost_m10), 1, 0),
            bi_opa_cost_m11 = ifelse(opd_cost_m11>0 & !is.na(opd_cost_m11), 1, 0),
            bi_opa_cost_m12 = ifelse(opd_cost_m12>0 & !is.na(opd_cost_m12), 1, 0)
      )

# Initialize an empty list to store the results
com_opa <- list()

# Loop over the months
for(i in 1:12) {
      # Create the column names
      opa_visits_m <- paste0("bi_opa_visit_m", i)
      opa_cost_m <- paste0("bi_opa_cost_m", i)
      
      # Call the function and store the result in the list
      com_opa[[i]] <- compare_bi_visit_cost_fn(data = com_matched, 
                                               visits_m = opa_visits_m, 
                                               cost_m = opa_cost_m, 
                                               mont_n = i)
}

# Combine all data frames in the list
com_opa_compare <- bind_rows(com_opa) %>% mutate(exposure = "Comparator")



# combine all opa visits in 12 months
com_matched$opa_visit_12m <- rowSums(com_matched[,opa_visit_12m],na.rm = T) # add them together
com_matched <-com_matched %>% mutate(opa_visit_12m = ifelse(opa_visit_12m>0, 1,0)) # recode

# combine all costs
com_matched$opa_cost_12m <- rowSums(com_matched[,opa_cost_12m],na.rm = T) 
com_matched <- com_matched %>% mutate(opa_cost_12m = ifelse(opa_cost_12m>0, 1,0)) # recode


com_12m_opa <- compare_bi_visit_cost_fn(data = com_matched, 
                                        visits_m = "opa_visit_12m", 
                                        cost_m = "opa_cost_12m", 
                                        mont_n = "total 12 months") %>% 
      mutate(exposure = "Comparator")

# Combine outputs for saving later: 

com_opa_tabulate <- bind_rows(com_opa_compare %>% mutate(month = as.character(month)), 
                              com_12m_opa)


# Combine and saving
bind_rows(exp_opa_tabulate, com_opa_tabulate) %>% relocate(exposure) %>% 
      write_csv(here("output", "qc09_opa_inconsistent_visit_cost.csv"))

# # Line graph showing the trend of the inconsistent pair: -----
exp_opa_inconsistent <- ggplot() + 
      geom_line(data = (filter(exp_opa_compare, visit =="No")),
                aes(x = month, y = have_cost, color = visit)) +
      geom_line(data = (filter(exp_opa_compare, visit =="Yes")),
                aes(x = month, y = no_cost, color = visit)) +
      ylab("Inconsistent pair counts") + xlab("Month") + theme_bw() + 
      scale_colour_discrete(labels=c("No visit but have costs", "Visited without costs")) +
      theme(legend.title = element_blank()) +
      scale_x_continuous(breaks = seq(1, 12))

com_opa_inconsistent <- ggplot() + 
      geom_line(data = (filter(com_opa_compare, visit =="No")),
                aes(x = month, y = have_cost, color = visit)) +
      geom_line(data = (filter(com_opa_compare, visit =="Yes")),
                aes(x = month, y = no_cost, color = visit)) +
      ylab("Inconsistent pair counts") + xlab("Month") + theme_bw() +
      scale_colour_discrete(labels=c("No visit but have costs", "Visited without costs")) +
      theme(legend.title = element_blank()) +
      scale_x_continuous(breaks = seq(1, 12))

ggarrange(exp_opa_inconsistent, com_opa_inconsistent, common.legend = T,
          ncol = 2, labels = c("Long COVID group", "Comparator group")
) %>% annotate_figure(top = text_grob("Outpatient clinic visits and costs comparison"))
# save outputs:
ggsave(file = "output/qc09_opa_non_zero_counts_comparison.png", width = 12, height = 4)
