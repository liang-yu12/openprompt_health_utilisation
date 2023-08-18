# Load previous data management:
source("analysis/dm02_02_pivot_longer_hxdata.R")



# Exposure & Contemporary subset: ----
now_exp <- hx_cases # subset for current group

# Need to sort follow-up time first
now_exp$fu_total <- as.numeric(now_exp$end_date) - as.numeric(now_exp$index_date)

now_exp <- now_exp %>% 
      mutate(fu_time_m1 = case_when(fu_total%/%30>=1 ~ 30,
                                    fu_total%/%30 <1 ~ fu_total,
                                    fu_total == 0 ~ NA_real_))

now_exp$end_date %>% summary
now_exp$index_date %>% summary
now_exp$fu_total %>% summary



# again, some people died before index date!



# Define the current visits
now_exp$all_month_m1 <- rowSums(now_exp[,m1_now, with = F], na.rm = T)
now_exp$all_month_m2 <- rowSums(now_exp[,m2_now, with = F], na.rm = T)
now_exp$all_month_m3 <- rowSums(now_exp[,m3_now, with = F], na.rm = T)
now_exp$all_month_m4 <- rowSums(now_exp[,m4_now, with = F], na.rm = T)
now_exp$all_month_m5 <- rowSums(now_exp[,m5_now, with = F], na.rm = T)
now_exp$all_month_m6 <- rowSums(now_exp[,m6_now, with = F], na.rm = T)
now_exp$all_month_m7 <- rowSums(now_exp[,m7_now, with = F], na.rm = T)
now_exp$all_month_m8 <- rowSums(now_exp[,m8_now, with = F], na.rm = T)
now_exp$all_month_m9 <- rowSums(now_exp[,m9_now, with = F], na.rm = T)
now_exp$all_month_m10 <- rowSums(now_exp[,m10_now, with = F], na.rm = T)
now_exp$all_month_m11 <- rowSums(now_exp[,m11_now, with = F], na.rm = T)
now_exp$all_month_m12 <- rowSums(now_exp[,m12_now, with = F], na.rm = T)

# simplify the data
now_exp <- now_exp %>% dplyr::select(all_of(common_vars), all_of(all_months_vars))

now_exp$exposure <- 1  # exposure group
now_exp$time <- 1 # contemporary records

# pivot the data:
now_exp_ts <- now_exp %>% 
      pivot_longer(
            cols = all_of(all_months_vars),
            names_to = c("month"),
            values_to = "monthly_visits"
      )

now_exp_ts$month <- str_sub(now_exp_ts$month, 12) %>% 
      as.numeric() # clean the month var

# Define current follow up time:




