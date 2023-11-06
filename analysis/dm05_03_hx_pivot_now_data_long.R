# Load previous data management:
source("analysis/dm05_02_hx_pivot_hxdata_longer.R")

# Exposure & Contemporary subset: ----
now_exp <- hx_cases # subset for current group

# Need to sort follow-up time first
now_exp$fu_total <- as.numeric(now_exp$end_date) - as.numeric(now_exp$index_date)

now_exp <- now_exp %>% 
      mutate(follow_up_m1=case_when(
            fu_total%/%30>=1 ~ 30,
            fu_total%/%30 <1 & fu_total!=0  ~ fu_total,
            fu_total == 0 ~ NA_real_)) %>% 
      mutate(follow_up_m2=case_when(
            fu_total%/%30> 1 ~ 30,
            fu_total%/%30==1 & fu_total%%30!=0 ~ fu_total%%30,
            fu_total%/%30==1 & fu_total%%30==0 ~ NA_real_,
            fu_total%/%30 < 1 ~ NA_real_)) %>% 
      mutate(follow_up_m3=case_when(
            fu_total%/%30> 2 ~ 30,
            fu_total%/%30==2 & fu_total%%30!=0  ~ fu_total%%30,
            fu_total%/%30==2 & fu_total%%30==0 ~ NA_real_,
            fu_total%/%30< 2 ~ NA_real_)) %>%
      mutate(follow_up_m4=case_when(
            fu_total%/%30> 3 ~ 30,
            fu_total%/%30==3 & fu_total%%30!=0  ~ fu_total%%30,
            fu_total%/%30==3 & fu_total%%30==0 ~ NA_real_,
            fu_total%/%30< 3 ~ NA_real_)) %>%
      mutate(follow_up_m5=case_when(
            fu_total%/%30> 4 ~ 30,
            fu_total%/%30==4 & fu_total%%30!=0  ~ fu_total%%30,
            fu_total%/%30==4 & fu_total%%30==0 ~ NA_real_,
            fu_total%/%30< 4 ~ NA_real_)) %>%
      mutate(follow_up_m6=case_when(
            fu_total%/%30> 5 ~ 30,
            fu_total%/%30==5 & fu_total%%30!=0 ~ fu_total%%30,
            fu_total%/%30==5 & fu_total%%30==0 ~ NA_real_,
            fu_total%/%30< 5 ~ NA_real_)) %>%
      mutate(follow_up_m7=case_when(
            fu_total%/%30> 6 ~ 30,
            fu_total%/%30==6 & fu_total%%30!=0 ~ fu_total%%30,
            fu_total%/%30==6 & fu_total%%30==0 ~ NA_real_,
            fu_total%/%30< 6 ~ NA_real_)) %>%
      mutate(follow_up_m8=case_when(
            fu_total%/%30> 7 ~ 30,
            fu_total%/%30==7 & fu_total%%30!=0 ~ fu_total%%30,
            fu_total%/%30==7 & fu_total%%30==0 ~ NA_real_,
            fu_total%/%30< 7 ~ NA_real_)) %>%
      mutate(follow_up_m9=case_when(
            fu_total%/%30> 8 ~ 30,
            fu_total%/%30==8 & fu_total%%30!=0 ~ fu_total%%30,
            fu_total%/%30==8 & fu_total%%30==0 ~ NA_real_,
            fu_total%/%30< 8 ~ NA_real_)) %>%
      mutate(follow_up_m10=case_when(
            fu_total%/%30> 9~ 30,
            fu_total%/%30==9 & fu_total%%30!=0 ~ fu_total%%30,
            fu_total%/%30==9 & fu_total%%30==0 ~ NA_real_,
            fu_total%/%30<9~NA_real_)) %>%
      mutate(follow_up_m11=case_when(
            fu_total%/%30> 10~ 30,
            fu_total%/%30==10 & fu_total%%30!=0 ~ fu_total%%30,
            fu_total%/%30==10 & fu_total%%30==0 ~ NA_real_,
            fu_total%/%30<10~NA_real_)) %>%
      mutate(follow_up_m12=case_when(
            fu_total%/%30> 11~ 30,
            fu_total%/%30==11 & fu_total%%30!=0 ~ fu_total%%30,
            fu_total%/%30==11 & fu_total%%30==0 ~ NA_real_,
            fu_total%/%30<11~NA_real_))


follow_up <- c("follow_up_m1","follow_up_m2","follow_up_m3","follow_up_m4",
               "follow_up_m5","follow_up_m6","follow_up_m7","follow_up_m8",
               "follow_up_m9","follow_up_m10","follow_up_m11","follow_up_m12")

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
now_exp <- now_exp %>% dplyr::select(all_of(common_vars), 
                                     all_of(all_months_vars), 
                                     all_of(follow_up))

# pivot data by visits:
now_exp_ts <- now_exp %>% 
      pivot_longer(
            cols = all_of(all_months_vars),
            names_to = c("month"),
            values_to = "monthly_visits"
      ) %>% 
      dplyr::select(all_of(common_vars), month, monthly_visits)

now_exp_ts$month <- str_sub(now_exp_ts$month, 12) %>% 
      as.numeric() # clean the month var

# Pivot data by follow-up time:
now_exp_fu_ts <- now_exp %>% 
      pivot_longer(
            cols = all_of(follow_up),
            names_to = c("month"),
            values_to = "fu_time"
      ) %>% 
      dplyr::select(patient_id, month, fu_time)

now_exp_fu_ts$month <- str_sub(now_exp_fu_ts$month, 12) %>% 
      as.numeric()

# join the follow-up data and visits data
now_exp_ts <- now_exp_ts %>% left_join(now_exp_fu_ts, 
                                       by = c("patient_id" = "patient_id", 
                                              "month" = "month"))

now_exp_ts$exposure <- 1  # exposure group
now_exp_ts$time <- 1 # contemporary records
now_exp_ts <- now_exp_ts %>% 
      relocate(exposure, .after = "number_comorbidities_cat") %>% 
      relocate(time, .after = "exposure")


# Comparator & Contemporary subset: --------------
now_com <- hx_control

# Sort follow-up time first
now_com$fu_total <- as.numeric(now_com$end_date) - as.numeric(now_com$index_date)

now_com <- now_com %>% 
      mutate(follow_up_m1=case_when(
            fu_total%/%30>=1 ~ 30,
            fu_total%/%30 <1 & fu_total!=0  ~ fu_total,
            fu_total == 0 ~ NA_real_)) %>% 
      mutate(follow_up_m2=case_when(
            fu_total%/%30> 1 ~ 30,
            fu_total%/%30==1 & fu_total%%30!=0 ~ fu_total%%30,
            fu_total%/%30==1 & fu_total%%30==0 ~ NA_real_,
            fu_total%/%30 < 1 ~ NA_real_)) %>% 
      mutate(follow_up_m3=case_when(
            fu_total%/%30> 2 ~ 30,
            fu_total%/%30==2 & fu_total%%30!=0  ~ fu_total%%30,
            fu_total%/%30==2 & fu_total%%30==0 ~ NA_real_,
            fu_total%/%30< 2 ~ NA_real_)) %>%
      mutate(follow_up_m4=case_when(
            fu_total%/%30> 3 ~ 30,
            fu_total%/%30==3 & fu_total%%30!=0  ~ fu_total%%30,
            fu_total%/%30==3 & fu_total%%30==0 ~ NA_real_,
            fu_total%/%30< 3 ~ NA_real_)) %>%
      mutate(follow_up_m5=case_when(
            fu_total%/%30> 4 ~ 30,
            fu_total%/%30==4 & fu_total%%30!=0  ~ fu_total%%30,
            fu_total%/%30==4 & fu_total%%30==0 ~ NA_real_,
            fu_total%/%30< 4 ~ NA_real_)) %>%
      mutate(follow_up_m6=case_when(
            fu_total%/%30> 5 ~ 30,
            fu_total%/%30==5 & fu_total%%30!=0 ~ fu_total%%30,
            fu_total%/%30==5 & fu_total%%30==0 ~ NA_real_,
            fu_total%/%30< 5 ~ NA_real_)) %>%
      mutate(follow_up_m7=case_when(
            fu_total%/%30> 6 ~ 30,
            fu_total%/%30==6 & fu_total%%30!=0 ~ fu_total%%30,
            fu_total%/%30==6 & fu_total%%30==0 ~ NA_real_,
            fu_total%/%30< 6 ~ NA_real_)) %>%
      mutate(follow_up_m8=case_when(
            fu_total%/%30> 7 ~ 30,
            fu_total%/%30==7 & fu_total%%30!=0 ~ fu_total%%30,
            fu_total%/%30==7 & fu_total%%30==0 ~ NA_real_,
            fu_total%/%30< 7 ~ NA_real_)) %>%
      mutate(follow_up_m9=case_when(
            fu_total%/%30> 8 ~ 30,
            fu_total%/%30==8 & fu_total%%30!=0 ~ fu_total%%30,
            fu_total%/%30==8 & fu_total%%30==0 ~ NA_real_,
            fu_total%/%30< 8 ~ NA_real_)) %>%
      mutate(follow_up_m10=case_when(
            fu_total%/%30> 9~ 30,
            fu_total%/%30==9 & fu_total%%30!=0 ~ fu_total%%30,
            fu_total%/%30==9 & fu_total%%30==0 ~ NA_real_,
            fu_total%/%30<9~NA_real_)) %>%
      mutate(follow_up_m11=case_when(
            fu_total%/%30> 10~ 30,
            fu_total%/%30==10 & fu_total%%30!=0 ~ fu_total%%30,
            fu_total%/%30==10 & fu_total%%30==0 ~ NA_real_,
            fu_total%/%30<10~NA_real_)) %>%
      mutate(follow_up_m12=case_when(
            fu_total%/%30> 11~ 30,
            fu_total%/%30==11 & fu_total%%30!=0 ~ fu_total%%30,
            fu_total%/%30==11 & fu_total%%30==0 ~ NA_real_,
            fu_total%/%30<11~NA_real_))


# Define the current visits
now_com$all_month_m1 <- rowSums(now_com[,m1_now, with = F], na.rm = T)
now_com$all_month_m2 <- rowSums(now_com[,m2_now, with = F], na.rm = T)
now_com$all_month_m3 <- rowSums(now_com[,m3_now, with = F], na.rm = T)
now_com$all_month_m4 <- rowSums(now_com[,m4_now, with = F], na.rm = T)
now_com$all_month_m5 <- rowSums(now_com[,m5_now, with = F], na.rm = T)
now_com$all_month_m6 <- rowSums(now_com[,m6_now, with = F], na.rm = T)
now_com$all_month_m7 <- rowSums(now_com[,m7_now, with = F], na.rm = T)
now_com$all_month_m8 <- rowSums(now_com[,m8_now, with = F], na.rm = T)
now_com$all_month_m9 <- rowSums(now_com[,m9_now, with = F], na.rm = T)
now_com$all_month_m10 <- rowSums(now_com[,m10_now, with = F], na.rm = T)
now_com$all_month_m11 <- rowSums(now_com[,m11_now, with = F], na.rm = T)
now_com$all_month_m12 <- rowSums(now_com[,m12_now, with = F], na.rm = T)

# simplify the data
now_com <- now_com %>% dplyr::select(all_of(common_vars), 
                                     all_of(all_months_vars), 
                                     all_of(follow_up))


# pivot data by visits:
now_com_ts <- now_com %>% 
      pivot_longer(
            cols = all_of(all_months_vars),
            names_to = c("month"),
            values_to = "monthly_visits"
      ) %>% 
      dplyr::select(all_of(common_vars), month, monthly_visits)

now_com_ts$month <- str_sub(now_com_ts$month, 12) %>% 
      as.numeric() # clean the month var


# Pivot data by follow-up time:
now_com_fu_ts <- now_com %>% 
      pivot_longer(
            cols = all_of(follow_up),
            names_to = c("month"),
            values_to = "fu_time"
      ) %>% 
      dplyr::select(patient_id, month, fu_time)

now_com_fu_ts$month <- str_sub(now_com_fu_ts$month, 12) %>% 
      as.numeric()


# join the follow-up data and visits data
now_com_ts <- now_com_ts %>% left_join(now_com_fu_ts, 
                                       by = c("patient_id" = "patient_id", 
                                              "month" = "month"))

now_com_ts$exposure <- 0  # exposure group
now_com_ts$time <- 1 # contemporary records
now_com_ts <- now_com_ts %>% 
      relocate(exposure, .after = "number_comorbidities_cat") %>% 
      relocate(time, .after = "exposure")


