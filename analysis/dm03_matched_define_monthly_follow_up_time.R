# Load previous data management
source("analysis/dm01_matched_current_data.R")

# ============== Caclulate follow-up time by month ============== 
# Explanation: people were followed from the index date, and we will estimate 
# the healthcare utilisation by month. However, we need to consider 
# end_date may fall within the monthly follow-up time. 
# Need to define monthly

# *Calculate the cumulative follow-up time

# Concept: ---------------------------
# if index_date + days(month_number) <= end_date, 
# which means we can follow them for full months; 
#     codes:      time1 + 30*n < time2 ~ 30*n,

# if index_date + days(month_number) > end_date, 
# people were censored earlier then the end of the month,
# so that we can only follow them until the end_date (time2)  
# codes:   
# (time2 < (time1 + 30*(n)) & (time2 - (time1+ 30*(n-1)))>0 ~ (time2 - (time1))


# Also, if the follow-up period is 0, then it should be coded as NA.
# codes: time2-time1 <=0 ~ NA

# key dates:index_date, end_date, and the monthly date 
# ------------------------------------

# First we need to change them into Date format:
matched_data$time1 <- as.numeric(matched_data$index_date)
matched_data$time2 <- as.numeric(matched_data$end_date)

# Month 1:
matched_data <- matched_data %>% 
      mutate(follow_up_m1 = as.double(case_when(
            time1 + 30*1 < time2 ~ 30*1,    #t2 censor later
            (time2 <= (time1 + 30*1)) & ((time2 - time1) > 30*(1-1)) ~ 
                  (time2 - time1), # t2 censor before follow 
            (time2 - time1) <= 30*(1-1) ~ NA_real_ # already censored 
            ))
)


# Month 2:
matched_data <- matched_data %>% 
      mutate(follow_up_m2 = as.double(case_when(
            time1 + 30*2 < time2 ~ 30*2,    #t2 censor later
            (time2 <= (time1 + 30*2)) & ((time2 - time1) > 30*(2-1)) ~ 
                  (time2 - time1), # t2 censor before follow 
            (time2 - time1) <= 30*(2-1) ~ NA_real_ # already censored 
      ))
)


# Month 3:
matched_data <- matched_data %>% 
      mutate(follow_up_m3 = as.double(case_when(
            time1 + 30*3 < time2 ~ 30*3,    #t2 censor later
            (time2 <= (time1 + 30*3)) & ((time2 - time1) > 30*(3-1)) ~ 
                  (time2 - time1), # t2 censor before follow 
            (time2 - time1) <= 30*(3-1) ~ NA_real_ # already censored 
      ))
      )



# Month 4:
matched_data <- matched_data %>% 
      mutate(follow_up_m4 = as.double(case_when(
            time1 + 30*4 < time2 ~ 30*4,    #t2 censor later
            (time2 <= (time1 + 30*4)) & ((time2 - time1) > 30*(4-1)) ~ 
                  (time2 - time1), # t2 censor before follow 
            (time2 - time1) <= 30*(4-1) ~ NA_real_ # already censored 
      ))
      )



# Month 5:
matched_data <- matched_data %>% 
      mutate(follow_up_m5 = as.double(case_when(
            time1 + 30*5 < time2 ~ 30*5,    #t2 censor later
            (time2 <= (time1 + 30*5)) & ((time2 - time1) > 30*(5-1)) ~ 
                  (time2 - time1), # t2 censor before follow 
            (time2 - time1) <= 30*(5-1) ~ NA_real_ # already censored 
      ))
      )



# Month 6:
matched_data <- matched_data %>% 
      mutate(follow_up_m6 = as.double(case_when(
            time1 + 30*6 < time2 ~ 30*6,    #t2 censor later
            (time2 <= (time1 + 30*6)) & ((time2 - time1) > 30*(6-1)) ~ 
                  (time2 - time1), # t2 censor before follow 
            (time2 - time1) <= 30*(6-1) ~ NA_real_ # already censored 
      ))
      )


# Month 7:
matched_data <- matched_data %>% 
      mutate(follow_up_m7 = as.double(case_when(
            time1 + 30*7 < time2 ~ 30*7,    #t2 censor later
            (time2 <= (time1 + 30*7)) & ((time2 - time1) > 30*(7-1)) ~ 
                  (time2 - time1), # t2 censor before follow 
            (time2 - time1) <= 30*(7-1) ~ NA_real_ # already censored 
      ))
      )

# Month 8:
matched_data <- matched_data %>% 
      mutate(follow_up_m8 = as.double(case_when(
            time1 + 30*8 < time2 ~ 30*8,    #t2 censor later
            (time2 <= (time1 + 30*8)) & ((time2 - time1) > 30*(8-1)) ~ 
                  (time2 - time1), # t2 censor before follow 
            (time2 - time1) <= 30*(8-1) ~ NA_real_ # already censored 
      ))
      )

# Month 9:
matched_data <- matched_data %>% 
      mutate(follow_up_m9 = as.double(case_when(
            time1 + 30*9 < time2 ~ 30*9,    #t2 censor later
            (time2 <= (time1 + 30*9)) & ((time2 - time1) > 30*(9-1)) ~ 
                  (time2 - time1), # t2 censor before follow 
            (time2 - time1) <= 30*(9-1) ~ NA_real_ # already censored 
      ))
      )

# Month 10:
matched_data <- matched_data %>% 
      mutate(follow_up_m10 = as.double(case_when(
            time1 + 30*10 < time2 ~ 30*10,    #t2 censor later
            (time2 <= (time1 + 30*10)) & ((time2 - time1) > 30*(10-1)) ~ 
                  (time2 - time1), # t2 censor before follow 
            (time2 - time1) <= 30*(10-1) ~ NA_real_ # already censored 
      ))
      )



# Month 11:
matched_data <- matched_data %>% 
      mutate(follow_up_m11 = as.double(case_when(
            time1 + 30*11 < time2 ~ 30*11,    #t2 censor later
            (time2 <= (time1 + 30*11)) & ((time2 - time1) > 30*(11-1)) ~ 
                  (time2 - time1), # t2 censor before follow 
            (time2 - time1) <= 30*(11-1) ~ NA_real_ # already censored 
      ))
      )


# Month 12:
matched_data <- matched_data %>% 
      mutate(follow_up_m12 = as.double(case_when(
            time1 + 30*12 < time2 ~ 30*12,    #t2 censor later
            (time2 <= (time1 + 30*12)) & ((time2 - time1) > 30*(12-1)) ~ 
                  (time2 - time1), # t2 censor before follow 
            (time2 - time1) <= 30*(12-1) ~ NA_real_ # already censored 
      ))
      )

# follow-up periods vector:

follow_up <- c("follow_up_m1","follow_up_m2","follow_up_m3","follow_up_m4",
               "follow_up_m5","follow_up_m6","follow_up_m7","follow_up_m8",
               "follow_up_m9","follow_up_m10","follow_up_m11","follow_up_m12")

matched_data %<>% as_tibble()
lapply(matched_data[follow_up], summary)

# Change healthcare visit to accumulateve visits:

visit <- c("all_month1", "all_month2", "all_month3", "all_month4", "all_month5",
           "all_month6", "all_month7", "all_month8", "all_month9", "all_month10",
           "all_month11", "all_month12")
matched_data[visit] <- lapply(matched_data[visit], as.numeric) # make it consistent

matched_data$all_month2 <- matched_data$all_month1 + matched_data$all_month2
matched_data$all_month3 <- matched_data$all_month2 + matched_data$all_month3
matched_data$all_month4 <- matched_data$all_month3 + matched_data$all_month4
matched_data$all_month5 <- matched_data$all_month4 + matched_data$all_month5
matched_data$all_month6 <- matched_data$all_month5 + matched_data$all_month6
matched_data$all_month7 <- matched_data$all_month6 + matched_data$all_month7
matched_data$all_month8 <- matched_data$all_month7 + matched_data$all_month8
matched_data$all_month9 <- matched_data$all_month8 + matched_data$all_month9
matched_data$all_month10 <- matched_data$all_month9 + matched_data$all_month10
matched_data$all_month11 <- matched_data$all_month10 + matched_data$all_month11
matched_data$all_month12 <- matched_data$all_month11 + matched_data$all_month12

lapply(matched_data[visit], summary)


