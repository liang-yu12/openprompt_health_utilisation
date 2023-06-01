# Load previous data management
source("analysis/dm01_matched_current_data.R")

# ============== Caclulate follow-up time by month ============== 
# Explanation: people were followed from the index date, and we will estimate 
# the healthcare utilisation by month. However, we need to consider 
# end_date may fall within the monthly follow-up time. 
# Need to define monthly

# Concept: ---------------------------
# if index_date + days(month_number) <= end_date, 
# which means we can follow them for a full month; 
#     codes:      time1 + 30*1 < time2 ~ 30,

# if index_date + days(month_number) > end_date, 
# people were censored earlier then the end of the month,
# so that we can only follow them from the beginning of the month (time1 + 30*n)
# until the end_date (time2)  
# codes:   
# (time2 < (time1 + 30*1)) & (time2 - time1)>0 ~ (time2 - (time1 + 30*0))


# Also, if the follow-up period is 0, then it should be coded as NA.
# codes: time2-time1 <=0 ~ NA

# key dates:index_date, end_date, and the monthly date 
# ------------------------------------

# First we need to change them into Date format:
matched_data$time1 <- as.numeric(matched_data$index_date)
matched_data$time2 <- as.numeric(matched_data$end_date)

# Month 1:
matched_data <- matched_data %>% 
      mutate(follow_up_m1 = case_when(
            time1 + 30*1 < time2 ~ 30,
            (time2 < (time1 + 30*1)) & (time2 - (time1 + 30*(1-1)))>0 ~ 
                  (time2 - (time1 + 30*(1-1))),
            (time2 - (time1 + 30*(1-1))) <=0 ~ NA
            )
)


# Month 2:
matched_data <- matched_data %>% 
      mutate(follow_up_m2 = case_when(
            time1 + 30*2 < time2 ~ 30,
            (time2 < (time1 + 30*2)) & (time2 - (time1 + 30*(2-1)))>0 ~ (time2 - (time1 + 30*(2-1))),
            (time2 - (time1 + 30*(2-1))) <=0 ~ NA
            )
)

# Month 3:
matched_data <- matched_data %>% 
      mutate(follow_up_m3 = case_when(
            time1 + 30*3 < time2 ~ 30,
            (time2 < (time1 + 30*3)) & (time2 - (time1 + 30*(3-1)))>0 ~ (time2 - (time1 + 30*(3-1))),
            (time2 - (time1 + 30*(3-1))) <=0 ~ NA
      )
)


# Month 4:
matched_data <- matched_data %>% 
      mutate(follow_up_m4 = case_when(
            time1 + 30*4 < time2 ~ 30,
            (time2 < (time1 + 30*4)) & (time2 - (time1 + 30*(4-1)))>0 ~ (time2 - (time1 + 30*(4-1))),
            (time2 - (time1 + 30*(4-1))) <=0 ~ NA
      )
)


# Month 5:
matched_data <- matched_data %>% 
      mutate(follow_up_m5 = case_when(
            time1 + 30*5 < time2 ~ 30,
            (time2 < (time1 + 30*5)) & (time2 - (time1 + 30*(5-1)))>0 ~ (time2 - (time1 + 30*(5-1))),
            (time2 - (time1 + 30*(5-1))) <=0 ~ NA
      )
)

# Month 6:
matched_data <- matched_data %>% 
      mutate(follow_up_m6 = case_when(
            time1 + 30*6 < time2 ~ 30,
            (time2 < (time1 + 30*6)) & (time2 - (time1 + 30*(6-1)))>0 ~ (time2 - (time1 + 30*(6-1))),
            (time2 - (time1 + 30*(6-1))) <=0 ~ NA
      )
)

# Month 7:
matched_data <- matched_data %>% 
      mutate(follow_up_m7 = case_when(
            time1 + 30*7 < time2 ~ 30,
            (time2 < (time1 + 30*7)) & (time2 - (time1 + 30*(7-1)))>0 ~ (time2 - (time1 + 30*(7-1))),
            (time2 - (time1 + 30*(7-1))) <=0 ~ NA
      )
)

# Month 8:
matched_data <- matched_data %>% 
      mutate(follow_up_m8 = case_when(
            time1 + 30*8 < time2 ~ 30,
            (time2 < (time1 + 30*8)) & (time2 - (time1 + 30*(8-1)))>0 ~ (time2 - (time1 + 30*(8-1))),
            (time2 - (time1 + 30*(8-1))) <=0 ~ NA
      )
)

# Month 9:
matched_data <- matched_data %>% 
      mutate(follow_up_m9 = case_when(
            time1 + 30*9 < time2 ~ 30,
            (time2 < (time1 + 30*9)) & (time2 - (time1 + 30*(9-1)))>0 ~ (time2 - (time1 + 30*(9-1))),
            (time2 - (time1 + 30*(9-1))) <=0 ~ NA
      )
)

# Month 10:
matched_data <- matched_data %>% 
      mutate(follow_up_m10 = case_when(
            time1 + 30*10 < time2 ~ 30,
            (time2 < (time1 + 30*10)) & (time2 - (time1 + 30*(10-1)))>0 ~ (time2 - (time1 + 30*(10-1))),
            (time2 - (time1 + 30*(10-1))) <=0 ~ NA
      )
)


# Month 11:
matched_data <- matched_data %>% 
      mutate(follow_up_m11 = case_when(
            time1 + 30*11 < time2 ~ 30,
            (time2 < (time1 + 30*11)) & (time2 - (time1 + 30*(11-1)))>0 ~ (time2 - (time1 + 30*(11-1))),
            (time2 - (time1 + 30*(11-1))) <=0 ~ NA
      )
)

# Month 12:
matched_data <- matched_data %>% 
      mutate(follow_up_m12 = case_when(
            time1 + 30*12 < time2 ~ 30,
            (time2 < (time1 + 30*12)) & (time2 - (time1 + 30*(12-1)))>0 ~ (time2 - (time1 + 30*(12-1))),
            (time2 - (time1 + 30*(12-1))) <=0 ~ NA
      )
)


# follow-up periods vector:

follow_up <- c("follow_up_m1","follow_up_m2","follow_up_m3","follow_up_m4",
               "follow_up_m5","follow_up_m6","follow_up_m7","follow_up_m8",
               "follow_up_m9","follow_up_m10","follow_up_m11","follow_up_m12")

matched_data %<>% as_tibble()
lapply(matched_data[follow_up], summary)
