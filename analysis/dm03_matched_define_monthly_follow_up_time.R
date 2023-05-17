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
# if index_date + days(month_number) > end_date, 
# which means people were censored earlier then the end of the month,
# so that we can only follow them from the beginning of the month 
# until the end_date

# key dates:index_date, end_date, and the monthly date 
# ------------------------------------

# First we need to change them into Date format:
matched_data$index_date <- as.Date(matched_data$index_date)
matched_data$end_date <- as.Date(matched_data$end_date)

# Month 1:
matched_data <- matched_data %>% 
      mutate(follow_up_m1 = ifelse(
            index_date + days(30*1) <= end_date , 
            30, 
            as.numeric(end_date) - as.numeric(index_date)
            )
)


# Month 2:
matched_data <- matched_data %>% 
      mutate(follow_up_m2 = ifelse(
            index_date + days(30*2) < end_date , 
            30, 
            as.numeric(end_date) - as.numeric(index_date) - (30*(2-1))
            )
)

# Month 3:
matched_data <- matched_data %>% 
      mutate(follow_up_m3 = ifelse(
            index_date + days(30*3) < end_date , 
            30, 
            as.numeric(end_date) - as.numeric(index_date) - (30*(3-1))
      )
)

# Month 4:
matched_data <- matched_data %>% 
      mutate(follow_up_m4 = ifelse(
            index_date + days(30*4) < end_date , 
            30, 
            as.numeric(end_date) - as.numeric(index_date) - (30*(4-1))
      )
)

# Month 5:
matched_data <- matched_data %>% 
      mutate(follow_up_m5 = ifelse(
            index_date + days(30*5) < end_date , 
            30, 
            as.numeric(end_date) - as.numeric(index_date) - (30*(5-1))
      )
)

# Month 6:
matched_data <- matched_data %>% 
      mutate(follow_up_m6 = ifelse(
            index_date + days(30*6) < end_date , 
            30, 
            as.numeric(end_date) - as.numeric(index_date) - (30*(6-1))
      )
)

# Month 7:
matched_data <- matched_data %>% 
      mutate(follow_up_m7 = ifelse(
            index_date + days(30*7) < end_date , 
            30, 
            as.numeric(end_date) - as.numeric(index_date) - (30*(7-1))
      )
)

# Month 8:
matched_data <- matched_data %>% 
      mutate(follow_up_m8 = ifelse(
            index_date + days(30*8) < end_date , 
            30, 
            as.numeric(end_date) - as.numeric(index_date) - (30*(8-1))
      )
)


# Month 9:
matched_data <- matched_data %>% 
      mutate(follow_up_m9 = ifelse(
            index_date + days(30*9) < end_date , 
            30, 
            as.numeric(end_date) - as.numeric(index_date) - (30*(9-1))
      )
)

# Month 10:
matched_data <- matched_data %>% 
      mutate(follow_up_m10 = ifelse(
            index_date + days(30*10) < end_date , 
            30, 
            as.numeric(end_date) - as.numeric(index_date) - (30*(10-1))
      )
)


# Month 11:
matched_data <- matched_data %>% 
      mutate(follow_up_m11 = ifelse(
            index_date + days(30*11) < end_date , 
            30, 
            as.numeric(end_date) - as.numeric(index_date) - (30*(11-1))
      )
)

# Month 12:
matched_data <- matched_data %>% 
      mutate(follow_up_m12 = ifelse(
            index_date + days(30*12) < end_date , 
            30, 
            as.numeric(end_date) - as.numeric(index_date) - (30*(12-1))
      )
)
