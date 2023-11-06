# Load previous data management
source("analysis/dm01_01_now_data_read_munge_vars.R")

# ============== Caclulate follow-up time by month ============== 
# Explanation: people were followed from the index date, and we will estimate 
# the healthcare utilisation by month. However, we need to consider 
# end_date may fall within the monthly follow-up time. 
#

# Goal: 
# fu_total, fu in month 1, 
# 0		, NA	      ==> fu_total==0~NA_real_
# 15		, 15		==> ((fu_total%/%30) <1 & fu_total!=0) ~ fu_total
# 30		, 30		==> fu_total%/%30>=1 ~ 30
# 35		, 30		==> fu_total%/%30>=1 ~ 30
# 45		, 30		==> fu_total%/%30>=1 ~ 30
# 60		, 30		==> fu_total%/%30>=1 ~ 30
# 65		, 30		==> fu_total%/%30>=1 ~ 30

# fu_total, fu in month 2,
# 0		,NA   ==> fu_total==0~NA_real_
# 15		,NA   ==> (fu_total%/%30) <1 ~ NA_real_
# 30		,NA   ==> fu_total%/%30 == 1 & fu_total%%30 ==0 ~ NA_real_
# 35		,5    ==> fu_total%/%30 == 1 & fu_total%%30 !=0 ~ fu_total%%30
# 45		,15   ==> fu_total%/%30 == 1 & fu_total%%30 !=0 ~ fu_total%%30
# 60		,30   ==> fu_total%/%30 > 1 ~ 30
# 65		,30   ==> fu_total%/%30 > 1 ~ 30

# Concept: use month 2 as example:
# If the total follow_up time can be exactly devided by 30 and the ratio >2, then the follow_up is 30; 
# If the follow up ended this month, then the remain of the dates will be the 
#     fu_total%/%30==3 & fu_total%%30 !=0~ fu_total%%30,
# If the remain is 0 when deviding by 30, then censored it with NA
#     fu_total%/%30 == 1 & fu_total%%30 ==0 ~ NA_real_
# If the follow-up ended in the previous month, then censored it with NA
#     fu_total%/%30 <1 ~ NA_real_

# total follow-up time: fu_total
# define follow-up time in the exposure dataset: ------

# define follow-up by months
lc_exp_matched <- lc_exp_matched %>% 
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

lc_exp_matched %<>% as_tibble()
lapply(lc_exp_matched[follow_up], summary)

# Define monthly follow-up in the comparator group:-----
com_matched<- com_matched%>% 
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



lapply(com_matched[follow_up], summary)
