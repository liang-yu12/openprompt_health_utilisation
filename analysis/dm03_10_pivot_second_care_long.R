# Goal: Data management to combine seconadary cares and 
# pivot the data

# run the analyses using different outcomes: 

# Load previous data management
source("analysis/dm03_matched_define_monthly_follow_up_time.R")

# 2nd care columns: exclude gp records from the previous created m
sec_m1 <- m1[!grepl("gp_visit_m",m1)]
sec_m2 <- m2[!grepl("gp_visit_m",m2)]
sec_m3 <- m3[!grepl("gp_visit_m",m3)]
sec_m4 <- m4[!grepl("gp_visit_m",m4)]
sec_m5 <- m5[!grepl("gp_visit_m",m5)]
sec_m6 <- m6[!grepl("gp_visit_m",m6)]
sec_m7 <- m7[!grepl("gp_visit_m",m7)]
sec_m8 <- m8[!grepl("gp_visit_m",m8)]
sec_m9 <- m9[!grepl("gp_visit_m",m9)]
sec_m10 <- m10[!grepl("gp_visit_m",m10)]
sec_m11 <- m11[!grepl("gp_visit_m",m11)]
sec_m12 <- m12[!grepl("gp_visit_m",m12)]

# Follow-up time:
fu_cols <- lc_exp_matched[grep("follow_up_m", names(lc_exp_matched))] %>% 
      names %>% as.vector()

# sec_visits_columns
sec_visits <- c()
for (i in 1:12) {
      sec_visits <- c(sec_visits, paste0("sec_visits_m", i))
}

# Exposure data: -----
# Calculate the monthly secondary care visits
lc_exp_matched$sec_visits_m1 <- rowSums(lc_exp_matched[sec_m1])
lc_exp_matched$sec_visits_m2 <- rowSums(lc_exp_matched[sec_m2])
lc_exp_matched$sec_visits_m3 <- rowSums(lc_exp_matched[sec_m3])
lc_exp_matched$sec_visits_m4 <- rowSums(lc_exp_matched[sec_m4])
lc_exp_matched$sec_visits_m5 <- rowSums(lc_exp_matched[sec_m5])
lc_exp_matched$sec_visits_m6 <- rowSums(lc_exp_matched[sec_m6])
lc_exp_matched$sec_visits_m7 <- rowSums(lc_exp_matched[sec_m7])
lc_exp_matched$sec_visits_m8 <- rowSums(lc_exp_matched[sec_m8])
lc_exp_matched$sec_visits_m9 <- rowSums(lc_exp_matched[sec_m9])
lc_exp_matched$sec_visits_m10 <- rowSums(lc_exp_matched[sec_m10])
lc_exp_matched$sec_visits_m11 <- rowSums(lc_exp_matched[sec_m11])
lc_exp_matched$sec_visits_m12 <- rowSums(lc_exp_matched[sec_m12])

#Pivot sec_visits in the exposure dataset: ==================
exp_2ndcare_visit_ts <- lc_exp_matched %>% 
      pivot_longer(
            cols = all_of(sec_visits),
            names_to = c("month"),
            values_to = "monthly_secondary_care_visits"
      )
exp_2ndcare_visit_ts$month <- str_sub(exp_2ndcare_visit_ts$month, 13) # remove opa_visit_m
exp_2ndcare_visit_ts$month <- as.numeric(exp_2ndcare_visit_ts$month)


# Pivot the follow_up time in the exposure data: ========================
exp_fu_ts <- lc_exp_matched %>% dplyr::select(patient_id, all_of(fu_cols)) %>% 
      pivot_longer(
            cols = all_of(fu_cols),
            names_to = c("month_fu"),
            values_to = "follow_up_time"
      )

exp_fu_ts$month_fu <- str_sub(exp_fu_ts$month_fu, 12)  # remove "follow_up_m"
exp_fu_ts$month_fu <- as.numeric(exp_fu_ts$month_fu)

# # Combine the exposure data: 
exp_2nd_long <- left_join(exp_2ndcare_visit_ts, exp_fu_ts,
                      by = c("patient_id" = "patient_id", "month" = "month_fu")
)
exp_2nd_long %>% names # looks good

rm(exp_2ndcare_visit_ts, exp_fu_ts) # housekeeping

# Data management of the comparator group: ------
com_matched$sec_visits_m1 <- rowSums(com_matched[sec_m1])
com_matched$sec_visits_m2 <- rowSums(com_matched[sec_m2])
com_matched$sec_visits_m3 <- rowSums(com_matched[sec_m3])
com_matched$sec_visits_m4 <- rowSums(com_matched[sec_m4])
com_matched$sec_visits_m5 <- rowSums(com_matched[sec_m5])
com_matched$sec_visits_m6 <- rowSums(com_matched[sec_m6])
com_matched$sec_visits_m7 <- rowSums(com_matched[sec_m7])
com_matched$sec_visits_m8 <- rowSums(com_matched[sec_m8])
com_matched$sec_visits_m9 <- rowSums(com_matched[sec_m9])
com_matched$sec_visits_m10 <- rowSums(com_matched[sec_m10])
com_matched$sec_visits_m11 <- rowSums(com_matched[sec_m11])
com_matched$sec_visits_m12 <- rowSums(com_matched[sec_m12])

# Pivot the comparator ==========
com_2ndcare_visit_ts <- com_matched %>% 
      pivot_longer(
            cols = all_of(sec_visits),
            names_to = c("month"),
            values_to = "monthly_secondary_care_visits"
      )
com_2ndcare_visit_ts$month <- str_sub(com_2ndcare_visit_ts$month, 13) # remove string
com_2ndcare_visit_ts$month <- as.numeric(com_2ndcare_visit_ts$month)

# Pivot the comparator follow_up time: ========================

com_fu_ts <- com_matched %>% dplyr::select(patient_id, all_of(fu_cols)) %>% 
      pivot_longer(
            cols = all_of(fu_cols),
            names_to = c("month_fu"),
            values_to = "follow_up_time"
      )

com_fu_ts$month_fu <- str_sub(com_fu_ts$month_fu, 12)  # remove "follow_up_m"
com_fu_ts$month_fu <- as.numeric(com_fu_ts$month_fu)


# Combine the data: =============
com_2nd_long <- left_join(com_2ndcare_visit_ts, com_fu_ts,
                      by = c("patient_id" = "patient_id", "month" = "month_fu")
)
com_2nd_long %>% names

# housekeeping
rm(com_2ndcare_visit_ts, com_fu_ts)
rm(lc_exp_matched, com_matched)

matched_data_2nd_ts <- bind_rows(exp_2nd_long, com_2nd_long)
matched_data_2nd_ts$exposure %>% levels
matched_data_2nd_ts$exposure <- relevel(matched_data_2nd_ts$exposure, "Comparator")
