#Find means of sleep duration, global psqi scores, and sleep distrubances per visits by randomization

df_control1 <- df_scored %>% 
  filter(randomization == 0, redcap_event_name == "baseline_arm_1") 

df_control2 <- df_scored %>% 
  filter(randomization == 0, redcap_event_name == "visit_2_arm_1") 

df_control3 <- df_scored %>% 
  filter(randomization == 0, redcap_event_name == "visit_3_arm_1") 

df_intervention1 <- df_scored %>% 
  filter(randomization == 1, redcap_event_name == "baseline_arm_1") 

df_intervention2 <- df_scored %>% 
  filter(randomization == 1, redcap_event_name == "visit_2_arm_1") 

df_intervention3 <- df_scored %>% 
  filter(randomization == 1, redcap_event_name == "visit_3_arm_1") 

mean(df_control1$psqi_sleep)
mean(df_control2$psqi_sleep)
mean(df_control3$psqi_sleep)

mean(df_intervention1$psqi_sleep)
mean(df_intervention2$psqi_sleep, na.rm = TRUE)
mean(df_intervention3$psqi_sleep, na.rm = TRUE)

mean(df_control1$global_psqi_score)
mean(df_control2$global_psqi_score)
mean(df_control3$global_psqi_score)

mean(df_intervention1$global_psqi_score, na.rm = TRUE)
mean(df_intervention2$global_psqi_score, na.rm = TRUE)
mean(df_intervention3$global_psqi_score, na.rm = TRUE)

mean(df_control1$disturbances_scored)
mean(df_control2$disturbances_scored)
mean(df_control3$disturbances_scored)

mean(df_intervention1$disturbances_scored, na.rm = TRUE)
mean(df_intervention2$disturbances_scored, na.rm = TRUE)
mean(df_intervention3$disturbances_scored, na.rm = TRUE)