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

baseline_duration_ctrl <- mean(df_control1$psqi_sleep)
trim2_duration_ctrl <- mean(df_control2$psqi_sleep)
trim3_duration_ctrl <- mean(df_control3$psqi_sleep)
baseline_sd_duration_ctrl <- sd(df_control1$psqi_sleep)
trim2_sd_duration_ctrl <- sd(df_control2$psqi_sleep)
trim3_sd_duration_ctrl <- sd(df_control3$psqi_sleep)

baseline_duration_int <- mean(df_intervention1$psqi_sleep)
trim2_duration_int <- mean(df_intervention2$psqi_sleep)
trim3_duration_int <- mean(df_intervention3$psqi_sleep)
baseline_sd_duration_int <- sd(df_intervention1$psqi_sleep)
trim2_sd_duration_int <- sd(df_intervention2$psqi_sleep)
trim3_sd_duration_int <- sd(df_intervention3$psqi_sleep)

combined_baseline_mean_duration <- round(rbind(baseline_duration_int, baseline_duration_ctrl), digits = 2)
combined_baseline_sd_duration <- round(rbind(baseline_sd_duration_int, baseline_sd_duration_ctrl), digits = 2)
combined_trim2_mean_duration <- round(rbind(trim2_duration_int, trim2_duration_ctrl), digits = 2)
combined_trim2_sd_duration <- round(rbind(trim2_sd_duration_int, trim2_sd_duration_ctrl), digits = 2)
combined_trim3_mean_duration <- round(rbind(trim3_duration_int, trim3_duration_ctrl), digits = 2)
combined_trim3_sd_duration <- round(rbind(trim3_sd_duration_int, trim3_sd_duration_ctrl), digits = 2)

duration_effects <- as.data.frame(round(fixef(duration_model), digits = 2))
duration_effects <- duration_effects[-c(1,3:4),]
sd_duration_total <- sd(df_scored$psqi_sleep)
duration_cohens_d <- round(abs(duration_effects/sd_duration_total), digits = 2)

duration_se <- as.data.frame(round(sqrt(diag(vcov(duration_model))), digits = 2))
duration_se <- duration_se[-c(1,3:4),]
duration_p <- as.data.frame(round(duration_results$"Pr(>Chisq)", digits = 2))
duration_p <- duration_p[-1,]

combined_duration <- data.frame(Sleep_Outcome = c("Duration", ""),
                                Group = c("Intervention", "Control"),
                                Baseline = paste0(combined_baseline_mean_duration, " (", combined_baseline_sd_duration, ")"),
                                Second_Trimester = paste0(combined_trim2_mean_duration, " (", combined_trim2_sd_duration, ")"),
                                Third_Trimester = paste0(combined_trim3_mean_duration, " (", combined_trim3_sd_duration, ")"),
                                Estimate = c("", paste0(duration_effects, " (", duration_se, ")")),
                                p = c("", duration_p),
                                Cohens_d = c("", duration_cohens_d)
)


baseline_global_ctrl <- mean(df_control1$global_psqi_score)
trim2_global_ctrl <- mean(df_control2$global_psqi_score)
trim3_global_ctrl <- mean(df_control3$global_psqi_score)
baseline_sd_global_ctrl <- sd(df_control1$global_psqi_score)
trim2_sd_global_ctrl <- sd(df_control2$global_psqi_score)
trim3_sd_global_ctrl <- sd(df_control3$global_psqi_score)

baseline_global_int <- mean(df_intervention1$global_psqi_score)
trim2_global_int <- mean(df_intervention2$global_psqi_score)
trim3_global_int <- mean(df_intervention3$global_psqi_score)
baseline_sd_global_int <- sd(df_intervention1$global_psqi_score)
trim2_sd_global_int <- sd(df_intervention2$global_psqi_score)
trim3_sd_global_int <- sd(df_intervention3$global_psqi_score)

combined_baseline_mean <- round(rbind(baseline_global_int, baseline_global_ctrl), digits = 2)
combined_baseline_sd <- round(rbind(baseline_sd_global_int, baseline_sd_global_ctrl), digits = 2)
combined_trim2_mean <- round(rbind(trim2_global_int, trim2_global_ctrl), digits = 2)
combined_trim2_sd <- round(rbind(trim2_sd_global_int, trim2_sd_global_ctrl), digits = 2)
combined_trim3_mean <- round(rbind(trim3_global_int, trim3_global_ctrl), digits = 2)
combined_trim3_sd <- round(rbind(trim3_sd_global_int, trim3_sd_global_ctrl), digits = 2)



mean(df_control1$disturbances_scored)
mean(df_control2$disturbances_scored)
mean(df_control3$disturbances_scored)

mean(df_intervention1$disturbances_scored, na.rm = TRUE)
mean(df_intervention2$disturbances_scored, na.rm = TRUE)
mean(df_intervention3$disturbances_scored, na.rm = TRUE)