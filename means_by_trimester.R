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

combined_duration <- data.frame(Sleep_Outcome = c("Duration (hrs)", ""),
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

baseline_global_int <- mean(df_intervention1$global_psqi_score, na.rm = TRUE)
trim2_global_int <- mean(df_intervention2$global_psqi_score)
trim3_global_int <- mean(df_intervention3$global_psqi_score)
baseline_sd_global_int <- sd(df_intervention1$global_psqi_score, na.rm = TRUE)
trim2_sd_global_int <- sd(df_intervention2$global_psqi_score)
trim3_sd_global_int <- sd(df_intervention3$global_psqi_score)

combined_baseline_mean_global <- round(rbind(baseline_global_int, baseline_global_ctrl), digits = 2)
combined_baseline_sd_global <- round(rbind(baseline_sd_global_int, baseline_sd_global_ctrl), digits = 2)
combined_trim2_mean_global <- round(rbind(trim2_global_int, trim2_global_ctrl), digits = 2)
combined_trim2_sd_global <- round(rbind(trim2_sd_global_int, trim2_sd_global_ctrl), digits = 2)
combined_trim3_mean_global <- round(rbind(trim3_global_int, trim3_global_ctrl), digits = 2)
combined_trim3_sd_global <- round(rbind(trim3_sd_global_int, trim3_sd_global_ctrl), digits = 2)

global_effects <- as.data.frame(round(fixef(global_model), digits = 2))
global_effects <- global_effects[-c(1,3:4),]
sd_global_total <- sd(df_scored$global_psqi_score, na.rm = TRUE)
global_cohens_d <- round(abs(global_effects/sd_global_total), digits = 2)

global_se <- as.data.frame(round(sqrt(diag(vcov(global_model))), digits = 2))
global_se <- global_se[-c(1,3:4),]
global_p <- as.data.frame(round(global_results$"Pr(>Chisq)", digits = 2))
global_p <- global_p[-1,]

combined_global <- data.frame(Sleep_Outcome = c("Global PSQI Score (points)", ""),
                                Group = c("Intervention", "Control"),
                                Baseline = paste0(combined_baseline_mean_global, " (", combined_baseline_sd_global, ")"),
                                Second_Trimester = paste0(combined_trim2_mean_global, " (", combined_trim2_sd_global, ")"),
                                Third_Trimester = paste0(combined_trim3_mean_global, " (", combined_trim3_sd_global, ")"),
                                Estimate = c("", paste0(global_effects, " (", global_se, ")")),
                                p = c("", global_p),
                                Cohens_d = c("", global_cohens_d)
)


baseline_disturbances_ctrl <- mean(df_control1$disturbances_scored)
trim2_disturbances_ctrl <- mean(df_control2$disturbances_scored)
trim3_disturbances_ctrl <- mean(df_control3$disturbances_scored)
baseline_sd_disturbances_ctrl <- sd(df_control1$disturbances_scored)
trim2_sd_disturbances_ctrl <- sd(df_control2$disturbances_scored)
trim3_sd_disturbances_ctrl <- sd(df_control3$disturbances_scored)

baseline_disturbances_int <- mean(df_intervention1$disturbances_scored, na.rm = TRUE)
trim2_disturbances_int <- mean(df_intervention2$disturbances_scored, na.rm = TRUE)
trim3_disturbances_int <- mean(df_intervention3$disturbances_scored, na.rm = TRUE)
baseline_sd_disturbances_int <- sd(df_intervention1$disturbances_scored, na.rm = TRUE)
trim2_sd_disturbances_int <- sd(df_intervention2$disturbances_scored, na.rm = TRUE)
trim3_sd_disturbances_int <- sd(df_intervention3$disturbances_scored, na.rm = TRUE)

combined_baseline_mean_disturbances <- round(rbind(baseline_disturbances_int, baseline_disturbances_ctrl), digits = 2)
combined_baseline_sd_disturbances <- round(rbind(baseline_sd_disturbances_int, baseline_sd_disturbances_ctrl), digits = 2)
combined_trim2_mean_disturbances <- round(rbind(trim2_disturbances_int, trim2_disturbances_ctrl), digits = 2)
combined_trim2_sd_disturbances <- round(rbind(trim2_sd_disturbances_int, trim2_sd_disturbances_ctrl), digits = 2)
combined_trim3_mean_disturbances <- round(rbind(trim3_disturbances_int, trim3_disturbances_ctrl), digits = 2)
combined_trim3_sd_disturbances <- round(rbind(trim3_sd_disturbances_int, trim3_sd_disturbances_ctrl), digits = 2)

disturbances_effects <- as.data.frame(round(fixef(disturbances_model), digits = 2))
disturbances_effects <- disturbances_effects[-c(1,3:4),]
sd_disturbances_total <- sd(df_scored$disturbances_scored, na.rm = TRUE)
disturbances_cohens_d <- round(abs(disturbances_effects/sd_disturbances_total), digits = 2)

disturbances_se <- as.data.frame(round(sqrt(diag(vcov(disturbances_model))), digits = 2))
disturbances_se <- disturbances_se[-c(1,3:4),]
disturbances_p <- as.data.frame(round(disturbances_results$"Pr(>Chisq)", digits = 2))
disturbances_p <- disturbances_p[-1,]

combined_disturbances <- data.frame(Sleep_Outcome = c("Disturbances (points)", ""),
                              Group = c("Intervention", "Control"),
                              Baseline = paste0(combined_baseline_mean_disturbances, " (", combined_baseline_sd_disturbances, ")"),
                              Second_Trimester = paste0(combined_trim2_mean_disturbances, " (", combined_trim2_sd_disturbances, ")"),
                              Third_Trimester = paste0(combined_trim3_mean_disturbances, " (", combined_trim3_sd_disturbances, ")"),
                              Estimate = c("", paste0(disturbances_effects, " (", disturbances_se, ")")),
                              p = c("", disturbances_p),
                              Cohens_d = c("", disturbances_cohens_d)
)


