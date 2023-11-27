#Find means of sleep duration, global psqi scores, and sleep distrubances per visits by randomization
library(dplyr)
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


#Subjective Sleep Quality 
baseline_quality_ctrl <- mean(df_control1$quality_scored)
trim2_quality_ctrl <- mean(df_control2$quality_scored)
trim3_quality_ctrl <- mean(df_control3$quality_scored)
baseline_sd_quality_ctrl <- sd(df_control1$quality_scored)
trim2_sd_quality_ctrl <- sd(df_control2$quality_scored)
trim3_sd_quality_ctrl <- sd(df_control3$quality_scored)

baseline_quality_int <- mean(df_intervention1$quality_scored, na.rm = TRUE)
trim2_quality_int <- mean(df_intervention2$quality_scored, na.rm = TRUE)
trim3_quality_int <- mean(df_intervention3$quality_scored, na.rm = TRUE)
baseline_sd_quality_int <- sd(df_intervention1$quality_scored, na.rm = TRUE)
trim2_sd_quality_int <- sd(df_intervention2$quality_scored, na.rm = TRUE)
trim3_sd_quality_int <- sd(df_intervention3$quality_scored, na.rm = TRUE)

combined_baseline_mean_quality <- round(rbind(baseline_quality_int, baseline_quality_ctrl), digits = 2)
combined_baseline_sd_quality <- round(rbind(baseline_sd_quality_int, baseline_sd_quality_ctrl), digits = 2)
combined_trim2_mean_quality <- round(rbind(trim2_quality_int, trim2_quality_ctrl), digits = 2)
combined_trim2_sd_quality <- round(rbind(trim2_sd_quality_int, trim2_sd_quality_ctrl), digits = 2)
combined_trim3_mean_quality <- round(rbind(trim3_quality_int, trim3_quality_ctrl), digits = 2)
combined_trim3_sd_quality <- round(rbind(trim3_sd_quality_int, trim3_sd_quality_ctrl), digits = 2)

quality_effects <- as.data.frame(round(fixef(quality_model), digits = 2))
quality_effects <- quality_effects[-c(1,3:4),]
sd_quality_total <- sd(df_scored$quality_scored, na.rm = TRUE)
quality_cohens_d <- round(abs(quality_effects/sd_quality_total), digits = 2)

quality_se <- as.data.frame(round(sqrt(diag(vcov(quality_model))), digits = 2))
quality_se <- quality_se[-c(1,3:4),]
quality_p <- as.data.frame(round(quality_results$"Pr(>Chisq)", digits = 2))
quality_p <- quality_p[-1,]

combined_quality <- data.frame(Sleep_Outcome = c("Quality (points)", ""),
                                    Group = c("Intervention", "Control"),
                                    Baseline = paste0(combined_baseline_mean_quality, " (", combined_baseline_sd_quality, ")"),
                                    Second_Trimester = paste0(combined_trim2_mean_quality, " (", combined_trim2_sd_quality, ")"),
                                    Third_Trimester = paste0(combined_trim3_mean_quality, " (", combined_trim3_sd_quality, ")"),
                                    Estimate = c("", paste0(quality_effects, " (", quality_se, ")")),
                                    p = c("", quality_p),
                                    Cohens_d = c("", quality_cohens_d)
)

#Sleep latency 
baseline_latency_ctrl <- mean(df_control1$latency_scored)
trim2_latency_ctrl <- mean(df_control2$latency_scored)
trim3_latency_ctrl <- mean(df_control3$latency_scored)
baseline_sd_latency_ctrl <- sd(df_control1$latency_scored)
trim2_sd_latency_ctrl <- sd(df_control2$latency_scored)
trim3_sd_latency_ctrl <- sd(df_control3$latency_scored)

baseline_latency_int <- mean(df_intervention1$latency_scored, na.rm = TRUE)
trim2_latency_int <- mean(df_intervention2$latency_scored, na.rm = TRUE)
trim3_latency_int <- mean(df_intervention3$latency_scored, na.rm = TRUE)
baseline_sd_latency_int <- sd(df_intervention1$latency_scored, na.rm = TRUE)
trim2_sd_latency_int <- sd(df_intervention2$latency_scored, na.rm = TRUE)
trim3_sd_latency_int <- sd(df_intervention3$latency_scored, na.rm = TRUE)

combined_baseline_mean_latency <- round(rbind(baseline_latency_int, baseline_latency_ctrl), digits = 2)
combined_baseline_sd_latency <- round(rbind(baseline_sd_latency_int, baseline_sd_latency_ctrl), digits = 2)
combined_trim2_mean_latency <- round(rbind(trim2_latency_int, trim2_latency_ctrl), digits = 2)
combined_trim2_sd_latency <- round(rbind(trim2_sd_latency_int, trim2_sd_latency_ctrl), digits = 2)
combined_trim3_mean_latency <- round(rbind(trim3_latency_int, trim3_latency_ctrl), digits = 2)
combined_trim3_sd_latency <- round(rbind(trim3_sd_latency_int, trim3_sd_latency_ctrl), digits = 2)

latency_effects <- as.data.frame(round(fixef(latency_model), digits = 2))
latency_effects <- latency_effects[-c(1,3:4),]
sd_latency_total <- sd(df_scored$latency_scored, na.rm = TRUE)
latency_cohens_d <- round(abs(latency_effects/sd_latency_total), digits = 2)

latency_se <- as.data.frame(round(sqrt(diag(vcov(latency_model))), digits = 2))
latency_se <- latency_se[-c(1,3:4),]
latency_p <- as.data.frame(round(latency_results$"Pr(>Chisq)", digits = 2))
latency_p <- latency_p[-1,]

combined_latency <- data.frame(Sleep_Outcome = c("Latency (points)", ""),
                               Group = c("Intervention", "Control"),
                               Baseline = paste0(combined_baseline_mean_latency, " (", combined_baseline_sd_latency, ")"),
                               Second_Trimester = paste0(combined_trim2_mean_latency, " (", combined_trim2_sd_latency, ")"),
                               Third_Trimester = paste0(combined_trim3_mean_latency, " (", combined_trim3_sd_latency, ")"),
                               Estimate = c("", paste0(latency_effects, " (", latency_se, ")")),
                               p = c("", latency_p),
                               Cohens_d = c("", latency_cohens_d)
)


#habitual sleep Efficiecny
baseline_efficiency_ctrl <- mean(df_control1$efficiency)
trim2_efficiency_ctrl <- mean(df_control2$efficiency)
trim3_efficiency_ctrl <- mean(df_control3$efficiency)
baseline_sd_efficiency_ctrl <- sd(df_control1$efficiency)
trim2_sd_efficiency_ctrl <- sd(df_control2$efficiency)
trim3_sd_efficiency_ctrl <- sd(df_control3$efficiency)

baseline_efficiency_int <- mean(df_intervention1$efficiency, na.rm = TRUE)
trim2_efficiency_int <- mean(df_intervention2$efficiency, na.rm = TRUE)
trim3_efficiency_int <- mean(df_intervention3$efficiency, na.rm = TRUE)
baseline_sd_efficiency_int <- sd(df_intervention1$efficiency, na.rm = TRUE)
trim2_sd_efficiency_int <- sd(df_intervention2$efficiency, na.rm = TRUE)
trim3_sd_efficiency_int <- sd(df_intervention3$efficiency, na.rm = TRUE)

combined_baseline_mean_efficiency <- round(rbind(baseline_efficiency_int, baseline_efficiency_ctrl), digits = 2)
combined_baseline_sd_efficiency <- round(rbind(baseline_sd_efficiency_int, baseline_sd_efficiency_ctrl), digits = 2)
combined_trim2_mean_efficiency <- round(rbind(trim2_efficiency_int, trim2_efficiency_ctrl), digits = 2)
combined_trim2_sd_efficiency <- round(rbind(trim2_sd_efficiency_int, trim2_sd_efficiency_ctrl), digits = 2)
combined_trim3_mean_efficiency <- round(rbind(trim3_efficiency_int, trim3_efficiency_ctrl), digits = 2)
combined_trim3_sd_efficiency <- round(rbind(trim3_sd_efficiency_int, trim3_sd_efficiency_ctrl), digits = 2)

efficiency_effects <- as.data.frame(round(fixef(efficiency_model), digits = 2))
efficiency_effects <- efficiency_effects[-c(1,3:4),]
sd_efficiency_total <- sd(df_scored$efficiency, na.rm = TRUE)
efficiency_cohens_d <- round(abs(efficiency_effects/sd_efficiency_total), digits = 2)

efficiency_se <- as.data.frame(round(sqrt(diag(vcov(efficiency_model))), digits = 2))
efficiency_se <- efficiency_se[-c(1,3:4),]
efficiency_p <- as.data.frame(round(efficiency_results$"Pr(>Chisq)", digits = 2))
efficiency_p <- efficiency_p[-1,]

combined_efficiency <- data.frame(Sleep_Outcome = c("Efficiency (%)", ""),
                               Group = c("Intervention", "Control"),
                               Baseline = paste0(combined_baseline_mean_efficiency, " (", combined_baseline_sd_efficiency, ")"),
                               Second_Trimester = paste0(combined_trim2_mean_efficiency, " (", combined_trim2_sd_efficiency, ")"),
                               Third_Trimester = paste0(combined_trim3_mean_efficiency, " (", combined_trim3_sd_efficiency, ")"),
                               Estimate = c("", paste0(efficiency_effects, " (", efficiency_se, ")")),
                               p = c("", efficiency_p),
                               Cohens_d = c("", efficiency_cohens_d)
)

#Daytime dysfunction 
baseline_daytime_dysfunction_ctrl <- mean(df_control1$daytime_dysfunction_scored)
trim2_daytime_dysfunction_ctrl <- mean(df_control2$daytime_dysfunction_scored)
trim3_daytime_dysfunction_ctrl <- mean(df_control3$daytime_dysfunction_scored)
baseline_sd_daytime_dysfunction_ctrl <- sd(df_control1$daytime_dysfunction_scored)
trim2_sd_daytime_dysfunction_ctrl <- sd(df_control2$daytime_dysfunction_scored)
trim3_sd_daytime_dysfunction_ctrl <- sd(df_control3$daytime_dysfunction_scored)

baseline_daytime_dysfunction_int <- mean(df_intervention1$daytime_dysfunction_scored, na.rm = TRUE)
trim2_daytime_dysfunction_int <- mean(df_intervention2$daytime_dysfunction_scored, na.rm = TRUE)
trim3_daytime_dysfunction_int <- mean(df_intervention3$daytime_dysfunction_scored, na.rm = TRUE)
baseline_sd_daytime_dysfunction_int <- sd(df_intervention1$daytime_dysfunction_scored, na.rm = TRUE)
trim2_sd_daytime_dysfunction_int <- sd(df_intervention2$daytime_dysfunction_scored, na.rm = TRUE)
trim3_sd_daytime_dysfunction_int <- sd(df_intervention3$daytime_dysfunction_scored, na.rm = TRUE)

combined_baseline_mean_daytime_dysfunction <- round(rbind(baseline_daytime_dysfunction_int, baseline_daytime_dysfunction_ctrl), digits = 2)
combined_baseline_sd_daytime_dysfunction <- round(rbind(baseline_sd_daytime_dysfunction_int, baseline_sd_daytime_dysfunction_ctrl), digits = 2)
combined_trim2_mean_daytime_dysfunction <- round(rbind(trim2_daytime_dysfunction_int, trim2_daytime_dysfunction_ctrl), digits = 2)
combined_trim2_sd_daytime_dysfunction <- round(rbind(trim2_sd_daytime_dysfunction_int, trim2_sd_daytime_dysfunction_ctrl), digits = 2)
combined_trim3_mean_daytime_dysfunction <- round(rbind(trim3_daytime_dysfunction_int, trim3_daytime_dysfunction_ctrl), digits = 2)
combined_trim3_sd_daytime_dysfunction <- round(rbind(trim3_sd_daytime_dysfunction_int, trim3_sd_daytime_dysfunction_ctrl), digits = 2)

daytime_dysfunction_effects <- as.data.frame(round(fixef(daytime_dysfunction_model), digits = 2))
daytime_dysfunction_effects <- daytime_dysfunction_effects[-c(1,3:4),]
sd_daytime_dysfunction_total <- sd(df_scored$daytime_dysfunction_scored, na.rm = TRUE)
daytime_dysfunction_cohens_d <- round(abs(daytime_dysfunction_effects/sd_daytime_dysfunction_total), digits = 2)

daytime_dysfunction_se <- as.data.frame(round(sqrt(diag(vcov(daytime_dysfunction_model))), digits = 2))
daytime_dysfunction_se <- daytime_dysfunction_se[-c(1,3:4),]
daytime_dysfunction_p <- as.data.frame(round(daytime_dysfunction_results$"Pr(>Chisq)", digits = 2))
daytime_dysfunction_p <- daytime_dysfunction_p[-1,]

combined_daytime_dysfunction <- data.frame(Sleep_Outcome = c("Daytime Dysfunction (points)", ""),
                               Group = c("Intervention", "Control"),
                               Baseline = paste0(combined_baseline_mean_daytime_dysfunction, " (", combined_baseline_sd_daytime_dysfunction, ")"),
                               Second_Trimester = paste0(combined_trim2_mean_daytime_dysfunction, " (", combined_trim2_sd_daytime_dysfunction, ")"),
                               Third_Trimester = paste0(combined_trim3_mean_daytime_dysfunction, " (", combined_trim3_sd_daytime_dysfunction, ")"),
                               Estimate = c("", paste0(daytime_dysfunction_effects, " (", daytime_dysfunction_se, ")")),
                               p = c("", daytime_dysfunction_p),
                               Cohens_d = c("", daytime_dysfunction_cohens_d)
)

