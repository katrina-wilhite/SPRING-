library(tidyr)

#Make new dataframe with just baseline data 
df_baseline <- df_scored %>% 
  filter(redcap_event_name == "baseline_arm_1") %>% 
  select(record_id, quality_scored, fallasleep_scored, psqi30_scored, latency_sum, latency_scored, psqi_sleep, sleep_scored, efficiency, efficiency_scored, disturbances_sum, disturbances_scored, meds_scored, daytime_dysfunction_sum, daytime_dysfunction_scored, global_psqi_score)

colnames(df_baseline)[2:16] <- paste(colnames(df_baseline)[2:16], 'baseline', sep = '_')

df_follow_up <- subset(df_scored, redcap_event_name != "baseline_arm_1")

df_complete <- left_join(df_follow_up, df_baseline, by='record_id')