library("ggplot2")
library(dplyr)

df_scored$redcap_event_name_recoded <- df_scored$redcap_event_name
df_scored$redcap_event_name_recoded <- recode(df_scored$redcap_event_name_recoded,
                            "baseline_arm_1" = 1,
                            "visit_2_arm_1" = 2,
                            "visit_3_arm_1" = 3)

ggplot(data = df_scored, aes(
  x= redcap_event_name_recoded, y = psqi_sleep, group = randomization) + 
    geom_line() 