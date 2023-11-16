load(file = "df_complete.RData")

#install.packages("lme4")
library("lme4")

global_model <- lmer(global_psqi_score ~ 1 + 
       randomization + (1 | record_id) +
       (1 | redcap_event_name), data = df_complete)


lmer(quality_scored ~ 1 + 
       randomization + (1 | record_id) +
       (1 | redcap_event_name), data = df_complete)

lmer(psqi_fallasleep ~ 1 + 
       randomization + (1 | record_id) +
       (1 | redcap_event_name), data = df_complete)

lmer(psqi_sleep ~ 1 + 
       randomization + (1 | record_id) +
       (1 | redcap_event_name), data = df_complete)

lmer(efficiency ~ 1 + 
       randomization + (1 | record_id) +
       (1 | redcap_event_name), data = df_complete)

lmer(disturbances_scored ~ 1 + 
       randomization + (1 | record_id) +
       (1 | redcap_event_name), data = df_complete)

lmer(meds_scored ~ 1 + 
       randomization + (1 | record_id) +
       (1 | redcap_event_name), data = df_complete)

lmer(daytime_dysfunction_scored ~ 1 + 
       randomization + (1 | record_id) +
       (1 | redcap_event_name), data = df_complete)
