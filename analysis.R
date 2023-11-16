load(file = "df_scored.RData")

#install.packages("lme4")
library("lme4")

global_model <- lmer(global_psqi_score ~ 1 + 
       randomization + (1 | record_id) +
       (1 | redcap_event_name), data = df_scored, subset = redcap_event_name == c("visit_2_arm_1", "visit_3_arm_1"))


lmer(quality_scored ~ 1 + 
       randomization + (1 | record_id) +
       (1 | redcap_event_name), data = df_scored, subset = redcap_event_name == c("visit_2_arm_1", "visit_3_arm_1"))

lmer(psqi_fallasleep ~ 1 + 
       randomization + (1 | record_id) +
       (1 | redcap_event_name), data = df_scored, subset = redcap_event_name == c("visit_2_arm_1", "visit_3_arm_1"))

lmer(psqi_sleep ~ 1 + 
       randomization + (1 | record_id) +
       (1 | redcap_event_name), data = df_scored, subset = redcap_event_name == c("visit_2_arm_1", "visit_3_arm_1"))

lmer(efficiency ~ 1 + 
       randomization + (1 | record_id) +
       (1 | redcap_event_name), data = df_scored, subset = redcap_event_name == c("visit_2_arm_1", "visit_3_arm_1"))

lmer(disturbances_scored ~ 1 + 
       randomization + (1 | record_id) +
       (1 | redcap_event_name), data = df_scored, subset = redcap_event_name == c("visit_2_arm_1", "visit_3_arm_1"))

lmer(meds_scored ~ 1 + 
       randomization + (1 | record_id) +
       (1 | redcap_event_name), data = df_scored, subset = redcap_event_name == c("visit_2_arm_1", "visit_3_arm_1"))

lmer(daytime_dysfunction_scored ~ 1 + 
       randomization + (1 | record_id) +
       (1 | redcap_event_name), data = df_scored, subset = redcap_event_name == c("visit_2_arm_1", "visit_3_arm_1"))
