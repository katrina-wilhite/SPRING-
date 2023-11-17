load(file = "df_complete.RData")

#install.packages("lme4")
library("lme4")

df_complete <- df_complete[which(complete.cases(df_complete[,"global_psqi_score"])),]

global_model <- lmer(global_psqi_score ~ randomization + 
       (1 | record_id), data = df_complete, REML = FALSE)

global_reduced <- lmer(global_psqi_score ~  + 
       (1 | record_id), data = df_complete, REML = FALSE)

anova(global_reduced,global_model)

coef(global_model)
anova(global_model)
summary(global_model)

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
