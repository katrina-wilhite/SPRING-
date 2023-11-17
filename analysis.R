load(file = "df_complete.RData")

#install.packages("lme4")
library("lme4")

df_complete <- df_complete[which(complete.cases(df_complete[,"global_psqi_score"])),]

#Run mixed effects model for global PSQI score
##Build full model
global_model <- lmer(global_psqi_score ~ randomization + global_psqi_score_baseline +
       (1 | record_id), data = df_complete, REML = FALSE)
##Build reduced model
global_reduced <- lmer(global_psqi_score ~  + 
       (1 | record_id), data = df_complete, REML = FALSE)
##Compare models 
global_results <- anova(global_reduced,global_model)

#Repeat for sleep quality 
quality_model <- lmer(quality_scored ~ randomization + 
                       (1 | record_id), data = df_complete, REML = FALSE)
##Build reduced model
quality_reduced <- lmer(quality_scored ~  + 
                         (1 | record_id), data = df_complete, REML = FALSE)
##Compare models 
quality_results <-anova(quality_reduced,quality_model)

#Repeat for sleep latency 
latency_model <- lmer(latency_scored ~ randomization + 
                        (1 | record_id), data = df_complete, REML = FALSE)
##Build reduced model
latency_reduced <- lmer(latency_scored ~  + 
                          (1 | record_id), data = df_complete, REML = FALSE)
##Compare models 
latency_results <-anova(latency_reduced,latency_model)

#Repeat for sleep duration 
duration_model <- lmer(psqi_sleep ~ randomization + 
                        (1 | record_id), data = df_complete, REML = FALSE)
##Build reduced model
duration_reduced <- lmer(psqi_sleep ~  + 
                          (1 | record_id), data = df_complete, REML = FALSE)
##Compare models 
duration_results <-anova(duration_reduced,duration_model)

#Repeat for sleep efficiency 
efficiency_model <- lmer(efficiency ~ randomization + 
                         (1 | record_id), data = df_complete, REML = FALSE)
##Build reduced model
efficiency_reduced <- lmer(efficiency ~  + 
                           (1 | record_id), data = df_complete, REML = FALSE)
##Compare models 
efficiency_results <-anova(efficiency_reduced,efficiency_model)

#Repeat for sleep disturbances 
disturbances_model <- lmer(disturbances_scored ~ randomization + 
                           (1 | record_id), data = df_complete, REML = FALSE)
##Build reduced model
disturbances_reduced <- lmer(disturbances_scored ~  + 
                             (1 | record_id), data = df_complete, REML = FALSE)
##Compare models 
disturbances_results <-anova(disturbances_reduced,disturbances_model)

#Repeat for daytime dysfunction 
daytime_dysfunction_model <- lmer(daytime_dysfunction_scored ~ randomization + 
                             (1 | record_id), data = df_complete, REML = FALSE)
##Build reduced model
daytime_dysfunction_reduced <- lmer(daytime_dysfunction_scored ~  + 
                               (1 | record_id), data = df_complete, REML = FALSE)
##Compare models 
daytime_dysfunction_results <-anova(daytime_dysfunction_reduced,daytime_dysfunction_model)