setwd("C:/Users/katri/Documents/WVU/R Projects/SPRING-/")
SPRING <- `SPRING data`
save(SPRING, file="SPRING.Rda")
View(SPRING[,1:50])
View(SPRING[,51:100])
View(SPRING[,101:150])
View(SPRING[,151:200])
View(SPRING[,201:ncol(SPRING)])

#Open appropriate libraries 
library("dplyr")

#Select relevant columns 
df <- SPRING %>% 
  select(c(record_id:redcap_event_name, psqi_bedtime:psqi_enthusiasm, randomization)) %>% 
  subset(redcap_event_name != "medical_chart_abst_arm_1" & !is.na(randomization) & !is.na(psqi_sleep))

##Run analysis again while removing participatings with missing follow-up data; as a sensitivty analysis 
SPRING %>% 
  select(c(record_id:redcap_event_name, psqi_bedtime:psqi_enthusiasm, randomization)) %>% 
  subset(redcap_event_name != "medical_chart_abst_arm_1" & !is.na(randomization) & is.na(psqi_sleep))

save(df, file = 'df.RData', envir = globalenv())

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

summary(df_complete)