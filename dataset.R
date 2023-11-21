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

save(df, file = 'df.RData', envir = globalenv())
