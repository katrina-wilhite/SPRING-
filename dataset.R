SPRING <- `SPRING data`
View(SPRING[,1:50])
View(SPRING[,51:100])
View(SPRING[,101:150])
View(SPRING[,151:200])
View(SPRING[,201:ncol(SPRING)])

#Open appropriate libraries 
library("dplyr")

#Select relevant columns 
df <- SPRING %>% 
  select(c(record_id:redcap_event_name, psqi_bedtime:psqi_enthusiasm, steps_day:sed_min, wear_min:weardays, cr_prepregwt_kgs, gawks_v1:af_gadays_v3, livebirths, meds, randomization, af_height_feet:af_height_inches))

save(df, file = 'df.RData', envir = globalenv())




