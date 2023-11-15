load(file = "df.RData")
library(dplyr)
#install.packages("lubridate")
library(lubridate)
#install.packages("hms")
library(hms)

#Subjective sleep quality: Question 6 - psqi_quality; assign component score
df$quality_scored <- df$psqi_quality
df$quality_scored <- recode(df$quality_scored,
                          "Very Good" = 0,
                          "Fairly Good" = 1,
                          "Fairly Bad" = 2,
                          "Very Bad" = 3)

#Sleep latency: Questions 2 and 5a - psqi_fallasleep & psqi_30min; assign component subscore
df$fallasleep_scored <- df$psqi_fallasleep
breakpoints_latency1 <- c(-Inf, 15, 30, 60, Inf)
df$fallasleep_scored <- cut(df$fallasleep_scored, breaks = breakpoints_latency1, labels = c(0,1,2,3), include.lowest = TRUE)
df$fallasleep_scored <- as.numeric(df$fallasleep_scored)

df$psqi30_scored <- df$psqi_30min
df$psqi30_scored <- recode(df$psqi30_scored,
                           "Not during the past month" = 0,
                           "Less than once a week" = 1, 
                           "Once or twice a week" = 2, 
                           "Three or more times a week" = 3)

##Sleep latency; sum question 2 and 5a
df <- df %>% 
  mutate(latency_sum = (fallasleep_scored + psqi30_scored))

##Sleep latency; assign component score 
df$latency_scored <- df$latency_sum
breakpoints_latency2 <- c(-Inf, 0, 2, 4, 6)
df$latency_scored <- cut(df$latency_scored, breaks = breakpoints_latency2, labels = c(0,1,2,3), include.lowest = TRUE)

#Sleep duration: Question 4 - psqi_sleep; assign component score 
df$sleep_scored <- df$psqi_sleep
breakpoints_duration <- c(-Inf, 4.999, 5.999, 6.999, Inf)
df$sleep_scored <- cut(df$sleep_scored, breaks = breakpoints_duration, labels = c(3,2,1,0), include.lowest = TRUE)

#Habitual sleep efficiency; first clean bedtime and getup times psqi_bedtime & psqi_getup
##add "pm" to values that don't have it
df$bedtime_clean <- df$psqi_bedtime
df$bedtime_clean <- sub("(\\d{1,2}:\\d{2}$)", "\\1 PM", df$bedtime_clean)
## change 'p' to 'pm'
df$bedtime_clean <- sub("p(?!m)", "pm", df$bedtime_clean, perl = TRUE)
##change 'p.m.' to 'pm'
df$bedtime_clean <- sub("pm\\.m\\.", "pm", df$bedtime_clean)
##remove whitespace
df$bedtime_clean <- gsub(" ", "", df$bedtime_clean, ignore.case = TRUE) 
#Remove leading 0s
df$bedtime_clean <- sub("^0+", "", df$bedtime_clean)
##make all characters lowercase
df$bedtime_clean <- tolower(df$bedtime_clean)
##Fix typos
df$bedtime_clean[df$bedtime_clean == "10:00pmn"] <- "10:00pm"
df$bedtime_clean[df$bedtime_clean == "11:00"] <- "11:00pm"
df$bedtime_clean[df$bedtime_clean == "10.00pm"] <- "10:00pm"
df$bedtime_clean[df$bedtime_clean == "900pm"] <- "9:00pm"
#Remove time for 8am because this was likely a typo - getup time was 5am so hard to determine which time was a typo (e.g., potentially a shift worker) 
df$bedtime_clean[df$bedtime_clean == "8:00am"] <- ""
##covert characters to datetime objects
formats <- c("%-I:%M%p","%-I%M%p", "-%I%p")
df$bedtime_clean <- parse_date_time(df$bedtime_clean, orders = formats)
#Add a "day" to all people who went to bed after midnight
after_midnight <- df$bedtime_clean < "0000-01-01 12:00:00 UTC" & !is.na(df$bedtime_clean)
df$bedtime_clean[after_midnight] <- df$bedtime_clean[after_midnight] + days(1)

##Clean getup data the same way as sleep data 
df$getup_clean <- df$psqi_getup
df$getup_clean <- sub("(\\d{1,2}:\\d{2}$)", "\\1 AM", df$getup_clean)
df$getup_clean <- sub("a(?!m)", "am", df$getup_clean, perl = TRUE)
df$getup_clean <- sub("am\\.m\\.", "am", df$getup_clean)
df$getup_clean <- gsub(" ", "", df$getup_clean, ignore.case = TRUE) 
df$getup_clean <- sub("^0+", "", df$getup_clean)
df$getup_clean <- tolower(df$getup_clean)
df$getup_clean[df$getup_clean == "6"] <- "6:00am"
df$getup_clean[df$getup_clean == "530am"] <- "5:30am"
df$getup_clean[df$getup_clean == "545am"] <- "5:45am"
df$getup_clean[df$getup_clean == "9:45"] <- "9:45am"
df$getup_clean[df$getup_clean == "800am"] <- "8:00am"
formats <- c("%-I:%M%p","%-I%M%p", "%-I%p")
df$getup_clean <- parse_date_time(df$getup_clean, orders = formats)
##Add one "day" to all values since they woudl have woken up the following day 
df$getup_clean <- df$getup_clean + days(1)

#Habitual sleep efficiency: calculate number of hours spent in bed (question 3 - question 1) - psqi_bedtime & psqi_getup
df <- df %>% 
  mutate(hours_in_bed = (getup_clean - bedtime_clean))
df$hours_in_bed

#Habitual sleep efficiency: (psqi_sleep/hours spent in bed) X 100 = % 


#Habitual sleep efficiency: assign component score 


#Sleep disturbances: questions 5b-5j - psqi_middle:psqi_other2; assign component subscores for each 
##First, move psqi_other as this is optional and a character column 
df <- relocate(df, psqi_other, .after = psqi_other2)
for (i in 8:16(df)) {
  df[,i] <- recode(df[,i],
         "Not during the past month" = 0,
         "Less than once a week" = 1, 
         "Once or twice a week" = 2, 
         "Three or more times a week" = 3)
}


#Sleep disturbances: Sum scores of psqi_middle:psqi_other2 


#Sleep disturbances: assign component score 


#Use of sleep medication: Question 7 - psqi_meds; assign component score 
df$meds_scored <- df$psqi_meds
df$meds_scored <- recode(df$meds_scored,
                           "Not during the last month" = 0,
                           "Less than once a week" = 1, 
                           "Once or twice a week" = 2, 
                           "Three or more times a week" = 3)

#Daytime dysfunction: Questions 8 and 9; psqi_stayawake & psqi_enthusiasm; assign component subscores for each 
df$psqi_stayawake_scored <- df$psqi_stayawake
df$psqi_stayawake_scored <- recode(df$psqi_stayawake_scored,
                                   "Not during the last month" = 0,
                                   "Less than once a week" = 1, 
                                   "Once or twice a week" = 2, 
                                   "Three or more times a week" = 3)

df$enthusiasm_scored <- df$psqi_enthusiasm
df$enthusiasm_scored <- recode(df$enthusiasm_scored,
                                   "No problem at all" = 0,
                                   "Only a very slight problem" = 1, 
                                   "Somewhat of a problem" = 2, 
                                   "A very big problem" = 3)

#Daytime dysfunction: sum scores of psqi_stayawake & psqi_enthusiasm 
df <- df %>% 
  mutate(daytime_dysfunction_sum = (psqi_stayawake_scored + psqi_enthusiasm_scored))

#Daytime dysfunction: assign component score 
df$daytime_dysfunction_sum <- df$daytime_dysfunction_sum
breakpoints_dysfunction <- c(-Inf, 0, 2, 4, 6)
df$daytime_dysfunction_scored <- cut(df$daytime_dysfunction_sum, breaks = breakpoints_dysfunction, labels = c(0,1,2,3), include.lowest = TRUE)

#Global PSQI Score: sum all 7 components together 
df <- df %>% 
  mutate(global_psqi_score = (quality_scored + latency_scored + sleep_scored + efficiency_scored + disturbances_scored + meds_scored +  daytime_dysfunction_scored))