load(file = "df.RData")
library(dplyr)
library(lubridate)

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
df$psqi_bedtime <- sub("(\\d{1,2}:\\d{2}$)", "\\1 PM", df$psqi_bedtime)
df$psqi_bedtime <- sub("p(?!m)", "pm", df$psqi_bedtime, perl = TRUE)
df$psqi_bedtime <- sub("pm\\.m\\.", "pm", df$psqi_bedtime)
df$psqi_bedtime <- gsub(" ", "", df$psqi_bedtime, ignore.case = TRUE) 
df$psqi_bedtime <- tolower(df$psqi_bedtime)
df$psqi_bedtime <- sub("(\\d{1,2})(\\d{2})pm", "\\1:\\2pm", df$psqi_bedtime)
df$psqi_bedtime <- sub("(\\d{1,2})([ap]m)$", "\\1:00\\2", df$psqi_bedtime)
df$psqi_bedtime[df$psqi_bedtime == "10:00pmn"] <- "10:00pm"
#Remove time for 8am because this was likely a typo - getup time was 5am so hard to determine which time was a typo (e.g., potentially a shift worker) 
df$psqi_bedtime[df$psqi_bedtime == "8:00:00am"] <- ""
df$psqi_bedtime[df$psqi_bedtime == "11:00"] <- "11:00pm"
as.numeric(sapply(strptime(df$psqi_bedtime, "%I:%M:%S%p", tz = "UTC"), as.POSIXct))
formats <- c("%I:%M%p","%I:%M:%S%p")
df$psqi_bedtime <- parse_date_time(df$psqi_bedtime, orders = formats)
#Add 1 day for people who went to bed at or before midnight 
after_midnight <- df$psqi_bedtime < "0000-01-01 12:00:00 UTC"
if (df$psqi_bedtime < "0000-01-01 12:00:00 UTC") {
  print(df$psqi_bedtime + days(1))
} 

df$psqi_getup <- gsub(" ", "", df$psqi_getup, ignore.case = TRUE) 
df$psqi_getup <- tolower(df$psqi_getup)
df$psqi_getup[df$psqi_getup == "530am"] <- "5:30am"
df$psqi_getup[df$psqi_getup == "545am"] <- "5:45am"
formats <- c("%I:%M%p","%I%M%p", "%I%M", "%I%p", "%I")
df$psqi_getup <- parse_date_time(df$psqi_getup, orders = formats)
df$psqi_getup <- df$psqi_getup + days(1)

#Habitual sleep efficiency: calculate number of hours spent in bed (question 3 - question 1) - psqi_bedtime & psqi_getup
df <- df %>% 
  mutate(hours_in_bed = difftime(psqi_getup, psqi_bedtime, units = "hours"))


#Habitual sleep efficiency: (psqi_sleep/hours spent in bed) X 100 = % 


#Habitual sleep efficiency: assign component score 


#Sleep disturbances: questions 5b-5j - psqi_middle:psqi_other; assign component subscores for each 


#Sleep disturbances: Sum scores of psqi_middle:psqi_other 


#Sleep disturbances: assign component score 


#Use of sleep medication: Question 7 - psqi_meds; assign component score 


#Daytime dysfunction: Questions 8 and 9; psqi_stayawake & psqi_enthusiasm; assign component subscores for each 


#Daytime dysfunction: sum scores of psqi_stayawake & psqi_enthusiasm 


#Daytime dysfunction: assign component score 


#Global PSQI Score: sum all 7 components together 