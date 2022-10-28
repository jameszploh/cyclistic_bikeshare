# Clearing memory

rm(list = ls())

# Installing required packages

library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)

# Checking column names to see if they match

colnames(`202110.divvy.tripdata_changed`)
colnames(`202111.divvy.tripdata_changed`)
colnames(`202112.divvy.tripdata_changed`)
colnames(`202201.divvy.tripdata_changed`)
colnames(`202202.divvy.tripdata_changed`)
colnames(`202203.divvy.tripdata_changed`)
colnames(`202204.divvy.tripdata_changed`)
colnames(`202205.divvy.tripdata_changed`)
colnames(`202206.divvy.tripdata_changed`)
colnames(`202207.divvy.tripdata_changed`)
colnames(`202208.divvy.tripdata_changed`)
colnames(`202209.divvy.tripdata_changed`)

# Checking structure of data

str(`202110.divvy.tripdata_changed`)
str(`202111.divvy.tripdata_changed`)
str(`202112.divvy.tripdata_changed`)
str(`202201.divvy.tripdata_changed`)
str(`202202.divvy.tripdata_changed`)
str(`202203.divvy.tripdata_changed`)
str(`202204.divvy.tripdata_changed`)
str(`202205.divvy.tripdata_changed`)
str(`202206.divvy.tripdata_changed`)
str(`202207.divvy.tripdata_changed`)
str(`202208.divvy.tripdata_changed`)
str(`202209.divvy.tripdata_changed`)

# Combining into one dataframe

all_trips<-bind_rows(`202110.divvy.tripdata_changed`,
                     `202111.divvy.tripdata_changed`,
                     `202112.divvy.tripdata_changed`,
                     `202201.divvy.tripdata_changed`,
                     `202202.divvy.tripdata_changed`,
                     `202203.divvy.tripdata_changed`,
                     `202204.divvy.tripdata_changed`,
                     `202205.divvy.tripdata_changed`,
                     `202206.divvy.tripdata_changed`,
                     `202207.divvy.tripdata_changed`,
                     `202208.divvy.tripdata_changed`,
                     `202209.divvy.tripdata_changed`)

# Removing unnecessary columns

all_trips<-all_trips %>% 
  select(-c(start_lat,start_lng,end_lat,end_lng))

# Checking new dataframe

colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)
table(all_trips$member_casual) #checking member type

# Creating columns for date, month, day, year, day of week

all_trips$date<-as.Date(all_trips$started_at) #default format is yyyy-mm-dd
all_trips$month<-format(as.Date(all_trips$date),"%m")
all_trips$day<-format(as.Date(all_trips$date),"%d")
all_trips$year<-format(as.Date(all_trips$date),"%Y")
all_trips$day_of_week<-format(as.Date(all_trips$date),"%A") 
all_trips$ride_length<-difftime(all_trips$ended_at,all_trips$started_at) #no need to run this line, added in Excel
is.factor(all_trips$ride_length) #checking if factor or numeric
all_trips$ride_length<-as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length) #checking if numeric for calculations

# Creating separate dataframe after removing missing and bad data

all_trips_v2<-all_trips[!(all_trips$start_station_name == "HQ QR"| all_trips$ride_length<0),]
all_trips_v2<-na.omit(all_trips_v2) #same result as removing unnecessary columns
all_trips_v2<-all_trips_v2[!(all_trips_v2$rideable_type == "docked_bike"),]
summary(all_trips_v2$ride_length) #summary for ride length

# Comparing members and casual users

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)

#Ordering by day of week

all_trips_v2$day_of_week<-ordered(all_trips_v2$day_of_week,levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")) #ordering days of the week

#Average ride time by each day for members vs casual users

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Analyse ridership by type and weekday

all_trips_v2 %>% 
  mutate(weekday=wday(started_at,label = TRUE)) %>% #creates weekday field using wday()
  group_by(member_casual,weekday) %>% #groups by user type and weekday
  summarise(number_of_rides=n() #calculates number of rides and average duration and sorts
            ,average_duration = mean(ride_length)) %>% arrange(member_casual,weekday)

# Viz number of rides by rider type

all_trips_v2 %>% 
  mutate(weekday=wday(started_at,label = TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides=n(),
            average_duration=mean(ride_length)) %>% 
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual))+
  geom_col(position = "dodge")

# Viz for average duration

all_trips_v2 %>% 
  mutate(weekday=wday(started_at,label = TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides=n(),
            average_duration=mean(ride_length)) %>% 
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x=weekday,y = average_duration,fill = member_casual)) +
  geom_col(position = "dodge")

#Exporting
counts<-aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual+
                    all_trips_v2$day_of_week,FUN = mean)
write.csv(counts,file= "C:/Users/james/Downloads/avg_ride_length.csv")
cyclistic_output<-aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual+all_trips_v2$rideable_type+all_trips_v2$started_at+all_trips_v2$ended_at+all_trips_v2$start_station_name+all_trips_v2$end_station_name+all_trips_v2$day_of_week+all_trips_v2$month,FUN = mean)
write.csv(cyclistic_output,file= "C:/Users/james/Downloads/Cyclistic_bikeshare.csv")
