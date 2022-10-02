# ANA 515 Assignment 3 Getting and Cleaning Data
# Mahsa Karkhaneh

library(tidyverse)
library(dplyr)
library(ggplot2)

#read Data
getwd()
storm_events_1991 <- read_csv("StormEvents_details-ftp_v1.0_d1991_c20220425.csv")
head(storm_events_1991,3)

#limite Data
myevent <-  storm_events_1991 %>% 
  select(BEGIN_DATE_TIME,END_DATE_TIME,EPISODE_ID,EVENT_ID,STATE,STATE_FIPS,CZ_NAME,
         CZ_TYPE,CZ_FIPS,EVENT_TYPE,SOURCE,BEGIN_LAT,BEGIN_LON,END_LAT,END_LON,BEGIN_YEARMONTH)

#arrange 
myevent1 <-arrange(myevent,BEGIN_YEARMONTH)

#rename
myevent1$state_title_case = str_to_title(myevent1$STATE, locale = "en")
myevent1

#filter
myevent2 <- myevent1 %>% 
  filter(CZ_TYPE == "C" )

#delete column
myevent3 <- select(myevent2,-CZ_TYPE)

#pad
myevent3$STATE_FIPS <- str_pad(myevent3$STATE_FIPS , width =3 , side = "left" , pad = "0"  )
myevent3$CZ_FIPS <- str_pad(myevent3$CZ_FIPS , width =3 , side = "left" , pad = "0"  )

#unite
myevent4 <- unite(myevent3, col = "fibs" , c('STATE_FIPS','CZ_FIPS') , sep = "", remove = TRUE)
head(myevent4,2)

#rename column
myevent5 <- rename_all(myevent4, tolower)
head(myevent5,2)

#new data frame
data("state")
us_state_info<-data.frame(state=state.name, region=state.region, area=state.area)
us_state_info1 <-mutate_all(us_state_info, toupper) 
head(us_state_info1,2)

#number of events
N_events <- data.frame(table(myevent5$state))
N_events1<-rename(N_events, c("state"="Var1"))
head(N_events1,5)

#Merge
Merged <- merge(x = N_events1, y = us_state_info1, by.x = "state", by.y = "state" )
head(Merged,5)

#plot

myplot <- ggplot(Merged,aes(x = area, y = Freq )) + 
  geom_point(aes(color = region, size = 5)) + 
  labs (x = "Land area(square miles)", y = "# storm events in 1991")
myplot

