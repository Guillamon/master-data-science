#data dowlowaded from this source: https://calendar.zoznam.sk/icalendar/ical-en.php
#bonus: web scrape holiday dfs

#install.packages("ical")
#install.packages("rlist")
library(ical)
library(stringr)
library(dplyr)

library (tidyr)
library(rlist)

period <- "month"
#saving path, name of file and extension

filepath <- "C:/Users/Pepe/Documents/02 - Master - DS/TFM/Holidays/"
name <- "holidays-sk-"
extension <- ".ics"

#initializing empty list for storing dfs from loops
calendar_list <- list()

for (i in 2015:2019) {
  year_ <- as.character(i)
  filename <- print(paste0(filepath, name, year_, extension))
  
  cal_df <- ical::ical_parse_df(filename) %>%
    select(-status, -last.modified, -uid, -description) %>% 
    drop_na() %>%
    mutate(start= as_date(start)) %>% 
    mutate_at(vars(summary), funs(as.character(summary))) #converting factor column to characters
  
  #appending df to list  
  calendar_list <- list.append(calendar_list,cal_df)
}

full_calendar_df <- bind_rows(calendar_list) %>% #binding all dfs into one
  mutate(Easter = str_detect(summary,"pondelok|piatok") %>% as.integer()) %>% #creating Easter feature
  mutate(Period = floor_date(start, period) %>%  as_date()) %>%
  group_by(Period) %>% summarise(Easter = sum(Easter), Num_Holidays= n()) %>% 
  ungroup() %>% 
  mutate(Easter = ifelse (Easter>0,1,0)) #one hot encoding Easter holidays
