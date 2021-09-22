#################################################
# Title: LCD Data Queries for LS
# Purpose: get abiotic data, tidy for use in eda
# Author: LP
# Created: 6/9/21
# Last edited: 7/13/21
##################################################

##### packages #####

library(janitor) # clean up data
library(rnoaa) # buoy data/weather data
library(lubridate) # dates/times
library(tidyverse) # always

##### load abiotic data (comment out after first run) #####

# create list of study years

studyyear <- c(1990:2020)

# pull data, loop over study years, save to folder

for (i in 1:length(studyyear)){
  
  # pull data for a given study year, name it 'weather'
  weather <- lcd(station = '72290023188',
                 year = studyyear[i])
  
  # write a csv with a file name that contains the year 
  # note: change the part of the path before the first "/" to desired file location
  write_csv(weather, paste('SD_Weather_Data/SD_Intl_Airport_', studyyear[i], '.csv', sep= ''))
  
}
