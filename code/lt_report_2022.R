#################################################
# Title: MS Code - Visitation and Long Term Monitoring
# Purpose: analyses and figures for manuscript
# Author: LS & LP
# Created: 6/11/21
# Last edited: 3/28/21
##################################################

##### load packages #####

library(readxl)     # load excel files
library(janitor)    # tidy data
library(broom)      # glance command   
library(PNWColors)  # color palette
library(lubridate)  # parse dates and times
library(vegan)      # multivariate toolbox
library(gt)         # neat tables
library(tidyverse)  # tidyverse packages

##### presets #####

cabrsites <- c('CAB1', 'CAB2', 'CAB3') 
zonenames <- c('Balanus/Chthamalus', 'Tetraclita rubescens', 'Mussel', 'Silvetia compressa', 'Pollicipes polymerus')
targetlist <- c('CHTBAL', 'TETRUB', 'MUSSEL', 'SILCOM', 'POLPOL')

###### plot theme #####

custom_theme <- theme(text = element_text(size = 12),
                                    # add more space between panels
                                    panel.spacing = unit(1, 'lines'),
                                    # no background to wrap panels
                                    strip.background = element_blank(),
                                    strip.text = element_text(size = 12, 
                                                              hjust = 0),
                                    # panel labels outside x axis labels
                                    strip.placement = 'outside',
                                    # adjust x axis labels
                                    axis.text.y = element_text(size = 12),
                                    axis.text.x = element_text(size = 12, 
                                                               angle = 45, 
                                                               hjust = 1))

##### load data #####

# visitation data - counts of shorebirds and visitors across zones
people <- read_excel('./data/Shorebird_People_Data/Shorebird_People_Data.xlsx')

# target data - MARINe photoplot data with % cover over time of target taxa
cover <- read_excel('./data/RI_Long_Term_Data/Photoplot_summary_by_plot_20210414.xlsx')

# aligns target taxa lists over time (converts all newer taxa back to categories from 1990)
align <- read_csv('./data/TGT_all.csv')

##### tidy data #####

### people data
people <- people %>% 
  # add year column
  mutate(SurveyYear = year(SurveyDate))%>%
  # filter for only visitor data
  filter(DataType == "People")%>%
  # calculate mean visitor count for each year and zone
  group_by(SurveyYear, ZoneClass) %>%
  summarise(people = mean(DataCount)) %>%
  # tidy up zone names for better figs
  mutate(ZoneName= paste('Zone', ZoneClass),
         Panel = if_else(ZoneName == 'Zone I', 'A', 
                         if_else(ZoneName == 'Zone II', 'B', 'C'))) %>%
  mutate(ZoneName = paste('(', Panel, ') ', ZoneName, sep = '')) %>%
  select(-c(Panel))%>%
  # remove rows with all values NA
  drop_na() %>%
  ungroup() %>%
  # make col names consistent (snake case)
  clean_names()

### photo-plot cover data 

# qa/qc notes: no data for 2018 or 2009
# exclude recently added plots: zone 2 Po7-8; z1 m6 
# po plots weren't established until 1995

cover <- 
  # add generic codes to this set, along with formatted scientific names
  left_join(cover, align, by = 'SpeciesCode') %>%
  # clean up names
  clean_names() %>%
  # only get cabr sites
  filter(site_code %in% cabrsites) %>%
  # use only fall data (spring sampling stopped in 2017) 
  filter(season_name == 'Fall') %>%
  # remove mytilus plot 6 in z1 and pollicipes plots 7+8 from Z2
  filter(!plot_code %in% c('POL-07', 'POL-08', 'MYT-06')) %>%
  # get rid of old scientific name (align has consistently formatted ones)
  select(-c(scientific_name_x)) %>%
  # rename scientific name column
  rename(scientific_name = scientific_name_y) %>%
  # make plot number a numeric column
  mutate(plot_num = parse_number(substr(plot_code, 6,6))) %>%
  # add zone/site column
  mutate(zone_name = paste('Zone', substr(site_name, 10, 12)),
         panel = if_else(site_name == 'Cabrillo I', 'A', 
                         if_else(site_name == 'Cabrillo II', 'B', 'C'))) %>%
  mutate(ZoneName = paste('(', panel, ') ', zone_name, sep='')) %>%
  # only get unique rows (removes duplicates)
  distinct() %>%
  ungroup()

# get sum of number of points for each species
cover90 <- cover %>%
  group_by(site_code, site_name, zone_name, survey_year, zone, plot_num, plot_code, code_1990) %>%
  summarise(cover = sum(n))

