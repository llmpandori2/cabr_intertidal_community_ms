#################################################
# Title: MS Code - Visitation and Long Term Monitoring
# Purpose: analyses and figures for manuscript
# Author: LS & LP
# Created: 6/11/21
# Last edited: 12/8/21
##################################################

##### load packages #####

library(readxl)     # load excel file
#library(grDevices)
#library(patchwork) # arrange plots
library(janitor)    # clean up messy datasets + names
library(ggdark)     # dark theme plots for presentations
#library(Hmisc)      
library(broom)      # glance command for linregs   
library(viridis)    # color palette
library(lubridate)  # deal with dates and times
library(vegan)      # multivariate toolbox
library(gt)         # generate tables w/ gg framework
#library(ggpubr)
#library(rstatix)
#library(labdsv) #hellinger transformation
library(tidyverse)  # all things tidy

##### load data #####

setwd('D:/LP_Files/RStudio_Files/cabr_intertidal_community_ms/data')

# visitation data (from shorebird and visitor count dataset)
shorebird <- read_excel('Shorebird_People_Data/Shorebird_People_Data.xlsx')

# target data (taxa lists differ across years - use align to standardize)
target <- read_excel('RI_Long_Term_Data/Photoplot_summary_by_plot_20210414.xlsx')

# aligns taxa lists over time (converts all newer taxa back to categories from 1990)
align <- read_csv('TGT_all.csv')

# lists of commonly filtered-for items
cabrsites <- c('CAB1', 'CAB2', 'CAB3') 
zonenames <- c('Balanus/Chthamalus', 'Tetraclita rubescens', 'Mussel', 'Silvetia compressa', 'Pollicipes polymerus')
targetlist <- c('CHTBAL', 'TETRUB', 'MUSSEL', 'SILCOM', 'POLPOL')

# not in operator
`%notin%` <- Negate(`%in%`)

# theme
lltheme_light <- theme_bw() + theme(text = element_text(size = 12),
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

lltheme_dark <- dark_theme_bw() + theme(text = element_text(size = 12),
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

##### tidy "shorebird" data #####

#filter for just people
shorebird <- shorebird %>% 
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
  # groups won't carry over into next code
  ungroup() %>%
  # make col names consistent (snake case)
  clean_names()

##### tidy photo-plot data #####

# QA/QC Notes
  # No data for 2018 or 2009
  # Exclude Po7 and Po8 from Zone 2 (recently added plots)
  # Exclude Z1 M6 (recently added plot)
  # Note that Po plots weren't established until 1995 

target <- 
  # add generic codes to this set, along with formatted scientific names
  left_join(target, align, by = 'SpeciesCode') %>%
  # clean up names
  clean_names() %>%
  # only get cabr sites
  filter(site_code %in% cabrsites) %>%
  # use only fall data (spring sampling stopped in 2017) 
  filter(season_name == 'Fall') %>%
  # remove mytilus plot 6 in z1 and pollicipes plots 7+8 from Z2
  filter(plot_code %notin% c('POL-07', 'POL-08', 'MYT-06')) %>%
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

# plot survey coverage, export as csv
ungroup(target) %>%
  select(site_code, survey_year, plot_code) %>%
  mutate(Plot = paste(site_code, plot_code)) %>%
  select(-c(site_code, plot_code)) %>%
  distinct() %>%
  na.omit() %>%
  mutate(surveyed = 'X') %>%
  right_join(., ungroup(target) %>%
               select(site_code, survey_year, plot_code) %>%
               mutate(Plot = paste(site_code, plot_code)) %>%
               select(-c(site_code, plot_code)) %>%
               expand(Plot, survey_year) %>%
               distinct() %>%
               na.omit(), 
             by = c('survey_year', 'Plot')) %>%
  pivot_wider(names_from = survey_year, values_from = surveyed) %>%
  filter(Plot %notin% c('CAB1 NA', 'CAB2 NA', 'CAB3 NA')) %>%
  write.csv('../figures/plot_data_table.csv')


# align with taxonomic resolution in 1990
# get sum of number of points for each species
target_1990 <- target %>%
  group_by(site_code, site_name, zone_name, survey_year, zone, plot_num, plot_code, code_1990) %>%
  summarise(cover = sum(n))

# calculate % cover (dividing points of cover by total points in a quadrat)
target_1990 <- left_join(target_1990, target %>%
                           group_by(site_code, site_name, zone_name, 
                                    survey_year, zone, plot_num, plot_code) %>% 
                           summarise(points = sum(n)), 
                          by = c("site_code", "site_name", "zone_name", 
                          "survey_year", "zone", "plot_num", "plot_code")) %>%
  # get cover by # of points
  mutate(pct_cover = (cover/points)*100) %>%
  select(-cover, -points) %>%
  rename(species_code = code_1990) %>%
  filter(site_name %in% c("Cabrillo I", "Cabrillo II", "Cabrillo III")) %>%
  # remove rows with na's
  na.omit() %>%
  ungroup() %>%
  # add species names from align 
  left_join(., align %>%
              clean_names() %>%
              select(species_code, scientific_name) %>%
              distinct(), 
            by = 'species_code')

# remove extras
remove(align, target)

# get cover of species in their "home" plots
target_90_home <- ungroup(target_1990) %>%
  # filter to only target taxa in their own plots
  filter((zone == 'CHT' & species_code == 'CHTBAL') |
         (zone == 'MYT' & species_code == 'MUSSEL') |
         (zone == 'SIL' & species_code == 'SILCOM') |
         (zone == 'POL' & species_code == 'POLPOL')) %>%
  # calculate mean + SE of percent cover for each year
  group_by(zone_name, survey_year, zone, species_code, scientific_name) %>%
  summarise(cover_mean = mean(pct_cover), 
            cover_SE = sd(pct_cover)/sqrt(length(pct_cover)))

#rotate from long to wide
target_wide <- select(target_1990, -scientific_name) %>% pivot_wider(names_from = species_code, values_from = pct_cover)%>%
  mutate_all(~replace(., is.na(.), 0))

#combine shorebird data and wide format target spp abundance data
target_shore<-left_join(target_wide, shorebird[,-c(2)], 
                        by = c('zone_name', 'survey_year'))%>%
  drop_na()

##### question 1 - has visitation changed over time? #####
# approach - linear regression for each zone
# result - visitation increased in Z1 and Z2, no change in Z3

# get regression coefficients + bind to "shorebird" data
visit_linreg <- ungroup(shorebird) %>%
  group_by(zone_name) %>%
  # make 3 sub-groups of data for each zone
  nest() %>%
  # fit linreg to each sub-group + get results summary
  mutate(fit = map(data, ~lm(.$people ~ .$survey_year)),
         summary = map(fit, glance)) %>%
  # un-nest data w/ linreg summary
  unnest(c(data, summary)) %>%
  clean_names() %>%
  select(-c(adj_r_squared, sigma, statistic, df:nobs, fit))

# make paneled figure to present results
visit_plot <- ggplot(data = visit_linreg, 
       mapping = aes(x = survey_year, y = people, color = zone_name)) + 
  # points + lines for all panels (people ~ year)
  geom_point() + 
  geom_smooth(method = 'lm') + 
  # p-values + r2 values in all panels
  geom_text(mapping = aes(x = survey_year[15], y = max(people)), 
                label = paste('p = ', scales::pvalue(visit_linreg$p_value), 
                              ', R� = ', round(visit_linreg$r_squared, digits = 2), 
                              sep = ''), color = 'black') + 
  xlab('Year') + 
  ylab('Mean number of visitors') + 
  scale_color_viridis(discrete = TRUE, begin = 0.9, end = 0.3) + 
  facet_wrap(~zone_name) +
  lltheme_light + 
  theme(legend.position = 'none')

# save light theme version
visit_plot + lltheme_light + theme(legend.position = 'none')
ggsave('../figures/visit_linreg.png', width = 7, height = 5)

# save dark theme version
visit_plot + lltheme_dark + theme(legend.position = 'none') 
ggsave('../figures/visit_linreg_dark.png', width = 7, height = 5)


##### question 2 - has % cover of target taxa changed over time? #####

# linreg approach (//'s visitation question)


### write loop to run GAM across sites + target species

# make output df for GAM results
gam_output <- tibble(ZoneName = target_90_home$ZoneName,
                     Scientific_name = target_90_home$Scientific_name) %>%
  distinct() %>%
  mutate(`Adjusted R²` = NA,
         `Deviance explained` = NA,
         N = NA,
         `Intercept - P` = NA,
         `Year - F` = NA,
         `Year - P` = NA,
         `People - F` = NA,
         `People - P` = NA)

# nested loop (i is for row # in gam_output table)
for (i in 1:nrow(gam_output)) {
  
  # subset relevant data 
  gam_data <- target_90_home %>%
    filter(Scientific_name == gam_output$Scientific_name[i] & 
             ZoneName == gam_output$ZoneName[i])
  
  # run model
  gam_mod <- gam(Cover_mean ~ s(SurveyYear) + s(people), data = gam_data)
  
  # put relevant summary info in output table 
  gam_output$N[i] <- nrow(gam_data)
  gam_output$`Deviance explained`[i] <- summary(gam_mod)$dev.expl
  gam_output$`Adjusted R²`[i] <- summary(gam_mod)$r.sq
  gam_output$`Intercept - P`[i] <- summary(gam_mod)$p.table[,4]
  gam_output$`Year - F`[i] <- summary(gam_mod)$s.table[1,3]
  gam_output$`Year - P`[i] <- summary(gam_mod)$s.table[1,4]
  gam_output$`People - F`[i] <- summary(gam_mod)$s.table[2,3]
  gam_output$`People - P`[i] <- summary(gam_mod)$s.table[2,4]
  
  # plot results (2 panels - one for people and one for year)
  p1 <- ggplot(data = gam_data,
               mapping = aes(x = SurveyYear, y = Cover_mean)) + 
    geom_point(color = 'black') + 
    xlab('Year') + 
    ylab('Percent cover') + 
    ggtitle(paste('GAM Result for', gam_data$Scientific_name[i], 'in',
                  gam_data$ZoneName[i]), 
            subtitle = paste('Abundance over time (P = ', scales::pvalue(gam_output$`Year - P`[i]),
                             ', F = ', round(gam_output$`Year - F`[i], digits = 2), 
                             ', N = ', gam_output$N, ')', sep = '')) +
    lltheme_light
  
  # if gam is significant, add in line
  if(gam_output$`Year - P`[i] <= 0.05) { 
    p1 <- p1 + geom_smooth(method = 'loess') }
  
  
  p2 <- ggplot(data = gam_data,
               mapping = aes(x = people, y = Cover_mean)) + 
    geom_point(color = 'black') + 
    xlab('Mean visitor count') + 
    ylab('Percent cover') + 
    ggtitle(paste('GAM Result for', gam_data$Scientific_name[i]
                  , 'in', gam_data$ZoneName[i]), 
            subtitle = paste('Abundance as a function of visitation (P = ', scales::pvalue(gam_output$`People - P`[i]),
                             ', F = ', round(gam_output$`People - F`[i], digits = 2), 
                             ', N = ', gam_output$N, ')', sep = '')) +
    lltheme_light
  
  # add line if GAM significant for people vs cover
  if(gam_output$`People - P`[i] <= 0.05) { 
    p2 <- p2 + geom_smooth(method = 'loess') }
  
  p3 <- p1 + p2
  
  ggsave(filename = paste('figures/GAM/gam_cover_people_time_', 
                          gsub('/', '', gam_data$Scientific_name[i]), '_',
                          gsub(' ', '', gam_data$ZoneName[i]), '.png', sep = ''),
         plot = p3,
         width = 12)
}

# clean up environment
remove(p1, p2, p3, site_list, spp_list, gam_data, gam_mod, i)

# make a formatted table using gam_output and the gt package
gt::gtsave(gam_output %>%
             mutate(`Intercept - P` = scales::pvalue(`Intercept - P`),
                    `Year - P` = scales::pvalue(`Year - P`),
                    `People - P` = scales::pvalue(`People - P`),
                    `Adjusted R²` = round(`Adjusted R²`, digits = 2),
                    `Deviance explained` = round(`Deviance explained`, digits = 2),
                    `Year - F` = round(`Year - F`, digits = 2),
                    `People - F` = round(`People - F`, digits = 2)) %>%
             rename(Site = ZoneName, `Target spp.` = Scientific_name) %>%
             gt::gt() %>%
             gt::fmt_percent(columns = `Deviance explained`, decimals = 0) %>%
             gt::cols_align(align = 'center', columns = `Intercept - P`:`People - P`),
           'figures/GAM/gam_table.png')

# add GAM info to dataset and make paneled plots for spp over time
target_90_home2 <- left_join(target_90_home, gam_output, 
                             by = c("ZoneName", "Scientific_name")) %>%
  # add common names for figs 
  mutate(CommonName = case_when(
    Scientific_name == 'Balanus/Chthamalus' ~ 'Acorn barnacle',
    Scientific_name == 'Mytilus' ~ 'Mussel', 
    Scientific_name == 'Silvetia' ~ 'Golden rockweed',
    Scientific_name == 'Pollicipes' ~ 'Goose barnacle'
  )) %>%
  # add numbers for factor order in paneled figs
  mutate(Fct_num = case_when (
    Scientific_name == 'Balanus/Chthamalus' ~ 1,
    Scientific_name == 'Mytilus' ~ 2, 
    Scientific_name == 'Silvetia' ~ 4,
    Scientific_name == 'Pollicipes' ~ 3
  )) %>%
  arrange(Fct_num)

target_90_home2$CommonName <- factor(target_90_home2$CommonName, 
                                     levels = c('Acorn barnacle',
                                                'Mussel', 
                                                'Goose barnacle',
                                                'Golden rockweed'))


# make paneled plot (panels are spp)  
# cover as a function of survey year

gam_time_fig <- function(dataset, savename) {
  
  ggplot(data = dataset,
         mapping = aes(x = SurveyYear, y = Cover_mean, color = CommonName, group = ZoneName)) + 
    geom_point(size = 2) +
    # only show smooth lines for significant relationships from GAM
    geom_smooth(data = filter(dataset, `Year - P` <= 0.05),
                mapping = aes(x = SurveyYear, y = Cover_mean,
                              color = CommonName, fill = CommonName),
                method = 'loess') +
    # put info (r2 and pval) for each in color in text
    geom_text(data = select(ungroup(dataset), ZoneName, CommonName, 
                            `Deviance explained`, `Year - P`) %>% distinct() %>%
                mutate(Value = case_when(ZoneName == "Zone I" ~ 100,
                                         ZoneName == "Zone II" ~ 93,
                                         ZoneName == "Zone III" ~ 86)), 
              mapping = aes(x = 1991, y = Value, 
                            label = paste('P =', scales::pvalue(`Year - P`))),
              hjust = 0) +
    #scale_color_viridis(discrete = TRUE, begin = 0.4, end = 1, name = 'Site') +
    #scale_fill_viridis(discrete = TRUE, begin = 0.4, end = 1, name = 'Site') +
    xlab('Year') + 
    ylab('Percent cover') +
    facet_wrap(~CommonName, scales = 'free_y') + 
    coord_cartesian(xlim = c(1990, 2020), ylim = c(0,100)) + 
    scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))  +
    lltheme_dark +
    theme(legend.position = 'none')
  # if using scientific names for strip/facet text
  # + theme(strip.text = element_text(face = "italic"))
  
  ggsave(paste('figures/GAM/all_panels_commonname', savename, '.png', sep = ''), width = 12)
}

# make for Z1 only

gam_time_fig(filter(target_90_home2, ZoneName == 'Zone I'), 'time2')

# now make for people 
ggplot(data = filter(target_90_home2, ZoneName == 'Zone I'),
       mapping = aes(x = people, y = Cover_mean, color = CommonName)) + 
  geom_point(size = 2) +
  # only show smooth lines for significant relationships from GAM
  geom_smooth(data = filter(target_90_home2, ZoneName == 'Zone I' & `People - P` <= 0.05),
              mapping = aes(x = people, y = Cover_mean,
                            color = CommonName, fill = CommonName),
              method = 'loess') +
  # put info (r2 and pval) for each in color in text
  geom_text(data = select(ungroup(filter(target_90_home2, ZoneName == 'Zone I')), 
                          CommonName, `Deviance explained`, `People - P`) %>%
              distinct(),
            mapping = aes(x = 0, y = 100, 
                          label = paste('P =', scales::pvalue(`People - P`))),
            hjust = 0) +
  #scale_color_viridis(discrete = TRUE, begin = 0.4, end = 1, name = 'Site') +
  #scale_fill_viridis(discrete = TRUE, begin = 0.4, end = 1, name = 'Site') +
  xlab('Number of visitors') + 
  ylab('Percent cover') +
  facet_wrap(~CommonName) + 
  coord_cartesian(ylim = c(0,100)) + 
  lltheme_dark + 
  theme(legend.position = 'none')
#theme(strip.text = element_text(face = "italic"))

ggsave(paste('figures/GAM/all_panels_commonname2_people.png'), width = 12)



# all zones (color by spp) like LS
ggplot(data = target_90_home,
       mapping = aes(x = SurveyYear, y = Cover_mean, color = CommonName)) + 
  geom_point(size = 2) +
  # only show smooth lines for significant relationships from GAM
  geom_smooth(data = filter(dataset, `Year - P` <= 0.05),
              mapping = aes(x = SurveyYear, y = Cover_mean,
                            color = ZoneName, fill = ZoneName),
              method = 'loess') +
  # put info (r2 and pval) for each in color in text
  geom_text(data = select(ungroup(dataset), ZoneName, Scientific_name, 
                          `Deviance explained`, `Year - P`) %>% distinct() %>%
              mutate(Value = case_when(ZoneName == "Zone I" ~ 100,
                                       ZoneName == "Zone II" ~ 93,
                                       ZoneName == "Zone III" ~ 86)), 
            mapping = aes(x = 1991, y = Value, 
                          label = paste('P =', scales::pvalue(`Year - P`))),
            hjust = 0) +
  xlab('Year') + 
  ylab('Percent cover') +
  facet_wrap(~CommonName) + 
  coord_cartesian(xlim = c(1990, 2020), ylim = c(0,100)) + 
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))  +
  lltheme_dark

ggsave(paste('figures/GAM/all_panels_', savename, '.png', sep = ''), width = 12)


# plot people x cover figure
ggplot(data = target_90_home,
       mapping = aes(x = people, y = Cover_mean, color = ZoneName)) + 
  geom_point(size = 2) +
  # only show smooth lines for significant relationships from GAM
  geom_smooth(data = filter(target_90_home, `People - P` <= 0.05),
              mapping = aes(x = people, y = Cover_mean,
                            color = ZoneName, fill = ZoneName),
              method = 'loess') +
  # put info (r2 and pval) for each in color in text
  geom_text(data = select(ungroup(target_90_home), ZoneName, Scientific_name, 
                          `Deviance explained`, `People - P`) %>% distinct() %>%
              mutate(Value = case_when(ZoneName == "Zone I" ~ 100,
                                       ZoneName == "Zone II" ~ 93,
                                       ZoneName == "Zone III" ~ 86)), 
            mapping = aes(x = 0, y = Value, 
                          label = paste('P =', scales::pvalue(`People - P`))),
            hjust = 0) +
  scale_color_viridis(discrete = TRUE, begin = 0.4, end = 1, name = 'Site') +
  scale_fill_viridis(discrete = TRUE, begin = 0.4, end = 1, name = 'Site') +
  xlab('Number of visitors') + 
  ylab('Percent cover') +
  facet_wrap(~Scientific_name) + 
  coord_cartesian(ylim = c(0,100)) + 
  lltheme_dark + 
  theme(strip.text = element_text(face = "italic"))

ggsave('figures/GAM/all_panels_people.png', width = 12)  
```


```{r}
#separate out people/year from target_shore
environ<-target_shore[c(1,2,3,4,5,17)] #tree

#separate out species matrix, including linking factors
species<-target_shore[c(1,2,3,8,9,10,11,12,13,14,15,16)] #bird
```

```{r}
#permanova- following rpubs.com/collnell/manova

#make matrix
species.matrix<-as.matrix(species[,4:12])

#standardize with sqrt
species.mat<-sqrt(species.matrix)

#find ecological resemblance
species.dist<-vegdist(species.mat, method='bray')

species.div<-adonis2(species.dist~ZoneName, data=species, permutations = 999, method='bray',strata = "PLOT")
species.div #issue with strata. ran w/out strata and pvalue=0.001

dispersion<-betadisper(species.dist, group=species$ZoneName)
permutest(dispersion) #pvalue=0.678

plot(dispersion, hull=F, ellipse=T) #just zone III shows up
# note from LP - i saw all zones, but it took a while to show up
```


```{r LP Suggestion PERMANOVA}
# let's run these parallel to the GAM analyses - running a separate model for each plot type
# does spp makeup of plots vary across zone and visitation?

# https://archetypalecology.wordpress.com/2018/02/21/permutational-multivariate-analysis-of-variance-permanova-in-r-preliminary/

# output from loop

# list to loop across (plot types)
spp_list <- unique(target_shore$Zone)

for(i in 1:length(spp_list)) {
  
  # filter for spp i
  dist_data <- target_shore %>%
    filter(Zone == spp_list[i])
  
  # get distance matrix
  dist_matrix <- vegdist(select(ungroup(dist_data), BARESUB:NOMATCH), method='bray')
  
  # conduct PERMANOVA (is makeup different across zones and visitation and year?)
  dist_mod <- adonis2(select(ungroup(dist_data), BARESUB:NOMATCH) ~ ZoneName + people + SurveyYear, data = dist_data)
  # different across zones, people and interaction (zones differ in visitation)
  
  # put results in table and save
  results <- tibble(Term = c('Zone', 'Visitation', 'Year', 'Residual', 'Total'),
                    `Deg. of freedom` = dist_mod$Df,
                    `Sum of squares` = round(dist_mod$SumOfSqs, digits = 2),
                    `F-value` = round(dist_mod$F, digits = 2),
                    `P-value` = dist_mod$`Pr(>F)`) %>%
    gt::gt() %>%
    gt::gtsave(paste('figures/PERMANOVA/permanova_result_',spp_list[i] ,'.png', sep = ''))
  
  # add more things to this loop :)
  
}

```
#NMDS
#zone 3 NMDS
```{r}

color<-tibble(ZoneName=unique(species$ZoneName), Colors=c("green", "red", "blue"))

#make matrix
#9445 first row of zone3, 14028
#zone3.matrix<-as.matrix(select(species[which(species$ZoneName=="(C) Zone III")], -c("SiteCode","SiteName","ZoneName")))

zone3.matrix<-ungroup(species)%>% #ungroup b/c r wants to group first 3 columns
  filter(ZoneName=="(C) Zone III")%>% #filter by zone 3 for zone 3 nmds
  select(-c(1:3))%>% #take out non-num variables to be able to square
  as.matrix() #make matrix

#standardize with sqrt
zone3.mat<-sqrt(zone3.matrix)

#remove matrix rows with all 0s to run MDS? Error
#zone3.matrix=zone3.matrix[, colSums(count != 0) > 0]

zone3MDS<-metaMDS(zone3.mat, distance="bray", k=3, trymax = 35, autotransform = T) #stress=0.1252488
zone3MDS

stressplot(zone3MDS)

#graph doesn't work
NMDS1zone3<-zone3MDS$points[,1]
NMDS2zone3<-zone3MDS$points[,2]

zone3species<-species[which(species$ZoneName=="(C) Zone III"),]
zone3environ<-environ[which(environ$ZoneName=="(C) Zone III"),]
zone3species.plot<-cbind(zone3species, NMDS1zone3, NMDS2zone3, zone3environ)
#remove repeating columns
zone3species.plot<-zone3species.plot[-c(15:17)]
zone3species.plot<-left_join(zone3species.plot, color)


p<-ggplot(zone3species.plot, aes(NMDS1zone3, NMDS2zone3, color=Zone))+
  stat_ellipse(type='t',size = 1)+ ##draws 95% confidence interval ellipses
  theme_minimal()+
  annotate("text", x=min(NMDS1zone3), y=min(NMDS2zone3), label=paste('Stress =',round(zone3MDS$stress,3)))+ #add stress to plot
  geom_point(data=zone3species.plot, aes(NMDS1zone3, NMDS2zone3, shape=Zone))+
  scale_shape_manual(values=c(3,16,17,18))
p #make diff shapes for each Zone (species)
#polpol in zone 1 are similar to polpol in zone 2 and 3. pol and cht same across all zones. Myt and sil differs across zones more. 
#sil and myt diff in zone 1 than other zones

#fit mussel environ
mus.zone3 <- target_shore %>%
  filter(Zone == 'MYT'&ZoneName=="(C) Zone III") %>%
  select(-c(SiteCode:Zone, people))

mus.envzone3 <- target_shore %>%
  filter(Zone == 'MYT'&ZoneName=="(C) Zone III") %>%
  select(SiteName, SurveyYear, people)

musMDSzone3<-metaMDS(mus.zone3, distance= "bray", k = 2, trymax = 35, autotransform = T)
fitzone3<-envfit(musMDSzone3, mus.envzone3)
arrowzone3<-data.frame(fitzone3$vectors$arrows,R = fitzone3$vectors$r, P = fitzone3$vectors$pvals)
arrowzone3$FG <- rownames(arrowzone3)
arrow.pzone3<-filter(arrowzone3, P <= 0.05)


#graph with arrows as environmental variables
ggplot(zone3species.plot, aes(NMDS1zone3, NMDS2zone3))+
  #geom_point(data=zone3species.plot, aes(NMDS1zone3, NMDS2zone3, color=Zone),position=position_jitter(.1))+##separates overlapping points
  stat_ellipse(aes(fill=Zone), alpha=.2,type='t',size =1, geom="polygon")+
  theme_minimal()+
  geom_segment(data=arrow.pzone3, aes(x=0, y=0, xend=NMDS1, yend=NMDS2, label=FG, lty=FG), arrowzone3=arrow(length=unit(.2, "cm")*arrow.pzone3$R))+ ##add arrows (scaled by R-squared value)
  annotate("text", x=min(NMDS1zone3), y=min(NMDS2zone3), label=paste('Stress =',round(zone3MDS$stress,3)))+ #add stress to plot
  geom_point(data=zone3species.plot, aes(NMDS1zone3, NMDS2zone3, shape=Zone))+
  scale_shape_manual(values=c(3,16,17,18))
ggsave("NMDS_zone3.png", width=6, height=4)

#graph with ellipses as zone
ggplot(zone3species.plot, aes(NMDS1zone3, NMDS2zone3))+
  #geom_point(data=zone3species.plot, aes(NMDS1zone3, NMDS2zone3, color=Zone),position=position_jitter(.1))+##separates overlapping points
  stat_ellipse(aes(fill=ZoneName), alpha=.2,type='t',size =1, geom="polygon")+
  theme_minimal()+
  geom_segment(data=arrow.pzone3, aes(x=0, y=0, xend=NMDS1, yend=NMDS2, label=FG, lty=FG), arrowzone3=arrow(length=unit(.2, "cm")*arrow.pzone3$R))+ ##add arrows (scaled by R-squared value)
  annotate("text", x=min(NMDS1zone3), y=min(NMDS2zone3), label=paste('Stress =',round(zone3MDS$stress,3)))+ #add stress to plot
  geom_point(data=zone3species.plot, aes(NMDS1zone3, NMDS2zone3, shape=Zone))+
  scale_shape_manual(values=c(3,16,17,18))+
  scale_fill_manual(values=zone3species.plot$Colors)
ggsave("NMDS_zone3ellip.png", width=6, height=4)

```

#NMDS with zone 2 and 3
```{r}
#make matrix
zone23.matrix<-as.matrix(species[104:305,4:9])

#standardize with sqrt
zone23.mat<-sqrt(zone23.matrix)

zone23MDS<-metaMDS(zone23.mat, distance="bray", k=2, trymax = 35, autotransform = T) #stress=0.1252488
zone23MDS

stressplot(zone23MDS)

#graph doesn't work
NMDS1zone23<-zone23MDS$points[,1]
NMDS2zone23<-zone23MDS$points[,2]

zone23species<-species[which(species$ZoneName!="(A) Zone I"),]
zone23environ<-environ[which(species$ZoneName!="(A) Zone I"),]
zone23species.plot<-cbind(zone23species, NMDS1zone23, NMDS2zone23, zone23environ)
#remove repeating columns
zone23species.plot<-zone23species.plot[-c(15:17)]
zone23species.plot<-left_join(zone23species.plot, color)

ggplot(zone23species.plot, aes(NMDS1zone23, NMDS2zone23, color=Zone))+
  stat_ellipse(type='t',size = 1)+ ##draws 95% confidence interval ellipses
  theme_minimal()+
  annotate("text", x=min(NMDS1zone23), y=min(NMDS2zone23), label=paste('Stress =',round(zone23MDS$stress,3)))+ #add stress to plot
  geom_point(data=zone23species.plot, aes(NMDS1zone23, NMDS2zone23, shape=Zone))+
  scale_shape_manual(values=c(3,16,17,18))
#make diff shapes for each Zone (species)
#polpol in zone 1 are similar to polpol in zone 2 and 3. pol and cht same across all zones. Myt and sil differs across zones more. 
#sil and myt diff in zone 1 than other zones

#fit mussel environ
mus.zone23 <- target_shore %>%
  filter(Zone == 'MYT'& ZoneName!="(A) Zone I") %>%
  select(-c(SiteCode:Zone, people))

mus.envzone23 <- target_shore %>%
  filter(Zone == 'MYT'& ZoneName!="(A) Zone I") %>%
  select(SiteName, SurveyYear, people)

musMDSzone23<-metaMDS(mus.zone23, distance= "bray", k = 2, trymax = 35, autotransform = T)
fitzone23<-envfit(musMDSzone23, mus.envzone23)
arrowzone23<-data.frame(fitzone23$vectors$arrows,R = fitzone23$vectors$r, P = fitzone23$vectors$pvals)
arrowzone23$FG <- rownames(arrowzone23)
arrow.pzone23<-filter(arrowzone23, P <= 0.05)

#graph with arrows as environmental variables
ggplot(zone23species.plot, aes(NMDS1zone23, NMDS2zone23))+
  #geom_point(data=zone23species.plot, aes(NMDS1zone23, NMDS2zone23, color=Zone),position=position_jitter(.1))+##separates overlapping points
  stat_ellipse(aes(fill=Zone), alpha=.2,type='t',size =1, geom="polygon")+
  theme_minimal()+
  geom_segment(data=arrow.pzone23, aes(x=0, y=0, xend=NMDS1, yend=NMDS2, label=FG, lty=FG), arrowzone23=arrow(length=unit(.2, "cm")*arrow.pzone23$R))+ ##add arrows (scaled by R-squared value)
  annotate("text", x=min(NMDS1zone23), y=min(NMDS2zone23), label=paste('Stress =',round(zone23MDS$stress,3)))+ #add stress to plot
  geom_point(data=zone23species.plot, aes(NMDS1zone23, NMDS2zone23, shape=Zone))+
  scale_shape_manual(values=c(3,16,17,18))
ggsave("NMDS_zone23.png", width=6, height=4)

#graph with ellipses as zones
ggplot(zone23species.plot, aes(NMDS1zone23, NMDS2zone23))+
  #geom_point(data=zone23species.plot, aes(NMDS1zone23, NMDS2zone23, color=Zone),position=position_jitter(.1))+##separates overlapping points
  stat_ellipse(aes(fill=ZoneName), alpha=.2,type='t',size =1, geom="polygon")+
  theme_minimal()+
  geom_segment(data=arrow.pzone23, aes(x=0, y=0, xend=NMDS1, yend=NMDS2, label=FG, lty=FG), arrowzone23=arrow(length=unit(.2, "cm")*arrow.pzone23$R))+ ##add arrows (scaled by R-squared value)
  annotate("text", x=min(NMDS1zone23), y=min(NMDS2zone23), label=paste('Stress =',round(zone23MDS$stress,3)))+ #add stress to plot
  geom_point(data=zone23species.plot, aes(NMDS1zone23, NMDS2zone23, shape=Zone))+
  scale_shape_manual(values=c(3,16,17,18))+
  scale_fill_manual(values=unique(zone23species.plot$Colors))
ggsave("NMDS_zone23ellip.png", width=6, height=4)
```

#All zones NMDS
```{r}
color<-tibble(ZoneName=unique(species$ZoneName), Colors=c("green", "red", "blue"))

#remove repeating columns
zone3species.plot<-zone3species.plot[-c(15:17)]
zone3species.plot<-left_join(zone3species.plot, color)
speciesMDS<-metaMDS(species.mat, distance="bray", k=2, trymax = 35, autotransform = T) #stress=0.1572664
speciesMDS

stressplot(speciesMDS)

#graph doesn't work
NMDS1<-speciesMDS$points[,1]
NMDS2<-speciesMDS$points[,2]
species.plot<-cbind(species, NMDS1, NMDS2, environ)
species.plot<-species.plot[-c(15:17)]
species.plot<-left_join(species.plot, color)

p<-ggplot(species.plot, aes(NMDS1, NMDS2, color=ZoneName))+
  stat_ellipse(type='t',size = 1)+ ##draws 95% confidence interval ellipses
  theme_minimal()+
  #geom_text(data=species.plot, aes(NMDS1, NMDS2, label=Zone))+ #position=position_jitter(.35))+
  annotate("text", x=min(NMDS1), y=min(NMDS2), label=paste('Stress =',round(speciesMDS$stress,3)))+ #add stress to plot
  geom_point(data=species.plot, aes(NMDS1, NMDS2, shape=Zone))+
  scale_shape_manual(values=c(3,16,17,18))
p #make diff shapes for each Zone (species)
#polpol in zone 1 are similar to polpol in zone 2 and 3. pol and cht same across all zones. Myt and sil differs across zones more. 
#sil and myt diff in zone 1 than other zones

#fit environmental variables
#fit<-envfit(speciesMDS, species.mat)
#arrow<-data.frame(fit$vectors$arrows,R = fit$vectors$r, P = fit$vectors$pvals)
#arrow$FG <- rownames(arrowsil)
#arrow.p<-filter(arrowsil, P <= 0.05)

#fit mussel environ
mus.species <- target_shore %>%
  filter(Zone == 'MYT') %>%
  select(-c(SiteCode:Zone, people))

mus.env <- target_shore %>%
  filter(Zone == 'MYT') %>%
  select(SiteName, SurveyYear, people)

musMDS<-metaMDS(mus.species, distance= "bray", k = 2, trymax = 35, autotransform = T)
fit<-envfit(musMDS, mus.env)
arrow<-data.frame(fit$vectors$arrows,R = fit$vectors$r, P = fit$vectors$pvals)
arrow$FG <- rownames(arrow)
arrow.p<-filter(arrow, P <= 0.05)

ggplot(data=species.plot, aes(NMDS1, NMDS2))+
  geom_point(data=species.plot, aes(NMDS1, NMDS2, color=ZoneName),position=position_jitter(.1))+##separates overlapping points
  stat_ellipse(aes(fill=ZoneName), alpha=.2,type='t',size =1, geom="polygon")+ ##changes shading on ellipses
  theme_minimal()+
  geom_segment(data=arrow.p, aes(x=0, y=0, xend=NMDS1, yend=NMDS2, label=FG, lty=FG), arrow=arrow(length=unit(.2, "cm")*arrow.p$R)) ##add arrows (scaled by R-squared value)

#graph with arrows of how species differ
ggplot(species.plot, aes(NMDS1, NMDS2))+
  #geom_point(data=species.plot, aes(NMDS1, NMDS2, color=ZoneName),position=position_jitter(.1))+##separates overlapping points
  stat_ellipse(aes(fill=Zone), alpha=.2,type='t',size =1, geom="polygon")+
  theme_minimal()+
  geom_segment(data=arrow.p, aes(x=0, y=0, xend=NMDS1, yend=NMDS2, label=FG, lty=FG), arrow=arrow(length=unit(.2, "cm")*arrow.p$R))+ ##add arrows (scaled by R-squared value)
  annotate("text", x=min(NMDS1), y=min(NMDS2), label=paste('Stress =',round(speciesMDS$stress,3)))+ #add stress to plot
  geom_point(data=species.plot, aes(NMDS1, NMDS2, shape=Zone))+
  scale_shape_manual(values=c(3,16,17,18))

#graph with arrows as environmental variables
ggplot(species.plot, aes(NMDS1, NMDS2))+
  #geom_point(data=species.plot, aes(NMDS1, NMDS2, color=ZoneName),position=position_jitter(.1))+##separates overlapping points
  stat_ellipse(aes(fill=ZoneName), alpha=.2,type='t',size =1, geom="polygon")+
  theme_minimal()+
  geom_segment(data=arrow.p, aes(x=0, y=0, xend=NMDS1, yend=NMDS2, label=FG, lty=FG), arrow=arrow(length=unit(.2, "cm")*arrow.p$R))+ ##add arrows (scaled by R-squared value)
  annotate("text", x=min(NMDS1), y=min(NMDS2), label=paste('Stress =',round(speciesMDS$stress,3)))+ #add stress to plot
  geom_point(data=species.plot, aes(NMDS1, NMDS2, shape=Zone))+
  scale_shape_manual(values=c(3,16,17,18))+
  scale_fill_manual(values=unique(species.plot$Colors))
ggsave("NMDS_allzones.png", width=6, height=4)

#R values sig for both people and survey year.
#people arrow driving x axis, associated with mussels
#Survey year driving y axis, not really associated with specific species. 
```

#plot zone 1 first. Then add zone 2 and 3. Or start zone 3 (no people), add zone 2 and 1 (show diff because of people on mussel communities)