#################################################
# Title: Report Figures & Stats - 1990-2021 Trend Report
# Purpose: adapted from LT report code + clean up code
# Author: LP
# Created: 5/2/22
# Last edited: 8/25/22
##################################################

##### packages #####
library(ggdark)       # dark field versions of ggplot2 themes
library(janitor)      # snake case col names and remove duplicates
library(broom)        # glance fn for linregs
library(calecopal)    # color palettes
library(ggrepel)      # flying labels 
library(vegan)        # community analyses
library(PNWColors)    # color palettes
library(viridis)      # color palettes
library(tidyverse)    # tidyverse packages

##### places & themes #####
# 3 color palette
ecopal_3 <- c(cal_palette('chaparral3')[4], cal_palette('bigsur')[5], 
  cal_palette('bigsur')[4])

ecopal_4 <- c(cal_palette('chaparral3')[4], cal_palette('bigsur')[5], 
              cal_palette('bigsur')[4], cal_palette('chaparral3')[1])

# save place
saveplace <- './figs/lt_trend_report_figs/'

# theme
base_theme <- theme(text = element_text(size = 12),
                    # add more space between panels
                    panel.spacing = unit(1, 'lines'),
                    # no background to wrap panels
                    strip.background = element_blank(),
                    strip.text = element_text(size = 12, 
                                              hjust = 0),
                    # panel labels outside x axis labels
                    strip.placement = 'outside',
                    panel.grid = element_blank(),
                    # adjust x axis labels
                    axis.text.y = element_text(size = 12),
                    axis.text.x = element_text(size = 12, 
                                               angle = 45, 
                                               hjust = 1))

light_theme <- theme_bw() + base_theme
dark_theme <- dark_theme_bw() + base_theme

remove(base_theme)

##### load data #####

# get list of tidy data files from wrangling output folder
files <- list.files ('D:/LP_Files/RStudio_Files/cabr_intertidal_community_ms/data/lt_trend_report_tidy_data/', full.names = TRUE) 

# loop to read csv files from folder and assign them to file name without path and '.csv'
for(i in 1:length(files)) {
assign(gsub('.csv', '', gsub('D:/LP_Files/RStudio_Files/cabr_intertidal_community_ms/data/lt_trend_report_tidy_data/', '', files[i])), read_csv(files[i]))
}

# remove files list from environment
remove(files)

##### visitor count (people) data #####

# step 1 - time series/linreg  across zones (has # visitors increased over time?)

# get regression coefficients + bind to people_count data 
people_count <- ungroup(people_count) %>%
  group_by(zone) %>%
  # make 3 sub-groups of data for each zone
  nest() %>%
  # fit linreg to each sub-group + get results summary
  mutate(fit = map(data, ~lm(.$mean_count ~ .$survey_year)),
         summary = map(fit, glance)) %>%
  # un-nest data w/ linreg summary
  unnest(c(data, summary)) %>%
  clean_names() %>%
  select(-c(adj_r_squared, sigma, statistic, df:nobs, fit)) %>%
  rename(Zone = zone)

# save stats table
write_csv(select(people_count, Zone, r_squared, p_value) %>% distinct(), 
          paste0(saveplace, 'stats_tables/people_linreg.csv'))

# make paneled figure to present results
visit_plot <- ggplot(data = people_count, 
                     mapping = aes(x = survey_year, y = mean_count, color = Zone)) + 
  # points + shading for all panels (people ~ year)
  geom_smooth(aes(color = Zone, fill = Zone),
              method = 'lm', linetype = 0) + 
  geom_point() + 
  # line for zone 1 (only significant with r2 value > 0.4)
  geom_smooth(data = filter(people_count, r_squared > 0.4),
              mapping = aes(x = survey_year, y = mean_count, color = Zone),
              method = 'lm') + 
  xlab('Year') + 
  ylab('Visitors per survey') + 
  scale_color_manual(values = ecopal_3) + 
  scale_fill_manual(values = ecopal_3)

# save light theme version
ggsave(filename = paste0(saveplace, 'visit_linreg_light.png'),
       plot = visit_plot + light_theme,
       dpi = 300, width = 6, height = 4)

# save dark theme version
ggsave(filename = paste0(saveplace, 'visit_linreg_dark.png'),
       plot = visit_plot + 
         geom_point(size = 4) + 
         dark_theme + 
         theme(text = element_text(size = 28),
              axis.text.x = element_text(size = 28),
              axis.text.y = element_text(size = 28)),
       dpi = 300, width = 12, height = 6)

remove(visit_plot)

# only z1 dark theme (for esa conference)
ggplot(data = filter(people_count, zone == 'Zone I'), 
                     mapping = aes(x = survey_year, y = mean_count, color = zone)) + 
  # points + lines for all panels (people ~ year)
  geom_smooth(method = 'lm', size = 2) + 
  geom_point(size = 2) + 
  xlab('Year') + 
  ylab('Visitors per survey') + 
  scale_color_manual(values = ecopal_3) + 
  dark_theme + 
  theme(text = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))

ggsave(filename = paste0(saveplace, 'visit_linreg_z1_dark.png'),
       dpi = 300, width = 6)

# step 2 - pie chart of magnitude of visitation across zones

# get % of all visitors observed in each zone
donut_data <- people_count %>%
  rename(zone = Zone) %>%
  group_by(zone) %>%
  summarize(n = sum(mean_count)) %>%
  ungroup() %>%
  # get % of visitors
  mutate(pct = round((n/sum(n)*100), digits = 0),
         # calculate label position
         lab_place = cumsum(pct) - 0.5*pct)

# make donut chart
donut_plot <- ggplot(data = donut_data,
       mapping = aes(x = '', y = pct, fill = zone)) +
  geom_bar(stat = 'identity') + 
  geom_text(mapping = aes(y = pct, label = paste0(zone, ' - ', pct, '%')), 
            nudge_x = 0.9, 
            nudge_y = -1, 
            vjust = 2.7, 
            hjust = 0.4,
            size = 3, color = 'black') + 
  scale_fill_manual(values = ecopal_3) + 
  coord_polar(theta = 'y') + 
  theme_void() + 
  theme(legend.position = 'none')

ggsave(filename = paste0(saveplace, 'visit_pie.png'),
       plot = donut_plot, dpi = 300,
       width = 3, height = 3)

remove(donut_plot, donut_data)

##### community change photoplot - stacked bar #####

area_fn <- function(plot_type, spp_code, sort_spp){
  
  # wrangle data
  dataset <- target_90 %>%
    filter(type == plot_type) %>%
    # calculate cover of each taxa across plot replicates
    group_by(survey_year, zone, taxa_code, scientific_name) %>%
    summarize(pct_cover = sum(pct_cover)) %>%
    ungroup() %>%
    # get sum of cover for each year
    group_by(survey_year, zone) %>%
    mutate(cover_sum = sum(pct_cover)) %>%
    ungroup() %>%
    # calculate % cover 
    mutate(pct_cover = (pct_cover/cover_sum)*100) %>%
    # make pretty label columns, nicer spp names
    mutate(scientific_name = case_when(
                      scientific_name =='Tetraclita rubescens' ~ 'Tetraclita',
                      scientific_name =='Pollicipes polymerus' ~ 'Pollicipes',
                      scientific_name =='Silvetia compressa' ~ 'Silvetia',
                      scientific_name == 'Balanus/Chthamalus' ~ 'Balanus Chthamalus',
                      TRUE ~ scientific_name))
  
  
  # step 2 - generate colors associated with taxa for consistent color assignment
  consistent_colors = setNames(object = cal_palette(name = "tidepool", 
                                                    n = 9, type = "continuous"), 
                               nm = sort(unique(dataset$scientific_name)))
  
  # change the sort spp to 'black'
  consistent_colors[sort_spp] = 'gray90'
  
  # step 3 - light theme stacked bar, with specified plot number and type
  ggplot(data = dataset,
         mapping = aes(x = survey_year, y = pct_cover, 
                       # make target spp last (bottom bar on plots)
                       fill = fct_relevel(scientific_name, sort_spp, after = Inf))) +
    # bar stacked by spp group (Scientific_name)
    geom_bar(stat = 'identity', width = 1) + 
    # axis and plot labels
    xlab('Year') + 
    ylab('Percent Cover') + 
    # colors
    scale_fill_manual(name = 'Taxon',
                      values = consistent_colors) +
    facet_wrap(~zone) +
    light_theme +
    theme(aspect.ratio = 1.3)
  
  # save plot
  ggsave(paste(saveplace, '/stackedbar/stackedbar_', sort_spp, '.png', sep = ''), 
         width = 7)
  
  # step 5 - dark theme stacked bar 
  
  # change target spp color to "white"
  consistent_colors[sort_spp] = 'white'
  
  # dark theme plot
  ggplot(data = dataset,
         mapping = aes(x = survey_year, y = pct_cover, 
                       # make target spp last (bottom bar on plots)
                       fill = fct_relevel(scientific_name, sort_spp, after = Inf))) +
    # bar stacked by spp group (Scientific_name)
    geom_bar(stat = 'identity', width = 1) + 
    # axis and plot labels
    xlab('Year') + 
    ylab('Percent Cover') + 
    ggtitle(paste0(sort_spp, ' plot composition')) +
    # colors
    scale_fill_manual(name = 'Taxon',
                      values = consistent_colors) +
    facet_wrap(~zone) +
    dark_theme + 
    theme(
          text = element_text(size = 26, color = 'white'),
          axis.text.x = element_text(size = 24, color = 'white'),
          axis.text.y = element_text(size = 24, color = 'white'),
          strip.text.x = element_text(size = 24, color = 'white'))
  
  # save
  ggsave(paste(saveplace, '/stackedbar/stackedbar_dark_', sort_spp, '.png', sep = ''), 
         width = 12)
}

# run for target spp 
area_fn(plot_type = 'MYT', spp_code = 'MYTCAL', sort_spp = 'Mytilus')
area_fn(plot_type = 'CHT', spp_code = 'CHTBAL', sort_spp = 'Balanus Chthamalus')
area_fn(plot_type = 'CHT', spp_code = 'TETRUB', sort_spp = 'Tetraclita')
area_fn(plot_type = 'POL', spp_code = 'POLPOL', sort_spp = 'Pollicipes')
area_fn(plot_type = 'SIL', spp_code = 'SILCOM', sort_spp = 'Silvetia')

### common name plot versions
area_fn <- function(plot_type, spp_code, sort_spp){
  
  # wrangle data
  dataset <- target_90 %>%
    filter(type == plot_type) %>%
    # calculate cover of each taxa across plot replicates
    group_by(survey_year, zone, taxa_code, scientific_name) %>%
    summarize(pct_cover = sum(pct_cover)) %>%
    ungroup() %>%
    # get sum of cover for each year
    group_by(survey_year, zone) %>%
    mutate(cover_sum = sum(pct_cover)) %>%
    ungroup() %>%
    # calculate % cover 
    mutate(pct_cover = (pct_cover/cover_sum)*100) %>%
    # make pretty label columns, nicer spp names
    mutate(common_name = case_when(
      scientific_name == 'Mytilus' ~ 'Mussel',
      scientific_name =='Tetraclita rubescens' ~ 'Volcano barnacle',
      scientific_name =='Pollicipes polymerus' ~ 'Goose barnacle',
      scientific_name =='Silvetia compressa' ~ 'Golden rockweed',
      scientific_name == 'Balanus/Chthamalus' ~ 'Acorn barnacles',
      TRUE ~ scientific_name))
  
  
  # step 2 - generate colors associated with taxa for consistent color assignment
  #consistent_colors = setNames(object = pnw_palette(name = 'Starfish',
                                                   # n = 9, type = 'continuous'), 
                               #nm = sort(unique(dataset$common_name)))
  
  # tidepool color palette from calecopal
  # https://github.com/an-bui/calecopal
  consistent_colors = setNames(object = cal_palette(name = "tidepool", 
                                                    n = 9, type = "continuous"), 
                               nm = sort(unique(dataset$common_name)))
  
  # change target spp color to "white"
  consistent_colors[sort_spp] = 'white'
  
  # dark theme plot
  ggplot(data = dataset,
         mapping = aes(x = survey_year, y = pct_cover, 
                       # make target spp last (bottom bar on plots)
                       fill = fct_relevel(common_name, sort_spp, after = Inf))) +
    # bar stacked by spp group (Scientific_name)
    geom_bar(stat = 'identity', width = 1) + 
    # axis and plot labels
    xlab('Year') + 
    ylab('Percent Cover') + 
    # colors
    scale_fill_manual(name = 'Taxon',
                      values = consistent_colors) +
    facet_wrap(~zone) +
    dark_theme + 
    theme(
      text = element_text(size = 26, color = 'white'),
      axis.text.x = element_text(size = 24, color = 'white'),
      axis.text.y = element_text(size = 24, color = 'white'),
      strip.text.x = element_text(size = 24, color = 'white'))
  
  # save
  ggsave(paste(saveplace, '/stackedbar/stackedbar_common_dark_', sort_spp, 
               '.png', sep = ''), width = 12)
}

area_fn(plot_type = 'MYT', spp_code = 'MYTCAL', sort_spp = 'Mussel')
area_fn(plot_type = 'CHT', spp_code = 'CHTBAL', sort_spp = 'Acorn barnacles')
area_fn(plot_type = 'CHT', spp_code = 'TETRUB', sort_spp = 'Volcano barnacle')
area_fn(plot_type = 'POL', spp_code = 'POLPOL', sort_spp = 'Goose barnacle')
area_fn(plot_type = 'SIL', spp_code = 'SILCOM', sort_spp = 'Golden rockweed')

##### community dynamics - multivariate #####

# pivot from long to wide format for community analyses
target_new <- select(target_90, -scientific_name) %>% 
  pivot_wider(names_from = taxa_code, values_from = pct_cover) %>%
  # fill in NA's with 0's
  mutate_all(~replace(., is.na(.), 0))

# PERMANOVA - do plots and zones differ in composition? 
permanova_fn <- function(plot_type){

# make spp and env datasets
target_spp <- target_new %>% filter(type == plot_type) %>%
                             select(-c(survey_year:zone))
target_env <- target_new %>% filter(type == plot_type) %>%
                             select(survey_year:zone)

# run PERMANOVA to detect time & plot code (run each factor alone first) - save results as csv
  # year
  write_csv(adonis(target_spp ~ survey_year, 
                   data = target_env, permutations = 999)$aov.tab,
            paste0(saveplace, 'stats_tables/permanova_yr_', 
                   plot_type, '.csv'))
  # code
  write_csv(adonis(target_spp ~ zone, 
                   data = target_env, permutations = 999)$aov.tab,
            paste0(saveplace, 'stats_tables/permanova_zone_', plot_type,
                   '.csv'))
  # both (+ interaction)
  write_csv(adonis(target_spp ~ survey_year*zone, 
                   data = target_env, permutations = 999)$aov.tab,
            paste0(saveplace, 'stats_tables/permanova_yr_zone_',
                  plot_type, '.csv'))

# plot results
# tutorial: https://chrischizinski.github.io/rstats/adonis/
mds <- metaMDS(target_spp)

# make new df with nmds 1 and 2 and id info
scores <- cbind(as_tibble(scores(mds)), target_env) %>%
  rename(Site = zone)


# calculate distance matrix
dist_matrix <- vegdist(target_spp)

# calculate dispersions (survey year, plot code, zone)
disper <- betadisper(dist_matrix, target_env$zone)

# get mds vectors
vectors <- as_tibble(scores(mds, 'species')) %>%
  mutate(spp = colnames(target_spp)) %>%
  filter(!is.na(NMDS1))

# plot mod results with ggplot2
ordplot <- ggplot() +
  stat_ellipse(data = scores,
               aes(x = NMDS1, y = NMDS2, color = Site), size = 1) + 
  geom_point(data = scores,
             aes(x = NMDS1, y = NMDS2, color = Site), alpha = 0.3) +
  geom_segment(data = vectors, mapping = aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               color = 'black') + 
  scale_color_manual(name = 'Zone', values = ecopal_3) + 
  geom_text(data = filter(vectors, NMDS1 > 0),
            mapping = aes(x = NMDS1, y = NMDS2, label = spp, 
                                          hjust = 0), color = 'black') +
  geom_text(data = filter(vectors, NMDS1 < 0),
            mapping = aes(x = NMDS1, y = NMDS2, label = spp, 
                                          hjust = 1), color = 'black') +
  labs(subtitle = paste0(case_when(plot_type == 'MYT' ~ 'Mytilus',
                                   plot_type == 'CHT' ~ 'Chthamalus/Balanus',
                                   plot_type == 'SIL' ~ 'Silvetia',
                                   plot_type == 'POL' ~ 'Pollicipes',
                                   T ~ 'You messed up'), 
                         ', Stress = ', round(mds$stress, digits = 2))) + 
  light_theme +
  xlim(c(-1.5,1.5)) + 
  coord_equal() + 
  theme(aspect.ratio = 1,
        axis.text.x = element_text(angle = 0, hjust = 0.5))

ggsave(ordplot, filename = paste0(saveplace, 'ordination/', plot_type, '_nmds.png'),
       width = 5)

# ord without arrows 
ordplot <- ggplot() +
  stat_ellipse(data = scores,
               aes(x = NMDS1, y = NMDS2, color = Site), size = 1) + 
  geom_point(data = scores,
             aes(x = NMDS1, y = NMDS2, color = Site), alpha = 0.5) +
  scale_color_manual(name = 'Zone', values = ecopal_3) + 
  labs(subtitle = paste0(case_when(plot_type == 'MYT' ~ 'Mytilus',
                                   plot_type == 'CHT' ~ 'Chthamalus/Balanus',
                                   plot_type == 'SIL' ~ 'Silvetia',
                                   plot_type == 'POL' ~ 'Pollicipes',
                                   T ~ 'You messed up'), 
                         ', Stress = ', round(mds$stress, digits = 2))) + 
  light_theme +
  xlim(c(-1.5,1.5)) + 
  coord_equal() + 
  theme(aspect.ratio = 1,
        axis.text.x = element_text(angle = 0, hjust = 0.5))

ggsave(ordplot, filename = paste0(saveplace, 'ordination/', plot_type, '_no_arrow_nmds.png'),
       width = 4)

# dark theme ord plots
ordplot <- ggplot() +
  stat_ellipse(data = scores,
               aes(x = NMDS1, y = NMDS2, color = Site), size = 2) + 
  geom_point(data = scores,
             aes(x = NMDS1, y = NMDS2, color = Site), alpha = 0.3, size = 3) +
  geom_segment(data = vectors, mapping = aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               color = 'white', size = 1.5) + 
  scale_color_manual(name = 'Zone', values = ecopal_3) + 
  geom_text(data = filter(vectors, NMDS1 > 0),
            mapping = aes(x = NMDS1, y = NMDS2, label = spp, 
                          hjust = 0), color = 'white', size = 4) +
  geom_text(data = filter(vectors, NMDS1 < 0),
            mapping = aes(x = NMDS1, y = NMDS2, label = spp, 
                          hjust = 1), color = 'white', size = 4) +
  labs(subtitle = paste0('Stress = ', round(mds$stress, digits = 2))) + 
  dark_theme +
  xlim(c(-1.5,1.5)) + 
  coord_equal() + 
  theme(aspect.ratio = 1,
        text = element_text(size = 18, color = 'white'),
        axis.text.x = element_text(angle = 0, hjust = 0, size = 18, color = 'white'),
        axis.text.y = element_text(size = 18, color = 'white'),
        strip.text.x = element_text(size = 18, color = 'white'))

ggsave(ordplot, filename = paste0(saveplace, 'ordination/', plot_type, 
                                  '_dark_nmds.png'), width = 5)

# dark theme, no arrows
ordplot <- ggplot() +
stat_ellipse(data = scores,
             aes(x = NMDS1, y = NMDS2, color = Site), size = 2) + 
  geom_point(data = scores,
             aes(x = NMDS1, y = NMDS2, color = Site), alpha = 0.7, size = 2) +
  scale_color_manual(name = 'Zone', values = ecopal_3) + 
  labs(subtitle = paste0('Stress = ', round(mds$stress, digits = 2))) + 
  dark_theme +
  xlim(c(-1.5,1.5)) + 
  coord_equal() + 
  theme(aspect.ratio = 1,
        text = element_text(size = 22, color = 'white'),
        axis.text.x = element_text(angle = 0, hjust = 0, size = 22, color = 'white'),
        axis.text.y = element_text(size = 22, color = 'white'),
        strip.text.x = element_text(size = 22, color = 'white'))


ggsave(ordplot, filename = paste0(saveplace, 'ordination/', plot_type, 
                                  '_dark_no_arrow_nmds.png'), height = 7)
}

permanova_fn(plot_type = 'MYT')
permanova_fn(plot_type = 'CHT')
permanova_fn(plot_type = 'SIL')
permanova_fn(plot_type = 'POL')

