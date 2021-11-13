## ---------------------------
##
## Script name: TW_source.R
##
## Purpose of script: Load options for report
##
## Author: Tyler Wiederich
##
## Date Created: 2021-11-12
##
## ---------------------------
##
## Notes:
##   This script loads objects required for my part of the report
##   Research questions: 
##      Check connections to weather, traffic accidents, etc.
##      Check significance for trends within dates
## ---------------------------


# Packages and data -------------------------------------------------------

library(tidyverse) #tidyverse packages
library(lubridate) #working with dates
library(car) #for Type II and III tests with unequal sample sizes
library(GGally)

options(scipen = 999)

#Original data sets
apparatus = read.csv('LFR_apparatus_2019_CLEAN.csv')
incidents = read.csv('LFR_incidents_2019_CLEAN.csv')
weather = read.csv('weather.csv') #https://www1.ncdc.noaa.gov/pub/data/cdo/documentation/GHCND_documentation.pdf
traffic = read.csv('Traffic_Crashes_2019.csv')
arrests = read.csv('LPD_arrestd_and_citations_2019.csv')
traffic_stops = read.csv('LPD_traffic_stops_2019.csv')
dispatch_lpd = read.csv('LPD_Dispatch_Records_2019.csv')

#Vehicle dispatches
apparatus2 = apparatus %>% 
  mutate(ctm_received = mdy_hms(ctm_received),
         ctm_dispatch = mdy_hms(ctm_dispatch),
         ctm_enroute = mdy_hms(ctm_enroute),
         ctm_arrive = mdy_hms(ctm_arrive),
         ctm_clear = mdy_hms(ctm_clear),
         total_commit_time = ctm_clear - ctm_received)

joined = full_join(apparatus2, incidents, by = c('incno' = 'incno'))

#Daily incident count
df_inc_summ_daily = joined %>% 
  mutate(Date = as_date(ctm_dispatch)) %>% 
  filter(is.na(Date) == F) %>% 
  select(incno, Date) %>% 
  unique() %>% #Since a record exists for each vehicle
  group_by(Date) %>% 
  summarize(Count = n()) %>% 
  mutate(wday = wday(Date, label = T),
         month = month(Date, label = T),
         week = epiweek(Date),
         wday_cat = ifelse(wday %in% c('Sun', 'Sat'), 'Weekend', 'Weekday'))

#Temperature
df_temps = weather %>% 
  mutate(Date = as_date(DATE)) %>% 
  select(STATION, Date, TMIN, TAVG ,TMAX) %>% 
  na.omit()
  
#Precipitation
df_precip = weather %>% 
  mutate(Date = as_date(DATE)) %>% 
  select(STATION, LATITUDE, LONGITUDE, Date, PRCP, SNOW, SNWD)
df_precip[is.na(df_precip)] <- 0

df_precip_avg = df_precip %>% 
  group_by(Date) %>% 
  summarise(precip = mean(PRCP),
            snow = mean(SNOW),
            snow_dpth = mean(SNWD))



# Trends for incidents with weather ---------------------------------------

#Joined data on precipitation averages
inc_weath = full_join(df_inc_summ_daily, 
                      df_precip_avg, 
                      by = c('Date' = 'Date'))

#Checking correlations with predictors
plot_pairs_precip = inc_weath %>% 
  select(Count, precip, snow, snow_dpth) %>% 
  ggpairs() +
  theme_bw() +
  ggtitle('Scatterplot Matrix of Dispatches and Precipitation')
plot_pairs_precip #No major correlations, can disregard



#Joined data on temperatures
inc_temp = full_join(df_inc_summ_daily,
                     df_temps,
                     by = 'Date')

plot_pairs_temp = inc_temp %>% 
  select(Count, TMIN, TAVG, TMAX) %>% 
  ggpairs() +
  theme_bw() +
  ggtitle('Scatterplot Matrix of Dispatches and Temperature')
plot_pairs_temp #No major correlations, can disregard


# Trends for incidents with traffic ----------------------------------------

#Joined incidents and traffic
traffic_summ = traffic %>% 
  mutate(Date = as_date(as.POSIXct(DOA/1000, origin = '1970-1-1'))) %>% 
  filter(is.na(year(Date)) == F, 
         year(Date) == 2019) %>% 
  group_by(Date) %>% 
  summarise(Accidents = n())

inc_traff = df_inc_summ_daily %>% 
  full_join(traffic_summ, by = 'Date')

#cor(inc_traff$Count[1:364], inc_traff$Accidents[1:364]) #Cor = 0.039, not worth checking into



# Breakdown of dates ------------------------------------------------------

#Monthly breakdown of dispatches
mod_mt = aov(Count ~ month, data = df_inc_summ_daily)

anova_mt = Anova(mod_mt, type = 3) %>% broom::tidy()

diff_mt = TukeyHSD(mod_mt) %>% 
  broom::tidy() %>% 
  filter(adj.p.value < 0.05) %>% 
  mutate(adj.p.value = round(adj.p.value, 4))

plot_mt = ggplot(df_inc_summ_daily, 
                 aes(x = month, y = Count)) +
  geom_boxplot(fill = 'skyblue') +
  theme_bw() +
  labs(title = 'Distribution of Dispatches Count per day by Month',
       x = '',
       y = 'Count of Dispatches',
       caption = paste0(
         'p-values for significant contrasts\n',
         diff_mt$contrast[1], ': ', diff_mt$adj.p.value[1],'\n',
         diff_mt$contrast[2], ': ', diff_mt$adj.p.value[2],'\n',
         diff_mt$contrast[3], ': ', diff_mt$adj.p.value[3]
       ))
plot_mt
  


#Day of week breakdown of dispatches
mod_wcat = aov(Count ~ wday_cat, data = df_inc_summ_daily)

anova_wcat = Anova(mod_wcat, type = 3) %>% broom::tidy()

diff_wcat = TukeyHSD(mod_wcat) %>% 
  broom::tidy() %>% 
  filter(adj.p.value < 0.05) %>% 
  mutate(adj.p.value = round(adj.p.value, 4),
         estimate = round(estimate, 2))

plot_wcat = ggplot(df_inc_summ_daily, 
                 aes(x = wday_cat, y = Count)) +
  geom_boxplot(fill = 'skyblue') +
  theme_bw() +
  labs(title = 'Distribution of Dispatches Count per day by Day Type',
       x = '',
       y = 'Count of Dispatches',
       caption = paste0(
         diff_wcat$contrast[1], '\np-value: ', diff_wcat$adj.p.value[1],
         '\nEstimate: ', diff_wcat$estimate)) +
  coord_flip()
plot_wcat

  
  
