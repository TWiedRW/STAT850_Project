## ---------------------------
##
## Script name: Exploration_TW
##
## Purpose of script:This is my exploration script
##
## Author: Tyler Wiederich
##
## Date Created: 2021-10-12
##
## ---------------------------
##
## Notes:
##   
## apparatus is the dispatch report
## incidents is the incidents report
##
## ---------------------------

library(tidyverse)
library(lubridate)
options(scipen = 999)

apparatus = read.csv('LFR_apparatus_2019_CLEAN.csv')
incidents = read.csv('LFR_incidents_2019_CLEAN.csv')

apparatus2 = apparatus %>% 
  mutate(ctm_received = mdy_hms(ctm_received),
         ctm_dispatch = mdy_hms(ctm_dispatch),
         ctm_enroute = mdy_hms(ctm_enroute),
         ctm_arrive = mdy_hms(ctm_arrive),
         ctm_clear = mdy_hms(ctm_clear),
         total_commit_time = ctm_clear - ctm_received)

joined = full_join(apparatus2, incidents, by = c('incno' = 'incno'))


#Looking at amount of time it takes for first responder to arrive
first_responder = apparatus2 %>% 
  group_by(incno) %>% 
  filter(ctm_arrive == min(ctm_arrive, na.rm = TRUE),
         is.na(ctm_arrive) == F,
         ctm_arrive > ctm_received) %>% 
  ungroup() %>% 
  mutate(time_to_arrive_min = as.numeric(ctm_arrive - ctm_received)/60)

summary(first_responder$time_to_arrive_min)


fr_log = first_responder %>% 
  ggplot(mapping = aes(x = log(time_to_arrive_min))) +
  geom_density() +
  facet_wrap(~station)
fr_log  #We see that this approximately log-normal

attach(first_responder)
mod = aov(log(time_to_arrive_min) ~ as.character(station))

anova(mod)
TukeyHSD(mod, conf.level = 0.95) %>% broom::tidy() %>% 
  filter(adj.p.value <= 0.05) %>% 
  mutate(estimate = exp(estimate),
         conf.low = exp(conf.low),
         conf.high = exp(conf.high)) %>% 
  View()

read

TukeyHSD(mod) %>% plot()

#Join weather and daily incidents
weather = read.csv('weather.csv')
weather = weather %>% 
  #filter(STATION == 'US10lanc043') %>% 
  mutate(DATE = as.Date(DATE))


dat_weath = apparatus2 %>% 
  select(incno, ctm_dispatch) %>% 
  mutate(ctm_dispatch = lubridate::as_date(ctm_dispatch)) %>% 
  distinct() %>% 
  group_by(ctm_dispatch) %>% 
  summarize(Count = n()) %>% 
  full_join(weather, by = c('ctm_dispatch' = 'DATE'))

dat_weath %>% 
  select(ctm_dispatch, PRCP, Count) %>% 
  reshape2::melt(id.vars = 'ctm_dispatch') %>% 
  ggplot(mapping = aes(x = ctm_dispatch)) +
  geom_point(mapping = aes(y = value)) +
  facet_wrap(~variable,
             nrow = 2,
             scales = 'free_y')








joined %>% 
  mutate(date = as.Date(ctm_dispatch)) %>% 
  select(incno, date) %>% 
  distinct() %>% 
  group_by(date) %>% 
  summarise(count = n()) %>% 
  mutate(month = lubridate::month(date, label = T),
         dayofweek = lubridate::wday(date, label = T)) %>% 
  filter(is.na(date) == F) %>% 
  ggplot(mapping = aes(x = dayofweek, y = month,
                       fill = count)) +
  geom_raster() +
  theme_minimal()


# Seeing if traffic accidents and incidents are related or not
traffic <- read_csv("Traffic_Crashes_2019.csv") %>% 
  mutate(DOA = as.POSIXct(DOA/1000, origin = '1970-01-01'),
         Date = as.Date(DOA)) %>% 
  filter(year(Date) == 2019)


traffic %>% 
  group_by(Date) %>% 
  summarise(Count = n()) %>% 
  ggplot(aes(x = Date, y = Count)) +
  geom_line()


traffic_summ = traffic %>% 
  group_by(Date) %>% 
  summarise(Count_acc = n()) %>% 
  filter(is.na(Date) == F)

inc_summ = joined %>% 
  mutate(Date = as.Date(ctm_dispatch)) %>% 
  select(Date, incno) %>% 
  unique() %>% 
  group_by(Date) %>% 
  summarise(Count = n()) %>% 
  mutate(wday = wday(Date, label = T),
         month = month(Date, label = T)) %>% 
  filter(is.na(Date) == F)
  
inc_traff = inc_summ %>% 
  full_join(traffic_summ, by = 'Date') 

cor(inc_traff$Count, inc_traff$Count_acc) #Very small correlation, probably not worth looking into


#Checking day of week and month model (wday and month)
#Note: unequal sample sizes for combinations, so Type III test needed
library(car)
mod_mw = lm(Count ~ wday*month, data = inc_summ)
Anova(mod_mw, type = 3)
#anova(mod_mw) #you can see how the Type I (R default) is different that the Type III

#Remove interaction
mod_mw2 = lm(Count ~ wday + month, data = inc_summ)
Anova(mod_mw2, type = 2) #Type 2 or Type 3 for no interaction

unloadNamespace('car') #unload package since it masks some Tidyverse functions

#Checking month sig
mod_m = lm(Count ~ month, data = inc_summ)
anova(mod_m)
Anova(mod_m, type = 3)
summary(mod_m)

#Month is sig, further exploration
TukeyHSD(mod_m) %>% 
  broom::tidy() %>% 
  filter(adj.p.value <= 0.05)






