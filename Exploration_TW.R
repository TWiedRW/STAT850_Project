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
TukeyHSD(mod, conf.level = 0.99) %>% broom::tidy() %>% 
  filter(adj.p.value <= 0.05) %>% 
  mutate(estimate = exp(estimate),
         conf.low = exp(conf.low),
         conf.high = exp(conf.high)) %>% 
  View()


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






