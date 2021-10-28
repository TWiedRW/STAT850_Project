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

apparatus = read.csv('LFR_apparatus_2019_CLEAN.csv')
incidents = read.csv('LFR_incidents_2019_CLEAN.csv')

apparatus2 = apparatus %>% 
  mutate(ctm_received = mdy_hms(ctm_received),
         ctm_dispatch = mdy_hms(ctm_dispatch),
         ctm_enroute = mdy_hms(ctm_enroute),
         ctm_arrive = mdy_hms(ctm_arrive),
         ctm_clear = mdy_hms(ctm_clear),
         total_commit_time = ctm_clear - ctm_received)

ggplot(apparatus2, mapping = aes(x = unit_descr, y = total_commit_time)) +
  geom_point() +
  scale_y_time(date_)


joined = full_join(apparatus, incidents, by = c('incno' = 'incno'))

test = joined %>% 
  group_by(incno, priority) %>% 
  summarise(number_vehicles = n(),
            priority = mean(priority),
            prop_loss = mean(prop_loss, na.rm = TRUE),
            prop_value = mean(prop_value, na.rm = T),
            percent_prop_loss = mean(prop_loss, na.rm = T) / mean(prop_value, na.rm = T))

  
test %>% 
  ggplot(mapping = aes(x = round(percent_prop_loss, 1), y = number_vehicles,
                       group = percent_prop_loss)) +
  geom_bar(stat = 'summary', fun = 'mean') 

apparatus %>% 
  mutate()
  group_by(incno) %>% 
  summarise(te)

