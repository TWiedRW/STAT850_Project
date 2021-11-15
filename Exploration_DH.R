library(tidyverse)
incidents <- read.csv("LFR_incidents_2019_CLEAN.csv")
apparatus <- read.csv("LFR_apparatus_2019_CLEAN.csv")
joined = full_join(apparatus, incidents, by = c('incno' = 'incno'))
library(skimr)


#Top 5 call types for each station
apparatus2 %>% 
  group_by(station, call_type) %>% 
  summarise(Count = n()) %>% 
  arrange(station, desc(Count)) %>% 
  mutate(Rank = rank(desc(Count), ties.method = 'average')) %>% 
  ungroup() %>% 
  group_by(station) %>% 
  mutate(prop = Count/sum(Count)) %>% 
  filter(Rank <= 5 & station != 98 & station != 0) %>% 
  ungroup() %>% 
  ggplot() +
  geom_point(aes(x = station, y = call_type, color = prop),
             show.legend = T, size = 5) +
  theme_bw() +
  labs(title = 'Top Five Call Types by Station',
       x = 'Station',
       y = 'Call Type',
       color = 'Count') +
  scale_x_continuous(breaks = 1:15) +
  ggthemes::scale_color_gradient_tableau('Blue-Teal') +
  theme(panel.grid.minor = element_blank())


#Top 5 units used for each station
apparatus2 %>% 
  group_by(station, unit_descr) %>% 
  summarise(Count = n()) %>% 
  arrange(station, desc(Count)) %>% 
  mutate(Rank = rank(desc(Count), ties.method = 'average')) %>% 
  ungroup() %>% 
  group_by(station) %>% 
  mutate(prop = Count/sum(Count)) %>% 
  filter(Rank <= 5 & station != 98 & station != 0) %>% 
  ungroup() %>% 
  ggplot() +
  geom_point(aes(x = station, y = unit_descr, color = prop),
             show.legend = T, size = 5) +
  theme_bw() +
  labs(title = 'Top Five Units Used by Station',
       x = 'Station',
       y = 'Unit Description',
       color = 'Count') +
  scale_x_continuous(breaks = 1:15) +
  ggthemes::scale_color_gradient_tableau('Blue-Teal') +
  theme(panel.grid.minor = element_blank())

  
#Top 5 incidents responded too by each station
apparatus2 %>% 
  group_by(station, inc_desc) %>% 
  summarise(Count = n()) %>% 
  arrange(station, desc(Count)) %>% 
  mutate(Rank = rank(desc(Count), ties.method = 'average')) %>% 
  ungroup() %>% 
  group_by(station) %>% 
  mutate(prop = Count/sum(Count)) %>% 
  filter(Rank <= 5 & station != 98 & station != 0) %>% 
  ungroup() %>% 
  ggplot() +
  geom_point(aes(x = station, y = inc_desc, color = prop),
             show.legend = T, size = 5) +
  theme_bw() +
  labs(title = 'Top Five IncidentsResponded too by Station',
       x = 'Station',
       y = 'Incident Description',
       color = 'Count') +
  scale_x_continuous(breaks = 1:15) +
  ggthemes::scale_color_gradient_tableau('Blue-Teal') +
  theme(panel.grid.minor = element_blank())


#Top 5 Actions taken by each station
apparatus2 %>% 
  group_by(station, action_desc) %>% 
  summarise(Count = n()) %>% 
  arrange(station, desc(Count)) %>%
  filter(is.na(action_desc) == F) %>% 
  mutate(Rank = rank(desc(Count), ties.method = 'average')) %>% 
  ungroup() %>% 
  group_by(station) %>% 
  mutate(prop = Count/sum(Count)) %>% 
  filter(Rank <= 5 & station != 98 & station != 0) %>% 
  ungroup() %>% 
  ggplot() +
  geom_point(aes(x = station, y = action_desc, color = prop, size = Rank),
             show.legend = T, size = 5) +
  theme_bw() +
  labs(title = 'Top Five Actions taken by Station',
       x = 'Station',
       y = 'Action Taken',
       color = 'Count') +
  scale_x_continuous(breaks = 1:15) +
  ggthemes::scale_color_gradient_tableau('Blue-Teal') +
  theme(panel.grid.minor = element_blank())






apparatus2 %>% 
  mutate(total_responce_time = (ctm_arrive - ctm_dispatch)) %>% 
  group_by(station, total_responce_time) %>% 

  filter(station != 98  ) %>% 
  ggplot() +
  geom_point(aes(x = station, y = total_responce_time))+
  ylim(0,6500)
             
  theme_bw() +
  labs(title = 'Average Response Time by Station',
       x = 'Station',
       y = 'Average Response Time',
       color = 'Count') +
  scale_x_continuous(breaks = 1:15) 
















#Vehicle dispatches
apparatus2 = apparatus %>% 
  mutate(ctm_received = mdy_hms(ctm_received),
         ctm_dispatch = mdy_hms(ctm_dispatch),
         ctm_enroute = mdy_hms(ctm_enroute),
         ctm_arrive = mdy_hms(ctm_arrive),
         ctm_clear = mdy_hms(ctm_clear),
         total_commit_time = ctm_clear - ctm_received)


ggplot(apparatus2) +
  geom_line(aes(x = ctm_arrive, y = turn_out_time))
