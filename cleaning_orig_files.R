library(tidyverse)

apparatus = read.csv('LFR_apparatus_2019.csv')
incidents = read.csv('LFR_incidents_2019.csv')

apparatus_clean = janitor::clean_names(apparatus)
incidents_clean = janitor::clean_names(incidents)

apparatus_clean[apparatus_clean=='NULL'] <- NA
incidents_clean[incidents_clean=='NULL'] <- NA

apparatus_clean[apparatus_clean=='#VALUE'] <- NA
incidents_clean[incidents_clean=='#VALUE'] <- NA

apparatus_clean[apparatus_clean==' '] <- NA
incidents_clean[incidents_clean==' '] <- NA

apparatus_clean[apparatus_clean==''] <- NA
incidents_clean[incidents_clean==''] <- NA


write.csv(apparatus_clean, file = 'LFR_apparatus_2019_CLEAN.csv')
write.csv(incidents_clean, file = 'LFR_incidents_2019_CLEAN.csv')