library(PAMpal)
library(tidyverse)

#MAKE STUDY ----
#Known issue: problem processing Bangarang data, params are incorrect
myPps <- PAMpalSettings()

#set sr_hz = 'auto', filterfrom_kHz = 100, filterto_kHz = 180, winLen_sec = 0.0025
myStudy <- processPgDetections(myPps,
                               mode = 'db',
                               #in second_training, all clicks between event
                               #start and end times were included, no attempt
                               #to select
                               id = 'second_training')


myStudy <- setSpecies(myStudy, method = 'pamguard')
myStudy <- addGps(myStudy)
saveRDS(myStudy, "analysis/data/derived_data/second_training/myStudy2.rds")
