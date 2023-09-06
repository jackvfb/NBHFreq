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

# add GPS -----------------------------------------------------------------
gpsFiles <- list.files("analysis/data/raw_data/second_training/gps", full.names = TRUE)

gpsAll <- map(gpsFiles, read_csv)
# have to remove DeviceId column because sometimes char, sometimes double
gpsAll <- map(gpsAll, subset, select = -6)
gpsAll <- gpsAll %>%
  bind_rows() %>%
  select(Latitude, Longitude, UTC)


myStudy <- addGps(myStudy, gps = gpsAll)
saveRDS(myStudy, "analysis/data/derived_data/second_training/myStudy2.rds")
