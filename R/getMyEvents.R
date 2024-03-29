library(tidyverse)
library(PAMpal)

#Extract tidy data from Acoustic Study
myStudy <- readRDS('analysis/data/derived_data/training/trainingStudy.rds')
dets <- as_tibble(getClickData(myStudy))

# DATA WRANGLE ------------------------------------------------------------

dets <- dets %>%
  #remove repeat UIDs, don't know why these exist but they do
  distinct(UID, Channel, .keep_all = TRUE) %>%
  #drop dets with NA values, with some exceptions for variables like Lat/Lon
  drop_na(-c(Latitude, Longitude, gpsUncertainty, angle, angleError)) %>%
  #recode species to be more interpretable
  mutate(species = if_else(species == "NBHF", "Unk", species))
  # mutate(region = case_when(species == "Unk" ~ "N",
  #                           species == "Kosp" ~ "S",
  #                           .default = "GT")) %>%
  # mutate(region = factor(region, levels = c("N", "S", "GT")))


clicks <- dets %>%
  #discard 10_dB metrics in lieu of 3_dB metrics (per Zahn 2021)
  select(-c(Q_10dB:centerkHz_10dB, BinaryFile, detectorName, eventLabel, db)) %>%
  #make pretty
  relocate(c(UID, Channel, eventId, species)) %>%
  #make even more pretty
  relocate(gpsUncertainty, .after = Longitude) %>%
  mutate(numPk = as_factor(case_when(peak3 != 0 ~ 3,
                                     peak2 != 0 ~ 2,
                                     .default = 1))) %>%
  mutate(Channel = as.factor(Channel))
# #add_count grouped by eventId for later plotting
# add_count(eventId, Channel)

metadata <- dets %>%
  #select event metadata
  select(UID, Channel, eventId, species) %>%
  #turn Channel into a numeric instead of a char
  mutate(Channel = as.factor(Channel)) %>%
  #group by event to calculate n
  group_by(eventId, Channel) %>%
  #summarize
  summarize(n = n(), species = unique(species)) %>%
  arrange(desc(n))

# EXTRA STUFF FOR RIP TALK ------------------------------------------------

##Will constrain to Channel 1 for generating figures for RIP talk
slide_clicks <- clicks %>%
  mutate(species = case_when(species == "Kosp" ~ "*Kogia* spp.",
                             species == "Phda" ~ "Dall's porpoise",
                             species == "Phph" ~ "Harbor porpoise")) %>%
  mutate(species = factor(species, levels = c("Harbor porpoise",
                                              "Dall's porpoise",
                                              "*Kogia* spp."))) %>%
  filter(as.numeric(Channel) == 1)
