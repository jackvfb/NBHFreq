library(tidyverse)
library(PAMpal)
library(FSA)
library(broom)

myStudy <- readRDS('analysis/data/derived_data/second_training/myStudy2.rds')
dets <- as_tibble(getClickData(myStudy))

dets <- dets %>%
  #remove repeat UIDs, don't know why these exist but they do
  distinct(UID, Channel, .keep_all = TRUE) %>%
  #drop dets with NA values, with some exceptions for variables like Lat/Lon
  drop_na(-c(Latitude, Longitude, gpsUncertainty, angle, angleError)) %>%
  #recode species to be more interpretable
  mutate(species = if_else(species == "NBHF", "Unk", species))


spectra <- dets %>%
  #discard 10_dB metrics in lieu of 3_dB metrics (per Zahn 2021)
  select(-c(Q_10dB:centerkHz_10dB, BinaryFile, detectorName, eventLabel, db)) %>%
  #make pretty
  relocate(c(UID, Channel, eventId, species)) %>%
  #make even more pretty
  relocate(gpsUncertainty, .after = Longitude)
  # #add_count grouped by eventId for later plotting
  # add_count(eventId, Channel)

metadata <- dets %>%
  #select event metadata
  select(UID, Channel, eventId, species) %>%
  #group by event to calculate n
  group_by(eventId, Channel) %>%
  #summarize
  summarize(n = n(), species = unique(species)) %>%
  arrange(desc(n))

# -------------------------------------------------------------------------


spectra %>%
  filter(species == "Unk") %>%
  ggplot() +
  geom_violin(aes(fct_reorder(eventId, n, .desc = TRUE), peak, color = Channel))

spectra %>%
  filter(species == "Kosp") %>%
  ggplot() +
  geom_violin(aes(fct_reorder(eventId, n, .desc = TRUE), peak, color = Channel))

spectra %>%
  filter(Channel == 2) %>%
  ggplot() +
  geom_violin(aes(fct_reorder(eventId, n, .desc = TRUE), peak, color = species))


# -------------------------------------------------------------------------




kw <- spectra %>%
  filter(species != "Unk") %>%
  kruskal.test(peak ~ species, data = .)
kw

dunn <- spectra %>%
  filter(species != "Unk") %>%
  dunnTest(peak~species, data = ., method = "bh")
dunn

# PREP FOR DISTANCE MATRIX ----------------------------------------------------

#replace the combination of identifiers "eventId" and "species" with just
# a single identifier. In the case of events with a common species, the events
#are grouped, whereas in the case of unclassified events, the data is treated
#seperately.

#first part of join, containing all data for unclassified events, seperated by event
j1 <- spectra %>%
  #remove Ch 2 detections, filter for just Unk events
  filter(Channel == 1, species == "Unk") %>%
  #drop species identifier
  select(-species) %>%
  #nest all the parameters with the new identifier, which in this case will be eventId
  nest(data = -eventId) %>%
  #rename identifier so it can later be used to join
  rename(id = eventId)

#second part of join, containing all data for a priori classified events, seperated by species
j2 <- spectra %>%
  #remove Ch 2 detections, and filter for just the a priori classified events
  filter(Channel == 1, species != "Unk") %>%
  #drop the eventId identifier
  select(-eventId) %>%
  #nest all the parameters with the new identifier, which in this case will be species
  nest(data = -species) %>%
  #rename the identifier so it can be used to join with j1 from above
  rename(id = species)

#full join j1 and j2 to create new tibble "events" with all data
events <- full_join(j1, j2)

#perform battery of tests on all sample units to provide data for later computing a
#distance matrix

#define battery of tests
myFunc <- function(x) {
  x %>%
    summarize(across(where(is.numeric), mean))
  names(x)
  return()
}

#map test battery across each sample unit in the events data frame
events %>%
  mutate(n_event = map_dbl(data, nrow),
         means = map(.x = data, ~myFunc(.x))) %>%
  unnest(means)
