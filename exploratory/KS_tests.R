library(tidyverse)
library(PAMpal)
library(FSA)
library(broom)
library(vegan)

myStudy <- readRDS('analysis/data/derived_data/second_training/myStudy2.rds')
dets <- as_tibble(getClickData(myStudy))


# DATA WRANGLE ------------------------------------------------------------

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

# RAW DATA VIZ---------------------------------------------------------------------
spectra %>%
  group_by(eventId) %>%
  add_count() %>%
  ggplot(aes(fct_reorder(eventId, n, .desc = TRUE), fill = species))+
  stat_count()+
  facet_wrap(~Channel)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "N of clicks in all events, colored by species")

spectra %>%
  filter(species == "Unk") %>%
  group_by(eventId) %>%
  add_count() %>%
  ggplot() +
  geom_violin(aes(fct_reorder(eventId, n, .desc = TRUE), peak, color = Channel))+
  labs(title = "Distribution of click peaks in Unknown events, arranged in order of n")

spectra %>%
  filter(species == "Kosp") %>%
  group_by(eventId) %>%
  add_count() %>%
  ggplot() +
  geom_violin(aes(fct_reorder(eventId, n, .desc = TRUE), peak, color = Channel))+
  labs(title = "Distribution of click peaks in Kogia events, arranged in order of n")

spectra %>%
  group_by(Channel, eventId) %>%
  add_count() %>%
  ggplot() +
  geom_violin(aes(fct_reorder(eventId, n, .desc = TRUE), peak, color = species))+
  facet_wrap(~Channel)+
  labs(title = "Distribution of click peaks in all events, arranged in order of n")


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
  #drop vars that will not be used downstream to calculate distance
  select(UID, eventId, duration:centerkHz_3dB) %>%
  #nest all the parameters with the new identifier, which in this case will be eventId
  nest(data = -eventId) %>%
  #rename identifier so it can later be used to join
  rename(id = eventId)

#second part of join, containing all data for a priori classified events, seperated by species
j2 <- spectra %>%
  #remove Ch 2 detections, and filter for just the a priori classified events
  filter(Channel == 1, species != "Unk") %>%
  #drop vars that will not be used downstream to calculate distance
  select(UID, species, duration:centerkHz_3dB) %>%
  #nest all the parameters with the new identifier, which in this case will be species
  nest(data = -species) %>%
  #rename the identifier so it can be used to join with j1 from above
  rename(id = species)

#full join j1 and j2 to create new tibble "events" with all data
events <- full_join(j1, j2)

#perform battery of tests on all sample units to provide data for later computing a
#distance matrix

#define a battery of tests to measure central tendency across all parameters
allColCtr <- function(x) {
  #summarize across columns to produce central measure every variable
  x <- x %>%
    summarize(across(where(is.numeric), median))
  #rename columns with the prefix _mean
  x <- x %>%
    rename_with(~paste0("median_", .x))
  return(x)
}

#perform battery of tests across each sample unit
dist <- events %>%
  mutate(n_event = map_dbl(data, nrow),
         centers = map(.x = data, ~allColCtr(.x))) %>%
  unnest(centers) %>%
  select(-c(data, n_event)) %>%
  column_to_rownames(var = "id") %>%
  as.matrix() %>%
  scale() %>%
  vegdist(method = "mahalanobis")

set.seed(18)
nmds <- metaMDS(dist)

#no clusters :(
scores(nmds) %>%
  as_tibble(rownames = "id") %>%
  ggplot(aes(x = NMDS1, y = NMDS2))+
  geom_point()

# COLLECTORS CURVES -----------------------------------------

#Collector's curve
spectra %>%
  select(UID, Channel, eventId, centerkHz_3dB, species) %>%
  filter(Channel == 1) %>%
  mutate(x = cut(centerkHz_3dB,
                 breaks = seq(from = floor(min(centerkHz_3dB)),
                              to = ceiling(max(centerkHz_3dB)),
                              by = 0.1))) %>%
  group_by(eventId) %>%
  summarize(n_U = length(unique(x)), n = n(), species = species) %>%
  ggplot(aes(x = n, y = n_U, color = species))+
  geom_point()+
  # geom_abline(slope = 1, intercept = 0)+
  scale_y_log10()

#Collector's curve for peak which has fewer significant figures,
#clearly the same relationship but the absolute values on the y axis are less
spectra %>%
  select(UID, Channel, eventId, peak, species) %>%
  filter(Channel == 1) %>%
  mutate(x = cut(peak,
                 breaks = seq(from = floor(min(peak)),
                              to = ceiling(max(peak)),
                              by = 0.1))) %>%
  group_by(eventId) %>%
  summarize(n_U = length(unique(x)), n = n(), species = species) %>%
  ggplot(aes(x = n, y = n_U, color = species))+
  geom_point()+
  # geom_abline(slope = 1, intercept = 0)+
  scale_y_log10()

# SUMMARY -----------------------------------------------------------------

#summary of units by how many events they appear in.
spectra %>%
  select(Channel, eventId, centerkHz_3dB) %>%
  filter(Channel == 1) %>%
  select(-Channel) %>%
  mutate(u = cut(centerkHz_3dB,
                 breaks = seq(from = floor(min(centerkHz_3dB)),
                              to = ceiling(max(centerkHz_3dB)),
                              by = 0.1))) %>%
  group_by(u) %>%
  summarize(n_Ev = length(unique(eventId))) %>%
  arrange(desc(n_Ev))%>%
  print(n = Inf)


# -------------------------------------------------------------------------
sampling_coverage <- spectra %>%
  select(UID, Channel, eventId, centerkHz_3dB, species) %>%
  filter(Channel == 1) %>%
  mutate(clicks = cut(centerkHz_3dB,
                 breaks = seq(from = floor(min(centerkHz_3dB)),
                              to = ceiling(max(centerkHz_3dB)),
                              by = 2))) %>%
  group_by(eventId) %>%
  summarize(n = n(), n_peaks = length(unique(clicks)))

sampling_coverage %>%
  ggplot(aes(n_peaks))+
  geom_histogram()+
  coord_cartesian(xlim = c(0, 50))

sampling_coverage %>%
  ggplot(aes(x = 1, y = n_clicks))+
  geom_jitter()+
  scale_y_log10()

sampling_coverage %>%
  arrange(n_clicks) %>%
  ggplot(aes(x = 1:nrow(.), y = n_clicks))+
  geom_line()+
  scale_y_log10()

# NMDS CTR_3kHz -----------------------------------------------------------------

ctr_3kHz <- spectra %>%
  select(Channel, eventId, centerkHz_3dB) %>%
  filter(Channel == 1) %>%
  select(-Channel) %>%
  mutate(name = as.character(cut(centerkHz_3dB,
                                 breaks = seq(from = floor(min(centerkHz_3dB)),
                                               to = ceiling(max(centerkHz_3dB)),
                                               by = .1)))) %>%
  select(-centerkHz_3dB) %>%
  group_by(eventId, name) %>%
  mutate(value = n()) %>%
  distinct(eventId, name, .keep_all = TRUE) %>%
  pivot_wider(id_cols = eventId, values_fill = 0) %>%
  as.data.frame()

rownames(ctr_3kHz) <- ctr_3kHz$eventId
ctr_3kHz <- ctr_3kHz[,-1]
ctr_3kHz <- as.matrix(ctr_3kHz)

set.seed(04291992)
dist <- vegdist(ctr_3kHz, method = "bray")
nmds <- metaMDS(dist)
scores(nmds) %>%
  as_tibble(rownames = "eventId") %>%
  inner_join(filter(metadata, Channel == 1), by = "eventId") %>%
  ggplot(aes(x = NMDS1, y = NMDS2, color = species))+
  geom_point()


# NMDS TROUGH ------------------------------------------------------------------
trough <- spectra %>%
  select(Channel, eventId, trough) %>%
  filter(Channel == 1) %>%
  select(-Channel) %>%
  mutate(name = as.character(cut(trough,
                                 breaks = seq(from = floor(min(trough)),
                                              to = ceiling(max(trough)),
                                              by = 0.1)))) %>%
  select(-trough) %>%
  group_by(eventId, name) %>%
  mutate(value = n()) %>%
  distinct(eventId, name, .keep_all = TRUE) %>%
  pivot_wider(id_cols = eventId, values_fill = 0) %>%
  as.data.frame()

rownames(trough) <- trough$eventId
trough <- trough[,-1]
trough <- as.matrix(trough)

set.seed(04291992)
dist <- vegdist(trough, method = "bray")
nmds <- metaMDS(dist)
scores(nmds) %>%
  as_tibble(rownames = "eventId") %>%
  inner_join(filter(metadata, Channel == 1), by = "eventId") %>%
  ggplot(aes(x = NMDS1, y = NMDS2, color = species, size = n))+
  geom_point()

ggplot(dets)+
  geom_violin(aes(x = eventId, y = trough, color = species))
