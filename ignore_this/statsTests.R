library(tidyverse)
library(PAMpal)
library(broom)

myStudy <- readRDS('analysis/data/derived_data/second_training/myStudy2.rds')
dets <- as_tibble(getClickData(myStudy))

library(manipulate)
plotDataExplorer(myStudy)

#Dets must all be unique. I don't know how to interpret otherwise.
dets <- dets %>%
  distinct(UID, Channel, .keep_all = TRUE) %>%
  mutate(species = if_else(species == "NBHF", "Unk", species)) %>% #species more interpretable
  mutate(eventId = str_replace(eventId, ".OE", ".")) #eventId more interpretable

spectra <- dets %>%
  select(UID, Channel, noiseLevel:dBPP, Q_3dB:centerkHz_3dB) %>%
  drop_na() %>%
  mutate(numPk = case_when(peak3 != 0 ~ 3, peak2 != 0 ~ 2, .default = 1)) %>%
  relocate(numPk, .after = noiseLevel)

metadata_UID <- dets %>%
  select(UTC:angleError, gpsUncertainty, eventId, species) %>%
  relocate(c(UID, eventId, Channel, species)) %>%
  relocate(gpsUncertainty, .after = Longitude) %>%
  semi_join(spectra, by = c("Channel", "UID"))

metadata_event <- metadata_UID %>%
  group_by(eventId, Channel) %>%
  summarize(n = n(), species = unique(species)) %>%
  arrange(desc(n))

# KW TEST ALL EV ----------------------------------------------------------
metadata_UID %>%


# KW TEST Val~EventId by Species ----------------------------------------------------------

events_wanted <- metadata_event %>%
  filter(n >60)
  #group_by(Channel, species) %>%
  #summarize(species_events = n()) #use to check which species have more than one event to be included in the next stage

UID_wanted <- metadata_UID %>%
  semi_join(events_wanted, by = c("eventId", "Channel"))

null_Ch1 <- spectra %>%
  inner_join(UID_wanted, by = c("UID", "Channel")) %>%
  filter(Channel == 1, species == "Kosp" | species == "Phph") %>% #filter for the species we are comparing
  pivot_longer(cols = noiseLevel:centerkHz_3dB,
               names_to = "var",
               values_to = "val") %>%
  # group_by(var, species) %>%
  # summarize(N_evs = length(unique(eventId)),
  #           N_dets = n())
  nest(data = -c(var, species)) %>% #n = 303 Kogia obs, n = 4,587 Phph obs :P
  mutate(stats = map(.x = data, ~kruskal.test(val~eventId, data = .x) %>% tidy())) %>%
  unnest(stats) %>%
  mutate(p.adj = p.adjust(p.value, method = 'BH')) %>%
  select(-(p.value:method)) %>%
  arrange(var, species, desc(p.adj)) %>%
  print(n = Inf)

null_Ch2 <- spectra %>%
  inner_join(UID_wanted, by = c("UID", "Channel")) %>%
  filter(Channel == 2, species == "Kosp" | species == "Phph") %>% #filter for the species we are comparing
  pivot_longer(cols = noiseLevel:centerkHz_3dB,
               names_to = "var",
               values_to = "val") %>%
  nest(data = -c(var, species)) %>% #n = 303 Kogia obs, n = 4,587 Phph obs :P
  mutate(stats = map(.x = data, ~kruskal.test(val~eventId, data = .x) %>% tidy())) %>%
  unnest(stats) %>%
  mutate(p.adj = p.adjust(p.value, method = 'BH')) %>%
  select(-(p.value:method)) %>%
  arrange(desc(p.adj)) %>%
  print(n = Inf)


# SAME AS ABOVE BUT SEPERATED BY NUM PK ---------------------------------------

#n_event > 1 only will be included in this analysis
spectra %>%
  inner_join(UID_wanted, by = c("UID", "Channel")) %>%
  group_by(Channel, species, numPk) %>%
  summarize(n = n(),
            n_events = length(unique(eventId))) %>%
  print(n = Inf)

events_wanted <- metadata_event %>%
  filter(n >60)

spectra %>%
  inner_join(UID_wanted, by = c("UID", "Channel")) %>%
  filter(Channel == 1, species == "Kosp" | species == "Phph") %>%  #filter for the species we are comparing
  pivot_longer(cols = c(noiseLevel, duration:centerkHz_3dB),
               names_to = "var",
               values_to = "val") %>%
  # group_by(numPk, var, species) %>%
  # summarize(N_evs = length(unique(eventId)),
  #            N_dets = n()) %>%
  # print(n = Inf)
  nest(data = -c(var, species, numPk)) %>%
  mutate(stats = map(.x = data, ~kruskal.test(val~eventId, data = .x) %>% tidy())) %>%
  unnest(stats) %>%
  mutate(p.adj = p.adjust(p.value, method = 'BH')) %>%
  select(-(p.value:method)) %>%
  arrange(species, numPk, desc(p.adj)) %>%
  print(n = Inf)


# SMALLER PANEL OF TESTS--------------------------------------------------

events_wanted <- metadata_event %>%
  filter(n>75) %>%
  group_by(species) %>%
  mutate(n_events = length(unique(eventId))) %>% #see how many ev are left for each species after filter
  print(n = Inf)

UID_wanted <- metadata_UID %>%
  semi_join(events_wanted, by = c("eventId", "Channel")) %>%
  select(-(UTC:angleError))

spectra %>%
  inner_join(UID_wanted, by = c("UID", "Channel")) %>%
  filter(Channel == 1, species != "Phda", numPk == 1 | numPk == 2) %>%
  # group_by(species) %>%
  # summarize(n_dets = n(), n_ev = length(unique(eventId)))
  select(UID, Channel, peak, BW_3dB, eventId, species) %>%
  pivot_longer(cols = c("peak", "BW_3dB"),
               names_to = "var",
               values_to = "val") %>%
  # group_by(var, species) %>%
  # summarize(N_evs = length(unique(eventId)),
  #           N_dets = n())
  nest(data = -c(var, species)) %>% #n = 303 Kogia obs, n = 4,587 Phph obs :P
  mutate(stats = map(.x = data, ~kruskal.test(val~eventId, data = .x) %>% tidy())) %>%
  unnest(stats) %>%
  mutate(p.adj = p.adjust(p.value, method = 'BH')) %>%
  select(-(p.value:method)) %>%
  arrange(var, species, desc(p.adj)) %>%
  print(n = Inf)

null_Ch2 <- spectra %>%
  inner_join(UID_wanted, by = c("UID", "Channel")) %>%
  filter(Channel == 2, species == "Kosp" | species == "Phph") %>% #filter for the species we are comparing
  pivot_longer(cols = noiseLevel:centerkHz_3dB,
               names_to = "var",
               values_to = "val") %>%
  nest(data = -c(var, species)) %>% #n = 303 Kogia obs, n = 4,587 Phph obs :P
  mutate(stats = map(.x = data, ~kruskal.test(val~eventId, data = .x) %>% tidy())) %>%
  unnest(stats) %>%
  mutate(p.adj = p.adjust(p.value, method = 'BH')) %>%
  select(-(p.value:method)) %>%
  arrange(desc(p.adj)) %>%
  print(n = Inf)





