library(tidyverse)
library(PAMpal)
library(FSA)
library(broom)
library(vegan)

#1. Wrangle data


# 2. Visualize spectral diversity different noise levels --------------------------------
# Can't simply calculate distance matrix, because there are too many detections
#computationally intractable. Must find a way to reduce number of clicks perhaps
#by filtering on noiseLevel.

#Create subsets of data to investigate how sample parameters are affected when
#a noiseLevel filter is applied
eventNoise <- spectra %>%
  nest(data = c(-eventId, -Channel)) %>%
  mutate(qNoise = map(data,
                      function(data) tidy(quantile(data$noiseLevel,
                                                      probs = seq(0.25, 1, 0.25),
                                                      names = TRUE)))) %>%
  unnest(qNoise) %>%
  rename(percentile = names, threshNoise = x) %>%
  unnest(data) %>%
  filter(noiseLevel < threshNoise)

#Summarize count in each noise category
eventNoise %>%
  group_by(eventId, Channel, percentile) %>%
  summarize(meanPeak = mean(peak),
            n = n()) %>%
  arrange(eventId, Channel) %>%
  head(30)

#Clearly shows that spectral diversity has no relationship with noiseLevel.
eventNoise %>%
  ggplot(aes(x = peak, color = percentile))+
  geom_freqpoly()+
  facet_wrap(~Channel)

#this shows that single peaked clicks have lesser noise level
eventNoise %>%
  mutate(numPk = case_when(peak3 != 0 ~ 3,
                           peak2 != 0 ~ 2,
                           .default = 1)) %>%
  mutate(numPk = factor(numPk)) %>%
  ggplot(aes(x = noiseLevel, color = numPk))+
  geom_density()+
  facet_wrap(~Channel)

#Retry with aesthetic for numPk
eventNoise %>%
  mutate(numPk = case_when(peak3 != 0 ~ 3,
                           peak2 != 0 ~ 2,
                           .default = 1)) %>%
  mutate(numPk = factor(numPk)) %>%
  ggplot(aes(x = peak, color = percentile))+
  geom_freqpoly()+
  facet_grid(numPk~Channel)

#Retry with just numPk no Noise parameter
spectra %>%
  mutate(numPk = case_when(peak3 != 0 ~ 3,
                           peak2 != 0 ~ 2,
                           .default = 1)) %>%
  mutate(numPk = factor(numPk)) %>%
  ggplot(aes(x = peak, color = numPk))+
  geom_freqpoly()+
  facet_grid(~Channel)

#Hold the phone, there is a clear discrepancy in Channel 1 distribution of peak
#Is there a specific species that is causing this?
#Looks like we can trace it to Unknown events, on Channel 1 and to a lesser extent Ch2
spectra %>%
  mutate(numPk = case_when(peak3 != 0 ~ 3,
                           peak2 != 0 ~ 2,
                           .default = 1)) %>%
  mutate(numPk = factor(numPk)) %>%
  ggplot(aes(x = peak, color = numPk))+
  geom_freqpoly()+
  facet_grid(species~Channel)


spectra %>%
  filter(species == "Unk") %>%
  mutate(numPk = case_when(peak3 != 0 ~ 3,
                           peak2 != 0 ~ 2,
                           .default = 1)) %>%
  mutate(numPk = factor(numPk)) %>%
  ggplot(aes(x = peak, color = numPk))+
  geom_freqpoly()+
  facet_grid(eventId~Channel)

spectra %>%
  filter(species == "Kosp") %>%
  mutate(numPk = case_when(peak3 != 0 ~ 3,
                           peak2 != 0 ~ 2,
                           .default = 1)) %>%
  mutate(numPk = factor(numPk)) %>%
  ggplot(aes(x = peak, color = numPk))+
  geom_freqpoly()+
  facet_grid(eventId~Channel)
