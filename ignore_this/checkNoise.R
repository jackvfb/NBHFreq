library(PAMpal)
library(tidyverse)

# myStudy <- readRDS("analysis/data/raw_data/second_training/myStudy2.rds")
# dets <- getClickData(myStudy)
# source("R/mutateDets.R")
# dets <- mutateDets(dets)
#WRANGLE DATA IN THIS BLOCK


# NOISE VIS ---------------------------------------------------------------

# Split dets into noise and signal, based on percentiles.
# Use noise and signal to visualize whether there is a reasonable noise threshold
# that should be chosen to filter with.
# Also visualize whether there is a systematic error that could be causing
# readings to vary systematically based on the db. Does calibration need to be
# applied to the data?

source("R/splitNoise.R")

# dets.c90 <- code_noiseLevel(dets, 0.9)
# dets.c90 <- group_by(dets.c90, eventId, isNoise)
# dets.c90 %>%
#   summarize(mnPk = mean(peak, na.rm = TRUE)) %>%
#   ungroup() %>%
#   rowwise(isNoise)
sn95.1 <- split_noiseLevel(dets$Ch1, 0.95)
sn90 <- split_noiseLevel(dets, 0.90)
sn85 <- split_noiseLevel(dets, 0.85)
ggplot()+
  geom_point(data = sn95, aes(n, abs(diff)), color = 'cyan')+
  geom_point(data = sn90, aes(n, abs(diff)), color = 'purple')+
  geom_point(data = sn85, aes(n, abs(diff)), color = 'red')
#not observing much of a relationship here

sn95 <- split_noiseLevel_BW(dets, 0.95)
sn90 <- split_noiseLevel_BW(dets, 0.90)
sn85 <- split_noiseLevel_BW(dets, 0.85)
ggplot()+
  geom_point(data = sn95, aes(n, abs(diff)), color = 'cyan')+
  geom_point(data = sn90, aes(n, abs(diff)), color = 'purple')+
  geom_point(data = sn85, aes(n, abs(diff)), color = 'red')

#Figures show that noise distribution is not all that different from signal
#distribution, suggesting that it may make more sense to leave all detections
#as-is, rather than trying to filter out based on noise Level
ggplot()+
  geom_density(data = dets.95$noise, aes(peak), linetype = "dashed")+
  geom_density(data = dets.95$signal, aes(peak))+
  facet_wrap(~eventId)+
  labs(title = "probs = 0.95")

ggplot()+
  geom_density(data = dets.90$noise, aes(peak), linetype = "dashed")+
  geom_density(data = dets.90$signal, aes(peak))+
  facet_wrap(~species)+
  labs(title = "probs = 0.90")

ggplot()+
  geom_density(data = dets.75$noise, aes(peak), linetype = "dashed")+
  geom_density(data = dets.75$signal, aes(peak))+
  facet_wrap(~species)+
  labs(title = "probs = 0.75")

ggplot()+
  geom_density(data = dets.60$noise, aes(peak), linetype = "dashed")+
  geom_density(data = dets.60$signal, aes(peak))+
  facet_wrap(~species)+
  labs(title = "probs = 0.60")

# noise %>%
#   filter(Channel == 1) %>%
#   mutate(pkBin = factor(cut(peak, seq(100, 180, 10)))) %>%
#   ggplot(aes(eventId, fill = pkBin))+
#   geom_bar(position = "fill")+
#   facet_wrap(~species)+
#   labs(title = "proportion of Ch 1 noise dets in each event colored by peak bin")
#
# noise %>%
#   filter(Channel == 2) %>%
#   mutate(pkBin = factor(cut(peak, seq(100, 180, 10)))) %>%
#   ggplot(aes(eventId, fill = pkBin))+
#   geom_bar(position = "fill")+
#   facet_wrap(~species)+
#   labs(title = "proportion of Ch 2 noise dets in each event colored by peak bin")
#
# noise %>%
#   mutate(pkBin = factor(cut(peak, seq(100, 180, 10)))) %>%
#   ggplot(aes(eventId, fill = pkBin))+
#   geom_bar()+
#   facet_wrap(~species)+
#   labs(title = "count of noise dets in each event colored by peak bin")


# NUM PEAKS ---------------------------------------------------------

#Clearly shows that multiple-peaked clicks occur at higher noiseLevels then
#single - peaked

ggplot(dets$allCh)+
  geom_density(aes(noiseLevel, color = numPk))+
  facet_wrap(~Channel)

#visualize distributions of n-peaked clicks by species. No clear patterns
dets$Ch1 %>%
  ggplot(aes(eventId, fill = numPk))+
  geom_bar(position = "fill")+
  facet_wrap(~species)+
  theme(axis.text.x = element_text(angle = 90))


