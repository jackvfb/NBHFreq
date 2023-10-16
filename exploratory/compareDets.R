library(PAMpal)
library(tidyverse)

myStudy <- readRDS("analysis/data/raw_data/second_training/myStudy.rds")
dets <- getClickData(myStudy)

source('R/mutateDets.R')
dets <- mutateDets(dets)

# VIZ ---------------------------------------------------------------------

# investigate differences in peak freq
#plotting all dets individually
# ggplot(dets$allCh, aes(x = peak, color = numPk))+
#   geom_density()+
#   facet_wrap(~species)
#
# #plotting event params instead
# sum <- group_by(best_dets, Channel, eventId)
# sum <- summarise(sum,
#                  n = n(),
#                  evPk = median(peak),
#                  evMin = mean(fmin_3dB),
#                  evMax = mean(fmax_3dB),
#                  evLat = mean(Latitude))
# p <- ggplot(data = sum) + facet_wrap(~Channel)
# p + geom_bin2d(aes(x = evLat, y = evPk))
# p + geom_point(aes(x = evLat, y = evPk, size = n))

# #trying to get other parameters to assess shape of spectrum for each det
# dets <- mutate(dets, shoulderL = peak-fmin_3dB/BW_3dB)
# dets <- mutate(dets, shoulderR = fmax_3dB-peak/BW_3dB)
# dets <- mutate(dets, sharp = shoulderL + shoulderR / 2)


#without first performing any filtering we see that a substantial proportion of
#detections are three-peaked across all species.
ggplot(dets)+
  geom_bar(aes(species, fill = numPk), position = "fill")


# DISTRIBUTIONS OF NOISELEVEL AT DIFFERENT NUMPK --------------------------

#Shows that noise level is correlated with number of peaks
#Looks like 3-peaked clicks are more likely to be noisy
#might be an indication that 3-peaked clicks are less important predictors for
#classification purposes
ggplot(dets$allCh, aes(noiseLevel, color = numPk))+
  geom_density()+
  xlim(-60, 0)


# VISUALIZING CLICK TRAINS ------------------------------------------------


#Demonstration of how noise level can be used to hone in on click trains,
#also shows that most on-train clicks are 1- or 2-peaked
ggplot(filter(dets$Ch1, eventId == 'ADRIFT_024.OE4'), aes(UTC, angDeg))+
  geom_point()+
  ylim(0, 180)+
  facet_wrap(~numPk)+
  labs(title = "Click Train faceted by numPk")

ggplot(filter(dets$Ch1, eventId == 'ADRIFT_024.OE4', noiseLevel <= -25), aes(UTC, angDeg))+
  geom_point()+
  ylim(0, 180)+
  facet_wrap(~Channel)

ggplot(filter(dets, eventId == 'ADRIFT_015.OE8'), aes(UTC, angDeg, color = numPk))+
  geom_point()+
  ylim(0, 180)+
  facet_wrap(~Channel)

ggplot(filter(dets, eventId == 'ADRIFT_015.OE8', noiseLevel <= -20), aes(UTC, angDeg, color = numPk))+
  geom_point()+
  ylim(0, 180)+
  facet_wrap(~Channel)


# #check if dBPP of dets is correlated with n of event
# ggplot(data = dets, mapping = aes(x = eventId, y = dBPP))+
#   geom_boxplot()+
#   theme(axis.text.x = element_text(angle = 90))
#
# #check noiseLevel of dets correlated with n of event
# ggplot(data = dets, mapping = aes(x = eventId, y = noiseLevel))+
#   geom_boxplot()+
#   ylim(c(0,-80))+
#   theme(axis.text.x = element_text(angle = 90))

# #checking out if SNR influences peak reading
# ggplot(dets, aes(noiseLevel, peak))+
#   geom_bin_2d()+
#   xlim(c(-60, 0))+
#   facet_wrap(~species)
#
# #checking out if BW influences peak reading
# ggplot(dets, aes(BW_3dB, peak))+
#   geom_bin_2d()+
#   facet_wrap(~species)

#visualize distribution of peaks in individual events, arranged by n of the event
#this does not distinguish between differently-peaked clicks
# but simply as the aggregate of all clicks regardless of numPk
ggplot(filter(clicks, species == 'Kosp'), aes(eventId, peak))+
  geom_bin_2d()+
  ylim(c(100, 160))+
  theme(axis.text.x = element_text(angle = 90))


#as you filter out noiser clicks, bins with lower counts typically disappear first,
#and bins with higher counts are preserved
ggplot(filter(clicks, species == 'Kosp', noiseLevel <= -15), aes(eventId, peak))+
  geom_bin_2d()+
  ylim(c(100, 160))+
  theme(axis.text.x = element_text(angle = 90))

#events with higher n have more defined patterns in peak freqs. banding suggests
#the possibility of distinct subtypes a la PCR gel plate
#also indicates importance of not collapsing dets into events fothat must be
#visualized when comparing events

ggplot(filter(clicks, species == 'Unk'), aes(eventId, peak))+
  geom_bin_2d()+
  ylim(c(100, 180))+
  theme(axis.text.x = element_text(angle = 90))

#as a follow on, you can see the affect of filtering out noise.
#as you filter out noiser clicks, bins with lower counts typically disappear first,
#and bins with higher counts are preserved. These tend to be the core bins
ggplot(filter(clicks, species == 'Unk', noiseLevel <= -15), aes(eventId, peak))+
  geom_bin_2d()+
  ylim(c(100, 180))+
  theme(axis.text.x = element_text(angle = 90))

ggplot(filter(clicks, species == 'Kosp', noiseLevel <= -15))+
  geom_bin2d(aes(eventId, peak))+
  facet_wrap(~numPk)+
  theme(axis.text.x = element_text(angle = 90))


#looking for patterns in peak-n of clicks and peak
ggplot(filter(dets, noiseLevel <= -15), aes(peak, color = species))+
  geom_density()+
  ylim(0, 0.20)+
  facet_wrap(~numPk)

ggplot(filter(dets, noiseLevel <= -15), aes(peak, color = numPk))+
  geom_density()+
  ylim(0, 0.20)+
  facet_wrap(~species)

#now make the same thing but seperate by event, can change species to see how indiv
# events differ from one another
ggplot(filter(dets, species == "NBHF", noiseLevel <= -10), aes(peak, color = numPk))+
  geom_freqpoly()+
  facet_wrap(~eventId)

#closer look at two-peaked dets shows that there may be meaningful differences
#based on species, at least some interesting patterns develop when the data
#is viewed in this way

#is there a way to visualize relationship between peak and peak2 for two-peaked dets?
ggplot(filter(dets, numPk == 2))+
  geom_freqpoly(aes(peak), color = 'blue')+
  geom_freqpoly(aes(trough), color = 'purple')+
  geom_freqpoly(aes(peak2), color = 'red')+
  facet_wrap(~species)

#is there a way to visualize relationship between peak and peak2 for two-peaked dets?
#now looking at individual events

ggplot(filter(dets, species == 'Kosp', numPk == 2))+
  geom_freqpoly(aes(peak), color = 'blue')+
  geom_freqpoly(aes(trough), color = 'purple')+
  geom_freqpoly(aes(peak2), color = 'red')

ggplot(filter(dets, species == 'NBHF', numPk == 2))+
  geom_freqpoly(aes(peak), color = 'blue')+
  geom_freqpoly(aes(trough), color = 'purple')+
  geom_freqpoly(aes(peak2), color = 'red')+
  facet_wrap(~eventId)

ggplot(filter(dets, species == "NBHF", numPk == 2))+
  geom_point(aes(peak, peakToPeak2))+
  facet_wrap(~eventId)

ggplot(filter(dets, species == "Kosp", numPk == 2))+
  geom_point(aes(peak, peakToPeak2))


#now a closer look at one-peaked dets, is there variation in sharpness?
#looks to be pretty consistent at the species level.
#but there is variation among species
ggplot(filter(dets, numPk == 1, noiseLevel <= -30))+
  stat_bin2d(aes(peak, BW_3dB))+
  facet_wrap(~species)

# most essential:

ggplot(filter(clicks, species == "Kosp", numPk == 1, noiseLevel <= -30))+
  geom_point(aes(BW_3dB, centerkHz_3dB))

ggplot(filter(clicks, species == "Unk", numPk == 1, noiseLevel <= -30))+
  geom_point(aes(BW_3dB, centerkHz_3dB))

# ggplot(filter(dets, species == 'Phph'), aes(eventId, peak))+
#   geom_bin_2d()+
#   theme(axis.text.x = element_text(angle = 90))
#
# ggplot(filter(dets, species == 'Phda'), aes(eventId, peak))+
#   geom_bin_2d()+
#   theme(axis.text.x = element_text(angle = 90))

#
ggplot(filter(dets, Channel == 2, species == "Phph"))+
  geom_qq(aes(sample = peak))+
  geom_qq_line(aes(sample = peak))+
  facet_wrap(~numPk)

ggplot(filter(dets, Channel == 2, species == "Kosp"))+
  geom_qq(aes(sample = peak))+
  geom_qq_line(aes(sample = peak))+
  facet_wrap(~numPk)


# DISTR OF PEAK BY NUMPK --------------------------------------------------

dets <- getClickData(myStudy2)
source("R/mutateDets.R")
dets <- mutateDets(dets)

ggplot()+
  geom_point(dets$Ch1, mapping = aes(eventId, peak, color = numPk), position = "dodge")
