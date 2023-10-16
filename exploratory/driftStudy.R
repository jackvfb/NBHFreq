library(PAMpal)
library(tidyverse)

myPps <- PAMpalSettings()

driftStudy <- processPgDetections(myPps,
                                  mode = 'db',
                                  id = 'drift_study')

driftData <- getClickData(driftStudy)

unique(driftData$eventId)
unique(basename(driftData$db))
unique(driftData$eventLabel)

driftData <- mutate(driftData, aPriori = as.factor(eventLabel))


ggplot(data = driftData) +
  stat_summary(
    mapping = aes(x = aPriori, y = peak, .by = eventId),
    fun.min = min,
    fun.max = max,
    fun = mean
  )

ggplot(data = driftData)+
  geom_density(aes(peak)) +
  facet_wrap(~ aPriori)


ggplot(data = driftData)+
  geom_density(aes(peak, color = eventId)) +
  facet_wrap(~ aPriori) +
  theme(legend.position = "none")


ggplot(data = driftData) +
  stat_summary(
    mapping = aes(x = aPriori, y = fmin_3dB),
    fun.min = min,
    fun.max = max,
    fun = mean,
  )

ggplot(data = driftData)+
  geom_density(aes(BW_3dB)) +
  facet_wrap(~ aPriori)

ggplot(data = driftData) +
  stat_summary(
    mapping = aes(x = aPriori, y = fmax_3dB),
    fun.min = min,
    fun.max = max,
    fun = mean
  )

ggplot(data = driftData, aes(fmax_3dB, fmin_3dB))+
  geom_point()

ggplot(data = driftData, aes(fmin_3dB, BW_3dB))+
  geom_point()
