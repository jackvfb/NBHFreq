library(PAMpal)
library(rfPermute)
library(banter)
library(tidyverse)

# data(train.data)
# bant.mdl <- initBanterModel(train.data$events)
# summary(bant.mdl)
# bant.mdl <- addBanterDetector(
#   bant.mdl,
#   data = train.data$detectors,
#   ntree = 100,
#   importance = TRUE,
#   sampsize = 2
# )
# summary(bant.mdl)
# plotDetectorTrace(bant.mdl)
# bant.mdl <- runBanterModel(bant.mdl,
#                            ntree = 100000,
#                            sampsize = 3)
# summary(bant.mdl)
# bant.rf <- getBanterModel(bant.mdl)
# bantData.df <- getBanterModelData(bant.mdl)
#

### Creating BANTER Model

bantStudy <- filter(shortStudy, species != "NBHF", channel == 2)
bantData <- export_banter(bantStudy,
                          #must drop these vars because no GPS data for Bangarang
                          dropVars = c('Latitude', 'Longitude', 'gpsUncertainty'))
bant.mdl <- initBanterModel(bantData$events)
bant.mdl <- addBanterDetector(bant.mdl,
                              data = bantData$detectors,
                              ntree = 10000,
                              importance = TRUE)
bant.mdl <- runBanterModel(bant.mdl, ntree = 10000, sampsize = 2)
bant.rf <- getBanterModel(bant.mdl)
detector.rf <- getBanterModel(bant.mdl, "Click_Detector_5")
summary(bant.mdl)
summary(detector.rf)
novData <- export_banter(myStudy,
                         dropSpecies = c("Phph","Kosp", "Phda"))
t1 <- predict(bant.mdl,novData)
t1$predict.df
# event.id predicted   Kosp   Phda   Phph original correct
# 1  ADRIFT_015.OE3      Phph 0.3019 0.0034 0.6947     NBHF   FALSE
# 2  ADRIFT_015.OE4      Phph 0.4161 0.1538 0.4301     NBHF   FALSE
# 3  ADRIFT_015.OE5      Kosp 0.6698 0.0082 0.3220     NBHF   FALSE
# 4  ADRIFT_015.OE6      Kosp 0.7126 0.0089 0.2785     NBHF   FALSE
# 5  ADRIFT_015.OE7      Kosp 0.7476 0.0083 0.2441     NBHF   FALSE
# 6  ADRIFT_015.OE1      Kosp 0.4772 0.1688 0.3540     NBHF   FALSE
# 7  ADRIFT_015.OE2      Kosp 0.7517 0.0083 0.2400     NBHF   FALSE
# 8  ADRIFT_015.OE8      Phph 0.2935 0.0000 0.7065     NBHF   FALSE
# 9  ADRIFT_017.OE1      Phph 0.2430 0.0000 0.7570     NBHF   FALSE
# 10 ADRIFT_017.OE2      Phph 0.1532 0.2616 0.5852     NBHF   FALSE
# 11 ADRIFT_024.OE1      Phph 0.2134 0.0019 0.7847     NBHF   FALSE
# 12 ADRIFT_024.OE3      Kosp 0.6601 0.0076 0.3323     NBHF   FALSE
# 13 ADRIFT_024.OE4      Kosp 0.5809 0.0013 0.4178     NBHF   FALSE
# 14 ADRIFT_025.OE1      Kosp 0.7560 0.0137 0.2303     NBHF   FALSE
# 15 ADRIFT_025.OE2      Phph 0.2672 0.2000 0.5328     NBHF   FALSE
# 16 ADRIFT_025.OE3      Kosp 0.6157 0.2295 0.1548     NBHF   FALSE
#Suspicious because no Phda predictions for any ADRIFT events!
plotTrace(bant.rf)
plotTrace(detector.rf)
plotImportance(bant.rf)
plotImportance(detector.rf)
plotPredictedProbs(detector.rf)
plotProximity(bant.rf)
