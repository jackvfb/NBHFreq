library(PAMpal)
library(banter)
library(tidyverse)


# LOAD STUDIES ----------------------------------------------------------------

myStudy1 <- readRDS('analysis/data/derived_data/first_training/myStudy1.rds')
myStudy2 <- readRDS('analysis/data/derived_data/second_training/myStudy2.rds')


# GET DETECTIONS ----------------------------------------------------------

bandat1 <- getClickData(myStudy1)
bandat2 <- getClickData(myStudy2)

source('R/mutateDets.R')          # Helper functions that transform data
bandat1 <- mutateDets(bandat1)
bandat2 <- mutateDets(bandat2)

# HELPER FUNCS ------------------------------------------------------------

bantEvDf <- function(bandat){
  evdf <- bandat %>%
    filter(species != 'NBHF') %>%
    select(eventId, species) %>%
    unique() %>%
    rename(event.id = eventId)
  evdf
}

bantNumPkDf <- function(bandat){
  pk1.df <- bandat %>%
    filter(species != 'NBHF', numPk == 1) %>%
    select(UID, duration:dBPP, Q_3dB:centerkHz_3dB, eventId) %>%
    rename(call.id = UID, event.id = eventId) %>%
    drop_na()

  pk2.df <- bandat %>%
    filter(species != 'NBHF', numPk == 2) %>%
    select(UID, duration:dBPP, Q_3dB:centerkHz_3dB, eventId) %>%
    rename(call.id = UID, event.id = eventId) %>%
    drop_na()

  pk3.df <- bandat %>%
    filter(species != 'NBHF', numPk == 3) %>%
    select(UID, duration:dBPP, Q_3dB:centerkHz_3dB, eventId) %>%
    rename(call.id = UID, event.id = eventId) %>%
    drop_na()

  detectors <- list(pk1 = pk1.df, pk2 = pk2.df, pk3 = pk3.df)
  detectors
}

bantAllPkDf <- function(bandat){
  allPk <- bandat %>%
    filter(species != 'NBHF') %>%
    select(UID, duration:dBPP, Q_3dB:centerkHz_3dB, eventId) %>%
    rename(call.id = UID, event.id = eventId) %>%
    drop_na()

  detectors <- list(allPk = allPk)
  detectors
}

# MAKE ALL PEAK MODELS --------------------------------------------------------------

bant1_all <- initBanterModel(bantEvDf(bandat1$Ch2))
bant2_all <- initBanterModel(bantEvDf(bandat2$Ch2))

bant1_all <- addBanterDetector(bant1_all,
                              data = bantAllPkDf(bandat1$Ch2),
                              ntree = 1000,
                              importance = TRUE)
bant2_all <- addBanterDetector(bant2_all,
                               data = bantAllPkDf(bandat2$Ch2),
                               ntree = 1000,
                               importance = TRUE)

bant1_all <- runBanterModel(bant1_all,
                            ntree = 1000)
bant2_all <- runBanterModel(bant2_all,
                            ntree = 1000)

saveRDS(bant1_all, "analysis/data/derived_data/first_training/bant1_all.Rds")
saveRDS(bant2_all, "analysis/data/derived_data/second_training/bant2_all.Rds")

# MAKE NUM PEAK MODEL --------------------------------------------------------------

bant2_num <- initBanterModel(bantEvDf(bandat2$Ch2))

bant2_num <- addBanterDetector(bant2_num,
                               data = bantNumPkDf(bandat2$Ch2),
                               ntree = 1000,
                               importance = TRUE)

bant2_num <- runBanterModel(bant2_num,
                            ntree = 1000)

saveRDS(bant2_num, "analysis/data/derived_data/second_training/bant2_num.Rds")


#######################################################
#
# IGNORE EVERYTHING BELOW THIS, ITS JUST SCRATCH WORK
#
########################################################
# MAKE EVENT FRAME --------------------------------------------------------

event.df <- bandat$allCh %>%
  filter(species != 'NBHF') %>%
  select(eventId, species) %>%
  unique() %>%
  rename(event.id = eventId)

# MAKE DETECTOR FRAMES ----------------------------------------------------

pk1.df <- bandat$Ch2 %>%
  filter(species != 'NBHF', numPk == 1) %>%
  select(UID, duration:dBPP, Q_3dB:centerkHz_3dB, eventId) %>%
  rename(call.id = UID, event.id = eventId) %>%
  drop_na()

pk2.df <- bandat$Ch2 %>%
  filter(species != 'NBHF', numPk == 2) %>%
  select(UID, duration:dBPP, Q_3dB:centerkHz_3dB, eventId) %>%
  rename(call.id = UID, event.id = eventId) %>%
  drop_na()

pk3.df <- bandat$Ch2 %>%
  filter(species != 'NBHF', numPk == 3) %>%
  select(UID, duration:dBPP, Q_3dB:centerkHz_3dB, eventId) %>%
  rename(call.id = UID, event.id = eventId) %>%
  drop_na()

detectors <- list(pk1 = pk1.df, pk2 = pk2.df, pk3 = pk3.df)

# ALT: ONE DETECTOR --------------------------------------------------------

allPk <- bandat$Ch2 %>%
  filter(species != 'NBHF') %>%
  select(UID, duration:dBPP, Q_3dB:centerkHz_3dB, eventId) %>%
  rename(call.id = UID, event.id = eventId) %>%
  drop_na()

detectors2 <- list(allPk = allPk)

# MAKE BANTER MODEL -------------------------------------------------------

bantMdl <- initBanterModel(event.df)
bantMdl <- addBanterDetector(bantMdl,
                             data = detectors,
                             ntree = 1000,
                             importance = TRUE)
bantMdl <- runBanterModel(bantMdl,
                          ntree = 1000)

saveRDS(bantMdl, "analysis/data/derived_data/first_training/bant_numPk_first.rds")
# ALT:BANT MODEL W/ ONE DETECTOR ------------------------------------------

bantMdlAll <- initBanterModel(event.df)
bantMdlAll <- addBanterDetector(bantMdlAll,
                             data = detectors2,
                             ntree = 1000,
                             importance = TRUE)
bantMdlAll <- runBanterModel(bantMdlAll,
                          ntree = 1000)

saveRDS(bantMdlAll, "analysis/data/derived_data/first_training/bant_allPk_first.rds")

#MAKE DETECTOR RANDOMFOREST MODEL ----

rfData <- getClickData(filter(myStudy, species != 'NBHF'))
rfData$species<-as.factor(rfData$species)
rfData <- select(rfData, "UID", "duration":"centerkHz_3dB", "species")
rfData <- rfData[complete.cases(rfData),]
my.sampSize <- balancedSampsize(rfData$species)
set.seed(123)
rfMdl <- randomForest(formula = species ~ .,
                      data = rfData,
                      sampsize = my.sampSize,
                      proximity = TRUE,
                      importance = TRUE)
saveRDS(rfMdl, "analysis/data/derived_data/rfMdl.rds")

#EVALUATE MODELS ----
#getSampSize(bantMdl)
#numCalls(bantMdl)
#numEvents(bantMdl)
#plotConfMat(bantRf, title = "Banter Conf Mat Heat Map")
#plotConfMat(detectorRf, title = "Detector Conf Mat Heat Map")

confusionMatrix(bantRf)
confusionMatrix(detectorRf)
confusionMatrix(rfMdl) #nominally the same as detectorRf

plotVotes(bantRf)
plotVotes(detectorRf)
plotVotes(rfMdl)

pctCorrect(bantRf, pct = c(seq(0.2, 0.6, 0.2), 0.95))
pctCorrect(rfMdl, pct = c(seq(0.2, 0.6, 0.2), 0.95))

#seems to be a problem in that distributions do not match red line,
#tried increasing sample size did not help
plotInbag(detectorRf) #bad
plotInbag(bantRf) #bad
plotInbag(rfMdl) #bad

plotTrace(bantRf)
plotTrace(detectorRf)

plotImportance(rfMdl)
plotImportance(bantRf)#

#plotImpPreds(RF.model, DF.model, "species")  #distribution of predictors
plotPredictedProbs(bantRf)

plotProximity(bantRf)
plotProximity(rfMdl)

dev.off()

#Confusion matrix
confusionMatrix(RF.model)

#PREDICT----
novDat <- export_banter(filter(newStudy, species == 'NBHF'))
t1 <- predict(bantMdl, novDat)
# t1$predict.dfstr()

#repeat for detector 1 data frame
d1.df <- bandat %>%
  filter(numPk == 1) %>%
  select(UID, duration:centerkHz_3dB, eventId) %>%
  rename(event.id = eventId, call.id = UID)

#repeat for detector 2 data frame
d2.df <- bandat %>%
  filter(numPk == 2) %>%
  select(UID, duration:centerkHz_3dB, eventId) %>%
  rename(event.id = eventId, call.id = UID)

#repeat for detector 3 data frame

d3.df <- bandat %>%
  filter(numPk == 3) %>%
  select(UID, duration:centerkHz_3dB, eventId) %>%
  rename(event.id = eventId, call.id = UID)

dlist <- list(d1 = d1.df, d2 = d2.df, d3 = d3.df)
bant.mdl <- initBanterModel(event.df)
bant.mdl <- addBanterDetector(bant.mdl,
                             data = dlist,
                             ntree = 200,
                             importance = TRUE)
bant.mdl <- runBanterModel(bant.mdl,
                          ntree = 200)
summary(bant.mdl)
bant.rf <- getBanterModel(bant.mdl)
d1.rf <- getBanterModel(bant.mdl, "d1")
d2.rf <- getBanterModel(bant.mdl, "d2")
d3.rf <- getBanterModel(bant.mdl, "d3")

#EVALUATE
rf.list <- list(d1.rf, d2.rf, d3.rf)
lapply(rf.list, confusionMatrix)
lapply(rf.list, plotVotes)
plotVotes(bant.rf)

lapply(rf.list, plotInbag, replace = FALSE, sampsize = 80)
plotInbag(bant.rf)

plotDetectorTrace(bant.mdl)
plotImportance(bant.rf)
lapply(rf.list, plotImportance)

plotPredictedProbs(bant.rf)
lapply(rf.list, plotPredictedProbs)

plotProximity(bant.rf)


#MAKE BANTER MODEL ----
#myStudy <- shortenStudyEvents(myStudy)
bantData <- export_banter(newStudy,
                          dropSpecies = "NBHF")
bantMdl <- initBanterModel(bantData$events)
bantMdl <- addBanterDetector(bantMdl,
                             data = bantData$detectors,
                             ntree = 10000,
                             importance = TRUE)
bantMdl <- runBanterModel(bantMdl,
                          ntree = 10000)
summary(bantMdl)
# Number of events and model classification rate:
#   species num.events Click_Detector_5 event
# 1    Kosp         30            98.40   100
# 2    Phda          5            96.77   100
# 3    Phph         29            95.26   100
# 4 Overall         64            95.97   100
bantRf <- getBanterModel(bantMdl)
bantDf <- getBanterModelData(bantMdl)
detectorRf <- getBanterModel(bantMdl, "Click_Detector_5")

#saveRDS(bantMdl, "analysis/data/derived_data/bantMdl.rds")

#MAKE DETECTOR RANDOMFOREST MODEL ----

rfData <- getClickData(filter(myStudy, species != 'NBHF'))
rfData$species<-as.factor(rfData$species)
rfData <- select(rfData, "UID", "duration":"centerkHz_3dB", "species")
rfData <- rfData[complete.cases(rfData),]
my.sampSize <- balancedSampsize(rfData$species)
set.seed(123)
rfMdl <- randomForest(formula = species ~ .,
                      data = rfData,
                      sampsize = my.sampSize,
                      proximity = TRUE,
                      importance = TRUE)
saveRDS(rfMdl, "analysis/data/derived_data/rfMdl.rds")

#EVALUATE MODELS ----
#getSampSize(bantMdl)
#numCalls(bantMdl)
#numEvents(bantMdl)
#plotConfMat(bantRf, title = "Banter Conf Mat Heat Map")
#plotConfMat(detectorRf, title = "Detector Conf Mat Heat Map")

confusionMatrix(bantRf)
confusionMatrix(detectorRf)
confusionMatrix(rfMdl) #nominally the same as detectorRf

plotVotes(bantRf)
plotVotes(detectorRf)
plotVotes(rfMdl)

pctCorrect(bantRf, pct = c(seq(0.2, 0.6, 0.2), 0.95))
pctCorrect(rfMdl, pct = c(seq(0.2, 0.6, 0.2), 0.95))

#seems to be a problem in that distributions do not match red line,
#tried increasing sample size did not help
plotInbag(detectorRf) #bad
plotInbag(bantRf) #bad
plotInbag(rfMdl) #bad

plotTrace(bantRf)
plotTrace(detectorRf)

plotImportance(rfMdl)
plotImportance(bantRf)#

#plotImpPreds(RF.model, DF.model, "species")  #distribution of predictors
plotPredictedProbs(bantRf)

plotProximity(bantRf)
plotProximity(rfMdl)

dev.off()

#Confusion matrix
confusionMatrix(RF.model)

#PREDICT----
novDat <- export_banter(filter(newStudy, species == 'NBHF'))
t1 <- predict(bantMdl, novDat)
t1$predict.df
