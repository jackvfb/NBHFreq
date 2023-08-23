library(PAMpal)
library(banter)
library(tidyverse)


# Create a two staged RF classification model using package "banter"
#
# First stage of model will employ three different RF models that classify clicks.
# The first stage models will be trained to classify 1-, 2-, and 3- peaked clicks
# separately from one another.
#
# At the second stage, the event classifier will use output from the detector
# models, plus any provided event-level data, which at present their is none.


# PREP TRAINING SET --------------------------------------------------------------

source("R/getMyEvents.R")

#Training on Channel == 1, filter out all unclassified events
training_events <- spectra %>%
  filter(species != "Unk", Channel == 1) %>%
  select(eventId, species) %>%
  distinct(eventId, species) %>%
  rename(event.id = eventId, call.id = UID)

#Make subsample of one-peaked detections to train model.
onePk <- spectra %>%
  #apply filters
  filter(species != "Unk", Channel == 1, numPk == 1) %>%
  #select only spectral parameters to be used for classification, in addition to event Id
  select(UID, eventId, duration:centerkHz_3dB) %>%
  rename(event.id = eventId, call.id = UID)

twoPk <- spectra %>%
  #apply filters
  filter(species != "Unk", Channel == 1, numPk == 2) %>%
  #select only spectral parameters to be used for classification, in addition to event Id
  select(UID, eventId, duration:centerkHz_3dB) %>%
  rename(event.id = eventId, call.id = UID)

threePk <- spectra %>%
  #apply filters
  filter(species != "Unk", Channel == 1, numPk == 3) %>%
  #select only spectral parameters to be used for classification, in addition to event Id
  select(UID, eventId, duration:centerkHz_3dB) %>%
  rename(event.id = eventId, call.id = UID)

detectors = list(onePk = onePk, twoPeak = twoPk, threePk = threePk)


# MAKE MODEL --------------------------------------------------------------

bantMdl <- initBanterModel(training_events)
bantMdl <- addBanterDetector(bantMdl,
                             data = detectors,
                             ntree = 1000,
                             importance = TRUE)
bantMdl <- runBanterModel(bantMdl,
                          ntree = 1000)
saveRDS(bantMdl, "analysis/data/derived_data/second_training/bant2_num.Rds")


#######################################################
#
# IGNORE EVERYTHING BELOW THIS, ITS THE FIRST GO AROUND
#
########################################################

# # LOAD STUDIES ----------------------------------------------------------------
#
# myStudy1 <- readRDS('analysis/data/derived_data/first_training/myStudy1.rds')
# myStudy2 <- readRDS('analysis/data/derived_data/second_training/myStudy2.rds')
#
#
# # GET DETECTIONS ----------------------------------------------------------
#
# bandat1 <- getClickData(myStudy1)
# bandat2 <- getClickData(myStudy2)
#
# source('R/mutateDets.R')          # Helper functions that transform data
# bandat1 <- mutateDets(bandat1)
# bandat2 <- mutateDets(bandat2)
#
# # HELPER FUNCS ------------------------------------------------------------
#
# bantEvDf <- function(bandat){
#   evdf <- bandat %>%
#     filter(species != 'NBHF') %>%
#     select(eventId, species) %>%
#     unique() %>%
#     rename(event.id = eventId)
#   evdf
# }
#
# bantNumPkDf <- function(bandat){
#   pk1.df <- bandat %>%
#     filter(species != 'NBHF', numPk == 1) %>%
#     select(UID, duration:dBPP, Q_3dB:centerkHz_3dB, eventId) %>%
#     rename(call.id = UID, event.id = eventId) %>%
#     drop_na()
#
#   pk2.df <- bandat %>%
#     filter(species != 'NBHF', numPk == 2) %>%
#     select(UID, duration:dBPP, Q_3dB:centerkHz_3dB, eventId) %>%
#     rename(call.id = UID, event.id = eventId) %>%
#     drop_na()
#
#   pk3.df <- bandat %>%
#     filter(species != 'NBHF', numPk == 3) %>%
#     select(UID, duration:dBPP, Q_3dB:centerkHz_3dB, eventId) %>%
#     rename(call.id = UID, event.id = eventId) %>%
#     drop_na()
#
#   detectors <- list(pk1 = pk1.df, pk2 = pk2.df, pk3 = pk3.df)
#   detectors
# }
#
# bantAllPkDf <- function(bandat){
#   allPk <- bandat %>%
#     filter(species != 'NBHF') %>%
#     select(UID, duration:dBPP, Q_3dB:centerkHz_3dB, eventId) %>%
#     rename(call.id = UID, event.id = eventId) %>%
#     drop_na()
#
#   detectors <- list(allPk = allPk)
#   detectors
# }
#
# # MAKE ALL PEAK MODELS --------------------------------------------------------------
#
# bant1_all <- initBanterModel(bantEvDf(bandat1$Ch2))
# bant2_all <- initBanterModel(bantEvDf(bandat2$Ch2))
#
# bant1_all <- addBanterDetector(bant1_all,
#                               data = bantAllPkDf(bandat1$Ch2),
#                               ntree = 1000,
#                               importance = TRUE)
# bant2_all <- addBanterDetector(bant2_all,
#                                data = bantAllPkDf(bandat2$Ch2),
#                                ntree = 1000,
#                                importance = TRUE)
#
# bant1_all <- runBanterModel(bant1_all,
#                             ntree = 1000)
# bant2_all <- runBanterModel(bant2_all,
#                             ntree = 1000)
#
# saveRDS(bant1_all, "analysis/data/derived_data/first_training/bant1_all.Rds")
# saveRDS(bant2_all, "analysis/data/derived_data/second_training/bant2_all.Rds")
#
# # MAKE NUM PEAK MODEL --------------------------------------------------------------
#
# bant2_num <- initBanterModel(bantEvDf(bandat2$Ch2))
#
# bant2_num <- addBanterDetector(bant2_num,
#                                data = bantNumPkDf(bandat2$Ch2),
#                                ntree = 1000,
#                                importance = TRUE)
#
# bant2_num <- runBanterModel(bant2_num,
#                             ntree = 1000)
#
# saveRDS(bant2_num, "analysis/data/derived_data/second_training/bant2_num.Rds")


#######################################################
#
# IGNORE EVERYTHING BELOW THIS, ITS JUST SCRATCH WORK
#
########################################################


# # MAKE EVENT FRAME --------------------------------------------------------
#
# event.df <- bandat$allCh %>%
#   filter(species != 'NBHF') %>%
#   select(eventId, species) %>%
#   unique() %>%
#   rename(event.id = eventId)
#
# # MAKE DETECTOR FRAMES ----------------------------------------------------
#
# pk1.df <- bandat$Ch2 %>%
#   filter(species != 'NBHF', numPk == 1) %>%
#   select(UID, duration:dBPP, Q_3dB:centerkHz_3dB, eventId) %>%
#   rename(call.id = UID, event.id = eventId) %>%
#   drop_na()
#
# pk2.df <- bandat$Ch2 %>%
#   filter(species != 'NBHF', numPk == 2) %>%
#   select(UID, duration:dBPP, Q_3dB:centerkHz_3dB, eventId) %>%
#   rename(call.id = UID, event.id = eventId) %>%
#   drop_na()
#
# pk3.df <- bandat$Ch2 %>%
#   filter(species != 'NBHF', numPk == 3) %>%
#   select(UID, duration:dBPP, Q_3dB:centerkHz_3dB, eventId) %>%
#   rename(call.id = UID, event.id = eventId) %>%
#   drop_na()
#
# detectors <- list(pk1 = pk1.df, pk2 = pk2.df, pk3 = pk3.df)
#
# # ALT: ONE DETECTOR --------------------------------------------------------
#
# allPk <- bandat$Ch2 %>%
#   filter(species != 'NBHF') %>%
#   select(UID, duration:dBPP, Q_3dB:centerkHz_3dB, eventId) %>%
#   rename(call.id = UID, event.id = eventId) %>%
#   drop_na()
#
# detectors2 <- list(allPk = allPk)
#
# # MAKE BANTER MODEL -------------------------------------------------------
#
# bantMdl <- initBanterModel(event.df)
# bantMdl <- addBanterDetector(bantMdl,
#                              data = detectors,
#                              ntree = 1000,
#                              importance = TRUE)
# bantMdl <- runBanterModel(bantMdl,
#                           ntree = 1000)
#
# saveRDS(bantMdl, "analysis/data/derived_data/first_training/bant_numPk_first.rds")
# # ALT:BANT MODEL W/ ONE DETECTOR ------------------------------------------
#
# bantMdlAll <- initBanterModel(event.df)
# bantMdlAll <- addBanterDetector(bantMdlAll,
#                              data = detectors2,
#                              ntree = 1000,
#                              importance = TRUE)
# bantMdlAll <- runBanterModel(bantMdlAll,
#                           ntree = 1000)
#
# saveRDS(bantMdlAll, "analysis/data/derived_data/first_training/bant_allPk_first.rds")
