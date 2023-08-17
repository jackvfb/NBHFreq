split_noiseLevel <- function(dets, probs){
    noise <- dets %>%
    group_by(eventId) %>%
    filter(noiseLevel >= quantile(noiseLevel, probs = probs, na.rm = TRUE)) %>%
    summarize(n = n(), val = mean(peak, na.rm = TRUE))
  signal <- dets %>%
    group_by(eventId) %>%
    filter(noiseLevel < quantile(noiseLevel, probs = probs, na.rm = TRUE)) %>%
    summarize(n = n(), val = mean(peak, na.rm = TRUE))
  sn <- full_join(noise, signal, by = join_by("eventId"), suffix = c(".n", ".s"))
  sn <- mutate(sn, diff = val.s - val.n, n = n.n + n.s)
  sn
}

split_noiseLevel_BW <- function(dets, probs){
  noise <- dets %>%
    group_by(eventId) %>%
    filter(noiseLevel >= quantile(noiseLevel, probs = probs, na.rm = TRUE)) %>%
    summarize(n = n(), val = mean(BW_10dB, na.rm = TRUE))
  signal <- dets %>%
    group_by(eventId) %>%
    filter(noiseLevel < quantile(noiseLevel, probs = probs, na.rm = TRUE)) %>%
    summarize(n = n(), val = mean(BW_10dB, na.rm = TRUE))
  sn <- full_join(noise, signal, by = join_by("eventId"), suffix = c(".n", ".s"))
  sn <- mutate(sn, diff = val.s - val.n, n = n.n + n.s)
  sn
}

# split_dBPP <- function(dets, probs){
#   signal <- dets %>%
#     group_by(eventId) %>%
#     filter(dBPP <= quantile(dBPP, probs = probs, na.rm = TRUE))
#   noise <- dets %>%
#     group_by(eventId) %>%
#     filter(dBPP > quantile(dBPP, probs = probs, na.rm = TRUE))
#   list(noise = noise, signal = signal)
# }

code_noiseLevel <- function(dets, probs) {
  dets <- dets %>%
    group_by(eventId) %>%
    mutate(snVal = quantile(noiseLevel,
                            probs = probs,
                            na.rm = TRUE),
           isNoise = case_when(noiseLevel <=  quantile(noiseLevel,
                                                     probs = probs,
                                                     na.rm = TRUE) ~ FALSE,
                             .default = TRUE
                             ))
  dets
}

#FILTER NOISE OUT OF STUDY ----

#function removes detections below a certain noise threshold from the event
replaceDets <- function(event, threshold) {
  noisyDets <- event@detectors$Click_Detector_5
  bestDets <- noisyDets[noisyDets$noiseLevel <= threshold, ]
  if(nrow(bestDets) == 0) {
    NA
  } else {
    event@detectors$Click_Detector_5 <- bestDets
    event
  }
}

# #remove noisy detections from events, replace events in Study
# myList <- events(myStudy)
# newList <- lapply(myList, replaceDets, threshold = -30)
# newStudy <- myStudy
# newStudy@events <- newList[!is.na(newList)]
#
# #check how many clicks were removed
# nClicks(myStudy)
# nClicks(newStudy)
#
# #observe example of difference in concat spectrogram and spectrum
# t<- calculateAverageSpectra(filter(myStudy, species == 'Kosp'))
# t2 <- calculateAverageSpectra(filter(newStudy, species == 'Kosp'))
#
#
# justK <- filter(newStudy, species == 'Phph')
# justK2 <- filter(myStudy, species == 'Phph')
# t <- calculateAverageSpectra(justK, evNum = 1:2)
# t <- calculateAverageSpectra(justK2, evNum = 1:length(events(justK2)))

