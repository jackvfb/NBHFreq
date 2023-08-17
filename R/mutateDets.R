mutateDets <- function(dets) {
  #remove rows where noiseLevel is NA, which is necessary for plots
  #dets <- drop_na(dets, noiseLevel)

  #Convert angle parameter from radians to degrees: angDeg
  dets <- mutate(dets, angDeg = angle*(180/pi))

  #Convert the eventId parameter into factor with levels for size of event
  ev.order <- dets %>% count(eventId)
  ev.order <- ev.order %>% arrange(desc(n))
  dets$eventId <- factor(dets$eventId, levels = ev.order$eventId)

  #Add numPk parameter for number of peaks
  dets <- mutate(dets, numPk = case_when(peak3 != 0 ~ 3,
                                         peak2 != 0 ~ 2,
                                         .default = 1))
  dets$numPk <- factor(dets$numPk)

  #Split by channel to avoid pseudo replication
  Ch1 <- dets %>%
    filter(Channel == 1)
  Ch2 <- dets %>%
    filter(Channel == 2)
  list(Ch1 = Ch1, Ch2 = Ch2, allCh = dets)
}
