##Function written by Taiki Sakai
getTimes <- function(event) {
  allDets <- getDetectorData(event)
  justTimes <- bind_rows(lapply(allDets, function(x){
    x[,c('UID','UTC')]
  }))
  justTimes
}

#Function written by Taiki Sakai
timeToStartEnd <- function(time, length = 120){
  range <- range(time)
  lenSecs <- as.numeric(difftime(range[2], range[1], units = 'secs'))
  numSplits <- ceiling(lenSecs / length)
  start <- range[1] + length*0:(numSplits-1)
  end <- start + length
  list(start = start, end = end)
}

#All code written by Taiki Sakai, bundled into this function shortenStudyEvents()
shortenStudyEvents <- function(study) {
  newEvents <- vector('list', length = length(events(study)))
  for(i in seq_along(newEvents)){
    thisEvent <- events(study)[[i]]
    thisTime <- getTimes(thisEvent)
    thisStartEnd <- timeToStartEnd(thisTime$UTC, length = 120)
    if(length(thisStartEnd) == 1){
      newEvents[[i]] <- list(thisEvent)
      next
    }
    evList <- vector('list', length = length(thisStartEnd$start))
    for(s in seq_along(thisStartEnd$start)) {
      onePart <- filter(thisEvent, UTC >= thisStartEnd$start[s],
                        UTC < thisStartEnd$end[s])
      if(is.null(onePart)) next
      id(onePart) <- paste0(id(onePart), '_', s)
      evList[[s]] <- onePart
    }
    newEvents[[i]] <- evList
  }
  newEvents <-unlist(newEvents)
  names(newEvents) <- sapply(newEvents, id)
  shortStudy <- study
  events(shortStudy) <- newEvents
  shortStudy
}
