# getStudySpectra <- function(acousticStudy){
#   allEv <- 1:length(events(acousticStudy))
#   evDFList <- lapply(allEv, function(x){
#     lab <- id(acousticStudy[[x]])
#     list <- calculateAverageSpectra(acousticStudy,
#                                     evNum = x,
#                                     title = lab,
#                                     plot = c(TRUE, FALSE))
#     return(data.frame(list[-c(2,4,6)],
#                       evName = lab,
#                       id = id(acousticStudy)))
#   })
# }

# getEventSpectrum <- function(index, acousticStudy){
#    <- id((study)[[index]])
#   p <- calculateAverageSpectra(study,
#                                evNum = x,
#                                title = ti,
#                                plot = c(TRUE, FALSE)
#   )
#   return(data.frame(p[-c(2,4,6)], evName = ti, id = id(study)))
# }

# getEventSpectrumData <- function(event, study) {
#     df <- calculateAverageSpectra(study,
#                                   evNum = event,
#                                   plot = FALSE)
#     df <- data.frame(df[-c(2,4,6)],
#                      evName = id(events(study)))
#     return(df)
#   }
#

# getStudySpectraData <- function(study) {
#   allEvents <- 1:length(events(study))
#   eventSpectraList <- lapply(allEvents, getEventSpectrumData(study = study))
#   eventSpectraDF <- reduce(eventSpectraList, rbind)
#   eventSpectraDF <- data.frame(eventSpectraDF, study = id(study))
#   return(eventSpectraDF)
# }

##Helper function that will be used to return data frame with average spectrum data for plotting.
getAvSpec <- function (filtStudy) {
  listData <- calculateAverageSpectra(filtStudy,
                                      evNum = 1: length(events(filtStudy)),
                                      #omit the built in plotting function because we want to view multiple spectra on the same plot
                                      plot = FALSE)
  df <- data.frame(listData[c("freq","avgNoise","avgSpec")],
                   species = id(filtStudy))
  df
}

#helper function to use calculateAverageSpectra() on individual events, merging the data from each event into a data frame for plotting purposes.
getStudySpectra <- function(acousticStudy){
  allEv <- 1:length(events(acousticStudy))
  evDFList <- lapply(allEv, function(x){
    lab <- id(acousticStudy[[x]])
    list <- calculateAverageSpectra(acousticStudy,
                                    evNum = x,
                                    title = lab,
                                    plot = c(TRUE, FALSE))
    return(data.frame(list[-c(2,4,6)],
                      evName = lab,
                      #id = id(acousticStudy)
                      species = species(acousticStudy[[x]])))
  })
}
