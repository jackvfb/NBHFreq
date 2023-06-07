library(PAMpal)
library(ggplot2)
library(tidyverse)
library(PAMpal)
library(purrr)

# List of file paths to Pg Databases where events have been created and labeled
# with species identification.
#
# In my Pg databases I labeled Kogia events with the code "Kosp" and
# harbor porpoise events "Phph"
#
# All other events I labeled "NBHF"
#
# The databases are named after the drifts and follow the standard SAEL naming
# convention, e.g. "ADRIFT_067" or "CCES_016"
myPaths <- list.files(path = "./analysis/data/raw_data/",
                      pattern = "sqlite3",
                      full.names = TRUE)


#Add GPS csv data to Pg databases before further processing.
#The GPS csv files follow the SAEL naming convention and are are named after
#the drift and have the "_GPS" suffix e.g. "ADRIFT_067_GPS.csv"
csvList <- list.files(path = "./analysis/data/raw_data/",
                      pattern = "_GPS.csv",
                      full.names = TRUE)

mapply(addPgGps, myPaths, csvList,
       source = 'csv',
       format = "%Y-%m-%d %H:%M:%S")

#Path to root binaries folder associated with the Pg databases
myBinaryFolder <- "./analysis/data/raw_data/binaries/"

#create PAMpal settings
myPps <- PAMpalSettings(db = myPaths,
                        binaries = myBinaryFolder,
                        sr_hz = 'auto',
                        filterfrom_khz = 100,
                        filterto_khz = NULL,
                        winLen_sec = .001)

#Create AcousticStudy
myStudy <- processPgDetections(myPps,
                               mode = 'db',
                               id = 'vfb_thesis')

#apply species classification to each Study as designated in PAMguard
myStudy <- setSpecies(myStudy, method = 'pamguard')

#apply GPS data to each Study
myStudy <- addGps(myStudy)


  #filter to return study with only Kogia events
  KoStudy <- filter(myStudy, species == sp)

  #vector with the indices for all events in the study
  numEv <- 1:length(events(KoStudy))

  #now lets take a look at the events individually to see if there
  #are any outliers or noisy events

  #Generate concatenated spectrogram for individual events
  # return data frame with information
  # about the average spectrum for each event
  # to later plot

  x1 <- lapply(numEv, function(x){
    ti <- id(events(KoStudy)[[x]])
    p <- calculateAverageSpectra(KoStudy,
                                 evNum = x,
                                 title = ti,
                                 plot = c(TRUE, FALSE))
    v1 <- p$avgSpec
    v2 <- p$freq
    return(data.frame(avgSpec = v1, freq = v2, evName = ti))
  })

  #merge information in the list returned
  #to generate data frame for plotting purposes
  x2 <- reduce(x1, rbind)

  #plot average spectra for all events
  ggplot(x2, aes(freq, avgSpec, color = evName)) +
    geom_line() +
    ggtitle(str_c(sp, " Events"))

  #end by aggregating all events to generate
  #concat spectrogram and average spectrum for all data
  avSpec <- calculateAverageSpectra(KoStudy,
                                    evNum = numEv,
                                    title = str_c("All ", sp, " Events"))


  clickData <- getClickData(KoStudy)
  # ggplot(KoClickD, aes(x = 1, y = peak))+
  #   geom_boxplot()


  ggplot(clickData, aes(x=eventId, y=mean(peak)))+
    geom_col()
  #summarize click data by event, choosing key parameters
  evSum <- summarise(clickData,
                     peakAvg = mean(peak),
                     BW_3dBAvg = mean(BW_3dB),
                     Q_3dBAvg = mean(Q_3dB),
                     fmin_3dBAvg = mean(fmin_3dB),
                     fmax_3dBAvg = mean(fmax_3dB),
                     .by = eventId)

  # x <- evSum$peakAvg
  # qqnorm(x)
  # qqline(x)
  # ggplot(data.frame(peakAvg = x), aes(peakAvg))+
  #   geom_density()
  # t.test(x, mu = 120.29)
  # ggplot(evSum, )
  # evSum <- clickData %>%
  #   group_by(eventId) %>%
  #   summarize(mean(peak),
  #             mean(BW_3dB),
  #             )

  #pivot from "wide" data to "long" data
  evSuml <- pivot_longer(evSum,
                         !eventId,
                         names_to = "param",
                         values_to = "freq")

  evSum %>%
    filter(param == c("peakAvg", "fmin_3dBAvg", "fmax_3dBAvg")) %>%
    ggplot(aes(x=param, y=freq))+
    geom_boxplot()+
    scale_y_continuous(limits = c(110, 140))

  t2 <-rbind(x3$BW_3dBAvg, x3$Q_3dBAvg)

  ggplot(t2, aes(x=param, y=freq))+
    geom_boxplot()
  # ggplot(x3, aes(x=1, y=peakAvg))+
  #   geom_boxplot()
  #
  # ggplot(KoEvParam, aes(x=1, y=peakAvg)) +
  #   geom_boxplot()
  #
  # ggplot(evParam, aes(1, fmin_3dBAvg)) +
  #   geom_boxplot()
  #
  #   p2 <- ggplot(clickSum, aes(x=0, y=BW_3dBAvg, group = 1)) +
  #   geom_boxplot()
  #
  # p3 <- ggplot(clickSum, aes(x=0, y=Q_3dBAvg)) +
  #   geom_boxplot()
  #
  # p4 <- ggplot(clickSum, aes(x=0, fmin_3dBAvg)) +
  #   geom_boxplot()
  #
  # p5 <- ggplot(clickSum, aes(x=0, y=fmax_3dBAvg)) +
  #   geom_boxplot()
  #
  # p12 <- rbind(p1, p2)
  #
  # p <- ggplot(p12, aes(x = 1, y = )
  # Hp <- data.frame(freq = avSpec_Hp$freq, avgSpec = avSpec_Hp$avgSpec, id = "Hp")
  # Ko <- data.frame(freq = avSpec_Ko$freq, avgSpec = avSpec_Ko$avgSpec, id = "Ko")
  #
  # spec2 <- rbind(Hp, Ko)
  #
  # ggplot(spec2, aes(x = freq, y = avgSpec, colour = id)) +
  #   geom_line()
  #
  # #now want to look at mean event parameters across all CCES, to compare against
  # #prev research
  # #df1 <- getClickData(KoStudy$CCES_016.OE5)
  # df2 <- getClickData(KoStudy)
  # df4 <- summarise(df2, peak=mean(peak), .by = c(eventId, Channel))
  #
  # ggplot(df4, aes(x=Channel, y=peak))+
  #   geom_boxplot()

