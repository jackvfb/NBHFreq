library(PAMpal)
library(banter)
library(rfPermute)
library(tidyverse)
library(densityClust)

clickData <- getClickData(myStudy)
drift <- filter(clickData, species == "Kosp" | species == "NBHF", na.rm = TRUE)
drift <- drift[complete.cases(drift),]
drift$eventId <- as.factor(drift$eventId)
driftDist <- drift[,c("centerkHz_3dB", "fmin_3dB", "fmax_3dB")]
plot(driftDist)
driftDist <- dist(driftDist)
driftClust <- densityClust(driftDist)
plot(driftClust)

driftClust <- findClusters(driftClust)

data("iris")
