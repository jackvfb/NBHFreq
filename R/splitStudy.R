library(PAMpal)

myStudy <- readRDS("analysis/data/derived_data/second_training/myStudy2.rds")

#see list of all events and species assignment
species(myStudy)

#create training set using only events ground-truthed or classed a priori
training <- filter(myStudy, species != "NBHF")
saveRDS(training, file = "analysis/data/derived_data/training/trainingStudy.rds")

#Lets get the Click Data now

