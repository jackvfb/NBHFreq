#adrift would be all ADRIFT data

#interesting, some clusters starting to form
ggplot(adrift, aes(noiseLevel, peak, color = Channel))+
  geom_point()+
  facet_wrap(~ eventId)+
  coord_cartesian(xlim = c(-60, 0))

ggplot(adrift15, aes(x=peak))+
  geom_histogram()+
  facet_wrap(~eventId)

ggplot(filter(adrift, noiseLevel <= -30), aes(angle, peak))+
  geom_point()+
  facet_wrap(~eventId)
##Visualizing Click Train
# Shows that peak freqs are influenced by the channel on which they are recorded,
# better to keep channels seperate
ggplot(filter(ev15.8, noiseLevel <= -20, Channel ==1), aes(UTC, angDeg, color = peak))+
  geom_point()+
  coord_cartesian(ylim = c(0, 180)) +
  scale_color_continuous(limits = c(120,160))

ggplot(filter(ev15.8, noiseLevel <= -20, Channel ==2), aes(UTC, angDeg, color = peak))+
  geom_point()+
  coord_cartesian(ylim = c(0, 180)) +
  scale_color_continuous(limits = c(120,160))


#choose high click events
summarize(adrift, n = n(), .by = eventId)

#choose event
ev24.4 <- adrift[grepl("24.OE4", det$eventId),]

ggplot(filter(ev24.4, noiseLevel <= -20, Channel ==1), aes(UTC, angDeg))+
  geom_point()+
  coord_cartesian(ylim = c(0, 180))

ggplot(filter(ev24.4, noiseLevel <= -20, Channel ==2), aes(UTC, angDeg))+
  geom_point()+
  coord_cartesian(ylim = c(0, 180))

#choose another high click events
ev24.3 <- adrift[grepl("24.OE3", det$eventId),]

ggplot(filter(ev24.3, noiseLevel <= -20, Channel ==1), aes(UTC, angDeg))+
  geom_point()+
  coord_cartesian(ylim = c(0, 180))

ggplot(filter(ev24.3, noiseLevel <= -20, Channel ==2), aes(UTC, angDeg))+
  geom_point()+
  coord_cartesian(ylim = c(0, 180))

##another
ev15.5 <- adrift[grepl("15.OE5", det$eventId),]
ggplot(filter(ev15.5, noiseLevel <= -15, Channel ==1), aes(UTC, angDeg))+
  geom_point()+
  coord_cartesian(ylim = c(0, 180))

ggplot(filter(ev15.5, noiseLevel <= -15, Channel ==2), aes(UTC, angDeg))+
  geom_point()+
  coord_cartesian(ylim = c(0, 180))

#choose another high click events
ev15.7 <- adrift[grepl("15.OE7", det$eventId),]

ggplot(filter(ev15.7, noiseLevel <= -15, Channel ==1), aes(UTC, angDeg))+
  geom_point()+
  coord_cartesian(ylim = c(0, 180))

ggplot(filter(ev15.7, noiseLevel <= -15, Channel ==2), aes(UTC, angDeg))+
  geom_point()+
  coord_cartesian(ylim = c(0, 180))

#add angDeg to df
opps <- mutate(opps, angDeg = angle*(180/pi))
summarize(opps, n = n(), .by = eventId)

opps8 <- opps[grepl("008", opps$eventId),]

ggplot(filter(opps8, noiseLevel <= -30, Channel ==1), aes(UTC, angDeg))+
  geom_point()+
  coord_cartesian(ylim = c(0, 180))

ggplot(filter(opps8, noiseLevel <= -30, Channel ==2), aes(UTC, angDeg))+
  geom_point()+
  coord_cartesian(ylim = c(0, 180))

##now for CCES
cces <- mutate(cces, angDeg = angle*(180/pi))
summarize(cces, n = n(), .by = eventId)
cces16.3 <- cces[grepl("16.OE3", cces$eventId),]

ggplot(filter(cces16.3, Channel ==1), aes(UTC, angDeg))+
  geom_point()+
  coord_cartesian(ylim = c(0, 180))

ggplot(filter(cces16.3, Channel ==2), aes(UTC, angDeg))+
  geom_point()+
  coord_cartesian(ylim = c(0, 180))

cces21.3 <- cces[grepl("21.OE3", cces$eventId),]
ggplot(filter(cces21.3, noiseLevel <= -20, Channel ==1), aes(UTC, angDeg))+
  geom_point()+
  coord_cartesian(ylim = c(0, 180))

ggplot(filter(cces21.3, Channel ==2), aes(UTC, angDeg))+
  geom_point()+
  coord_cartesian(ylim = c(0, 180))
#all of this to show that NoiseLevel is a good way to filter out noise! also that
# Channel ==1 or Channel ==2 changes the peak!

#though this should maybe be done on an event-level to select the click trains
adrift.10dB<- filter(adrift, noiseLevel <= -10)
adrift.20dB<- filter(adrift, noiseLevel <= -20)
adrift.30dB<- filter(adrift, noiseLevel <= -30)

#lets look back through data knowing that noiseLevel can be used to hone in on

ggplot(filter(dets, noiseLevel <= -20), aes(x = peak, y = fmax_3dB, color = species))+
geom_point()+
  facet_wrap(~ Channel)

ggplot(adrift.20dB, aes(x = peak, y = BW_3dB, color = eventId))+
  geom_point()+
  facet_wrap(~ Channel)

ggplot(adrift.30dB, aes(x = peak, y = BW_3dB, color = eventId))+
  geom_point()+
  facet_wrap(~ Channel)

##
