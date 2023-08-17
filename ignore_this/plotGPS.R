library(tidyverse)
library(ggmap)

drgps <- gps(myStudy)
drgps <- mutate(drgps, driftId = file_path_sans_ext(basename(db)))
drbox <- make_bbox(drgps$Longitude, drgps$Latitude, f = 0.1)
drmap <- get_map(location = drbox,
                 maptype = "terrain-background",
                 source = "stamen")
ggmap(drmap)+
  geom_path(data = filter(drgps, driftId == "ADRIFT_015"),
            aes(Longitude, Latitude),
            color = 'blue')+
  geom_path(data = filter(drgps, driftId == "ADRIFT_024"),
            aes(Longitude, Latitude),
            color = 'green')+
  geom_path(data = filter(drgps, driftId == "ADRIFT_025"),
            aes(Longitude, Latitude),
            color = 'yellow')+
  geom_path(data = filter(drgps, driftId == "ADRIFT_017"),
            aes(Longitude, Latitude),
            color = 'orange')+
geom_path(data = filter(drgps, driftId == "CCES_016"),
          aes(Longitude, Latitude),
          color = 'red')+
geom_path(data = filter(drgps, driftId == "CCES_019"),
            aes(Longitude, Latitude),
            color = 'purple')

#now plot just detections

ggmap(drmap)+
  geom_point(data = filter(dets, species != 'Phph'), aes(Longitude, Latitude, color = eventId))
