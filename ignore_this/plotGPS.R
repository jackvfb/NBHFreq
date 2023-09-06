library(tidyverse)
library(ggmap)
library(maptiles)
library(tools)
library(glue)
library(ggOceanMaps)
library(ggspatial)

drgps <- gps(myStudy)
drgps <- mutate(drgps, driftId = file_path_sans_ext(basename(db)))
drbox <- make_bbox(drgps$Longitude, drgps$Latitude, f = 0.1)
drmap <- get_map(location = drbox,
                 maptype = "terrain-background",
                 source = "stamen")
ggmap(drmap)+
  geom_point(data = filter(drgps, driftId == "ADRIFT_015"),
            aes(Longitude, Latitude),
            color = 'blue')+
  geom_point(data = filter(drgps, driftId == "ADRIFT_024"),
            aes(Longitude, Latitude),
            color = 'green')+
  geom_point(data = filter(drgps, driftId == "ADRIFT_025"),
            aes(Longitude, Latitude),
            color = 'yellow')+
  geom_point(data = filter(drgps, driftId == "ADRIFT_017"),
            aes(Longitude, Latitude),
            color = 'orange')+
  geom_point(data = filter(drgps, driftId == "CCES_016"),
            aes(Longitude, Latitude),
            color = 'red')+
  geom_point(data = filter(drgps, driftId == "CCES_019"),
              aes(Longitude, Latitude),
              color = 'purple')

# investigate whats wrong with cces drifts --------------------------------

cces <- drgps %>%
  filter(grepl("CCES", .$driftId))
  distinct(Latitude, Longitude, .keep_all = TRUE)

cces_box <- make_bbox(cces$Longitude, cces$Latitude, f = 0.1)

ccesmap <- get_map(location = cces_box,
                   maptype = "terrain-background",
                   source = "stamen")

maps <- cces %>%
  as_tibble() %>%
  nest(data = -driftId) %>%
  mutate(map = map2(data, driftId,
                    ~ggmap(ccesmap)+
                     geom_path(data = .x, aes(Longitude, Latitude))+
                     ggtitle(glue("Drift ID: {.y}"))))

maps$map

ggmap(ccesmap)+
  geom_path(data = drgps, aes(Longitude, Latitude, color = driftId))

#now plot just detections

ggmap(drmap)+
  geom_point(data = filter(dets, species != 'Phph'), aes(Longitude, Latitude, color = eventId))


# try again with just GPS data --------------------------------------------

gpsFiles <- list.files("analysis/data/raw_data/second_training/gps", full.names = TRUE)
gpsAll <- map(gpsFiles, read_csv)
# have to remove DeviceId column because sometimes char, sometimes double, so cant rbind
gpsAll <- map(gpsAll, subset, select = -6)
gpsAll <- gpsAll %>%
  bind_rows() %>%
  select(Latitude, Longitude, UTC, DriftName)

maps <- gpsAll %>%
  nest(data = -DriftName) %>%
  mutate(map = map2(data, DriftName,
                    ~ggmap(ccesmap)+
                      geom_path(data = .x, aes(Longitude, Latitude))+
                      ggtitle(glue("Drift ID: {.y}"))))
maps$map

ggmap(drmap)+
  geom_path(data = gpsAll, aes(Longitude, Latitude, color = DriftName))


# trying again with OceanMaps ---------------------------------------------

basemap(data = gpsAll, bathymetry = TRUE, bathy.style = "rbb") +
  geom_spatial_path(data = gpsAll, aes(x = Longitude, y = Latitude, .by = DriftName), size = 1, color = "red")
