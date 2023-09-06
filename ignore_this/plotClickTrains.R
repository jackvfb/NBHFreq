library(lubridate)
source('R/getMyEvents.R')

plots <- spectra %>%
  nest(data = -eventId) %>%
  mutate(plot = map(data, ~ggplot(data = .x, aes(x = UTC, y = angle))+
                      geom_point()))
plots$plot


# select click trains -----------------------------------------------------

# spectra %>%
#   select(UID, Channel, eventId, UTC, angle) %>%
#   nest(data = c("UID", "UTC", "angle")) %>%
#   mutate()
#
#   mutate(data = tibble(data, rank))
#
# spectra %>%
#   filter(species == "Kosp") %>%
#   mutate()
