source("R/getMyEvents.R")
spectra %>%
  filter(species == "Kosp") %>%
  ggplot(aes(peak, color = numPk))+
  geom_density()+
  facet_wrap(~Channel)

ord_data <- spectra %>%
  nest(data = -c(Channel, species, numPk)) %>%
  filter(species == "Kosp") %>%
  mutate(ord = map(data,function(data) map_countClicks(data))) %>%
  mutate(plot = map(ord, ~ggplot(data = .x, aes(x = NMDS1, y = NMDS2))+
                       geom_point()))

ord_data$plot

dist <- vegdist(ord_data$ord, method = "bray")
