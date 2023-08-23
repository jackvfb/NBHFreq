
ord12 <- spectra %>%
  select(Channel, eventId, peak, numPk) %>%
  filter(Channel == 1, numPk == 1 | numPk == 2) %>%
  select(-Channel) %>%
  mutate(name = as.character(cut(peak,
                                 breaks = seq(from = floor(min(peak)),
                                              to = ceiling(max(peak)),
                                              by = .1)))) %>%
  select(-peak) %>%
  group_by(eventId, name) %>%
  mutate(value = n()) %>%
  distinct(eventId, name, .keep_all = TRUE) %>%
  pivot_wider(id_cols = eventId, values_fill = 0) %>%
  as.data.frame()

rownames(ord12) <- ord12$eventId
ord12 <- ord12[,-1]
ord12 <- as.matrix(ord12)

set.seed(04291992)
dist <- vegdist(ord12, method = "bray")
nmds <- metaMDS(dist)
scores(nmds) %>%
  as_tibble(rownames = "eventId") %>%
  inner_join(filter(metadata, Channel == 1), by = "eventId") %>%
  ggplot(aes(x = NMDS1, y = NMDS2, color = species))+
  geom_point()
