
# ORDINATION --------------------------------------------------------------
set.seed(123)

mCh1 <- countClicks(spectra, centerkHz_3dB)$m
mCh2 <- countClicks(spectra, centerkHz_3dB)$m

dist <- lapply(list(mCh1, mCh2), avgdist, dmethod = "bray", sample = 30)

nmds <- lapply(dist, metaMDS)

scores(nmds[[1]]) %>%
  as_tibble(rownames = "eventId") %>%
  inner_join(filter(metadata, Channel == 1), by = "eventId") %>%
  ggplot(aes(x = NMDS1, y = NMDS2, color = species))+
  geom_point()

scores(nmds[[2]]) %>%
  as_tibble(rownames = "eventId") %>%
  inner_join(filter(metadata, Channel == 2), by = "eventId") %>%
  ggplot(aes(x = NMDS1, y = NMDS2, color = species))+
  geom_point()

# COLLECTORS CURVES -----------------------------------------

#Collector's curve
spectra %>%
  select(UID, Channel, eventId, centerkHz_3dB, species) %>%
  filter(Channel == 1) %>%
  mutate(x = cut(centerkHz_3dB,
                 breaks = seq(from = floor(min(centerkHz_3dB)),
                              to = ceiling(max(centerkHz_3dB)),
                              by = 0.1))) %>%
  group_by(eventId) %>%
  summarize(n_U = length(unique(x)), n = n(), species = species) %>%
  ggplot(aes(x = n, y = n_U, color = species))+
  geom_point()+
  # geom_abline(slope = 1, intercept = 0)+
  scale_y_log10()

#Collector's curve for peak which has fewer significant figures compared to centerkHz,
#clearly the same relationship but the absolute values on the y axis are less
spectra %>%
  select(UID, Channel, eventId, peak, species) %>%
  filter(Channel == 1) %>%
  mutate(x = cut(peak,
                 breaks = seq(from = floor(min(peak)),
                              to = ceiling(max(peak)),
                              by = 0.1))) %>%
  group_by(eventId) %>%
  summarize(n_U = length(unique(x)), n = n(), species = species) %>%
  ggplot(aes(x = n, y = n_U, color = species))+
  geom_point()+
  # geom_abline(slope = 1, intercept = 0)+
  scale_y_log10()

# SUMMARY -----------------------------------------------------------------

#summary of bins by how many events they appear in.
spectra %>%
  select(Channel, eventId, centerkHz_3dB) %>%
  filter(Channel == 1) %>%
  select(-Channel) %>%
  mutate(u = cut(centerkHz_3dB,
                 breaks = seq(from = floor(min(centerkHz_3dB)),
                              to = ceiling(max(centerkHz_3dB)),
                              by = 0.1))) %>%
  group_by(u) %>%
  summarize(n_Ev = length(unique(eventId))) %>%
  arrange(desc(n_Ev))%>%
  print(n = Inf)


# -------------------------------------------------------------------------

sampling_coverage <- spectra %>%
  select(UID, Channel, eventId, centerkHz_3dB, species) %>%
  filter(Channel == 1) %>%
  mutate(clicks = cut(centerkHz_3dB,
                      breaks = seq(from = floor(min(centerkHz_3dB)),
                                   to = ceiling(max(centerkHz_3dB)),
                                   by = 2))) %>%
  group_by(eventId) %>%
  summarize(n = n(), n_peaks = length(unique(clicks)))

sampling_coverage %>%
  ggplot(aes(n_peaks))+
  geom_histogram(binwidth = 1)+
  coord_cartesian(xlim = c(0, 50))

sampling_coverage %>%
  ggplot(aes(x = 1, y = n_peaks))+
  geom_jitter()+
  scale_y_log10()

sampling_coverage %>%
  arrange(n_peaks) %>%
  ggplot(aes(x = 1:nrow(.), y = n_peaks))+
  geom_line()+
  scale_y_log10()
