
# plot avg noise spectra for all events -----------------------------------

justKosp <- filter(myStudy, species == "Kosp")
kEv <- lapply(events(justKosp), id)
t <-  lapply(kEv, function(x){
  calculateAverageSpectra(justKosp, evNum = x, plot = FALSE)
})
u <- lapply(t, function(x){
  data.frame(value = x$avgNoise, freq = x$freq)
})

v <- list_rbind(u, names_to = "eventId")

ggplot(v, aes(x = freq, y = value, color = eventId))+
  geom_line()

# plot average signal spectra for all events ------------------------------


justKosp <- filter(myStudy, species == "Kosp")
kEv <- lapply(events(justKosp), id)
t <-  lapply(kEv, function(x){
  calculateAverageSpectra(justKosp, evNum = x, plot = FALSE)
})
u <- lapply(t, function(x){
  data.frame(value = x$avgSpec, freq = x$freq)
})

v <- list_rbind(u, names_to = "eventId")

# -------------------------------------------------------------------------


ggplot(v, aes(x = freq, y = value, color = eventId))+
  geom_line()

# distance matrix on spectra of individual clicks -------------------------

#matrix with all spectra from all UID
km <- k$allSpec
justKosp <- filter(myStudy, species == "Kosp")
kEv <- lapply(events(justKosp), id)
t <-  lapply(kEv, function(x){
  calculateAverageSpectra(justKosp, evNum = x)
})
u <- lapply(t, function(x){
  data.frame(value = x$avgNoise, freq = x$freq)
})

v <- list_rbind(u, names_to = "eventId")

ggplot(v, aes(x = freq, y = value, color = eventId))+
  geom_line()

colnames(v) <- k$freq

#add attributes to the matrix
rownames(km) <- k$freq
colnames(km) <- k$UID
#transpose the matrix so as to perform distance calculation
km <- t(km)
#distance calculation
dist <- vegdist(km, method = "euclidean")
#ordination
nmds <- metaMDS(dist)
#plot
scores(nmds) %>%
  as_tibble(rownames = "UID") %>%
  inner_join(filter(spectra, Channel == 1), by = "UID") %>%
  ggplot(aes(x = NMDS1, y = NMDS2, color = eventId))+
  geom_point()

km_df <- as.data.frame(km)


# same thing but for noise ------------------------------------------------

#matrix with all spectra used to calculate the average spectrum
knm <- k$allNoise
#add attributes to the matrix
rownames(knm) <- k$freq
colnames(knm) <- k$UID
#transpose the matrix so as to perform distance calculation
knm <- t(knm)
knm <- na.omit(knm)
#distance calculation
ndist <- vegdist(knm, method = "euclidean")
#ordination
nnmds <- metaMDS(ndist)
#plot
scores(nnmds) %>%
  as_tibble(rownames = "UID") %>%
  inner_join(filter(spectra, Channel == 1), by = "UID") %>%
  ggplot(aes(x = NMDS1, y = NMDS2, color = eventId))+
  geom_point()


# plot individual noise spectra -------------------------------------------

plot <- k$allNoise %>%
  t() %>%
  as_tibble()

colnames(plot) <- k$freq

plot %>%
  add_column(UID = k$UID) %>%
  relocate(UID) %>%
  pivot_longer(-UID, names_to = "freq")
  ggplot(aes(freq, value))+
  geom_line()


ggplot(plot, aes(x = freq))
