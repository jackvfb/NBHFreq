library(PAMpal)
library(ggtext)

k <- calculateAverageSpectra(justKosp,
                            evNum = 1:length(events(justKosp)),
                            plot = FALSE)
k <- tibble(spec = k$avgSpec, freq = k$freq)

d <- calculateAverageSpectra(justPhda,
                             evNum = 1:length(events(justPhda)),
                             plot = FALSE)
d <- tibble(spec = d$avgSpec, freq = d$freq)

h <- calculateAverageSpectra(justPhph,
                             evNum = 1:length(events(justPhph)),
                             plot = FALSE)
h <- tibble(spec = h$avgSpec, freq = h$freq)

l <- list("*Kogia* spp." = k, "*P. dalli*" = d, "*P. phocoena*" = h)
l <- list_rbind(l, names_to = "species")
l %>%
  as_tibble() %>%
  rename("Normalized Magnitude (dB)" = spec, "Frequency (kHz)" = freq) %>%
  mutate(species = factor(species, levels = c("*P. dalli*", "*Kogia* spp.", "*P. phocoena*"))) %>%
  mutate(`Frequency (kHz)` = `Frequency (kHz)`/1000) %>%
  ggplot(aes(`Frequency (kHz)`, `Normalized Magnitude (dB)`, color = species))+
  geom_smooth(se = FALSE, span = 0.15)+
  xlim(100, 160) +
  labs(title = "Averaged Spectra for NBHF Species") +
  theme_classic()+
  theme(legend.text = element_markdown(), plot.title = element_text(size = 18, hjust = 0.5))
