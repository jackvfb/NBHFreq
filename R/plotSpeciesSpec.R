library(PAMpal)
library(tidyverse)

myStudy = readRDS('analysis/data/derived_data/second_training/myStudy2.rds')

getAllSpec <- function(study) {
  l <- calculateAverageSpectra(study, evNum = 1:length(events(study)), norm = TRUE)
  df <- l$allSpec
  df <- data.frame(df, row.names = l$freq)
  colnames(df) <- l$UID
  df <- df[,!duplicated(l$UID)]
}

makeDf <- function(df) {
  df %>%
    as_tibble(rownames = "freq") %>%
    pivot_longer(cols = -freq, names_to = "UID") %>%
    mutate(freq = as.numeric(freq)) %>%
    group_by(freq) %>%
    summarize(mn = mean(value),
              lci = quantile(value, probs = 0.25),
              uci = quantile(value, probs = 0.75)) %>%
    filter(freq >= 100000 & freq <= 160000)
}


justKosp <-  filter(myStudy, species == "Kosp")
justPhda <-  filter(myStudy, species == "Phda")
justPhph <-  filter(myStudy, species == "Phph")

studies <- list("Kosp" = justKosp, "Phda" = justPhda, "Phph" = justPhph)
data <- lapply(studies, getAllSpec)
dataframes <- lapply(data, makeDf)
dataTib <- list_rbind(dataframes, names_to = "species")

# Make figure of genearlized spectra
#Define color palette and labels
spNames = c("Kosp" = "*Kogia* spp.",
            "Phda" = "Dall's porpoise",
            "Phph" = "Harbor porpoise")
spCols = c("Phda" = '#F8766D',
           "Kosp" = '#00BA38',
           "Phph" = '#619CFF')

dataTib %>%
  ggplot() +
  geom_line(aes(freq/1000, mn, color = species), size = 1) +
  geom_ribbon(aes(freq/1000, ymin = lci, ymax = uci, fill = species), alpha = 0.3) +
  labs(title = "NBHF Click Spectra by Species",
       fill = "Interquartile range",
       color = "Median") +
  xlab("Frequency (kHz)") +
  ylab("Magnitude (dB)") +
  scale_fill_manual(values = spCols, labels = spNames) +
  scale_color_manual(values = spCols, labels = spNames) +
  theme(legend.text = element_markdown(),
        plot.title = element_text(hjust = 0.5, size = 18))

# ko <- calculateAverageSpectra(justKosp, evNum = 1:length(events(justKosp)))
# m <- ko$allSpec
# # m <- apply(m, 2, max)
#
# length(m)
# d <- data.frame(m)
# colnames(d) <- ko$UID
