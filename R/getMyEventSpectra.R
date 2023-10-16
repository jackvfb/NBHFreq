#apply PAMpal::calculateAverageSpectra to each event in the study to generate lists of spectra

library(PAMpal)

myStudy <- readRDS('analysis/data/derived_data/second_training/myStudy2.rds')

getAvSpec <- function(ev, ch){
  calculateAverageSpectra(myStudy,
                          channel = ch,
                          evNum = ev,
                          plot = FALSE,
                          filterfrom_khz = 110,
                          filterto_khz = 150)
}

myEvs <- lapply(events(myStudy), id)
spectra_ch1 <- lapply(myEvs, getAvSpec, ch = 1)
spectra_ch2 <- lapply(myEvs, getAvSpec, ch = 2)

saveRDS(spectra_ch1, file = "analysis/data/derived_data/training/spectra_ch1.rds")
saveRDS(spectra_ch2, file = "analysis/data/derived_data/training/spectra_ch2.rds")
