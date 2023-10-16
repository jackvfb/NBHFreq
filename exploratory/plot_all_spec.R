myEvents <- lapply(events(myStudy), id)
evSpecData <-  lapply(myEvents, function(x){
  calculateAverageSpectra(myStudy, evNum = x, plot = FALSE)
})
evSpecNoise <- lapply(evSpecData, function(x){
  data.frame(value = x$avgNoise, freq = x$freq)
})
evSpecSig <- lapply(evSpecData, function(x){
  data.frame(value = x$avgSpec, freq = x$freq)
})

evSpecNoise_df <- list_rbind(evSpecNoise, names_to = "eventId")
evSpecSig_df <- list_rbind(evSpecSig, names_to = "eventId")
ggplot(evSpecNoise_df, aes(x = freq, y = value, color = eventId))+
  geom_line()

ggplot(evSpecSig_df, aes(x = freq, y = value, color = eventId))+
  geom_line()+
  scale_x_continuous(limits = c(100000, 150000))
