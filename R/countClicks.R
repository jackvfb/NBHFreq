countClicks <- function(spectra, param, ch = 1, binw = 0.1){
  info <- spectra %>%
    filter(Channel == 1) %>%
    select(eventId, {{param}}) %>%
    mutate(name = as.character(cut({{param}},
                                   breaks = seq(from = floor(min({{param}})),
                                                to = ceiling(max({{param}})),
                                                by = binw)))) %>%
    select(-{{param}}) %>%
    group_by(eventId, name) %>%
    mutate(value = n()) %>%
    distinct(eventId, name, .keep_all = TRUE)

  result <- info %>%
    pivot_wider(id_cols = eventId, values_fill = 0) %>%
    as.data.frame()

  rownames(result) <- result$eventId
  result <- result[,-1]
  result <- as.matrix(result)
  return(list(m = result, info = info))
}
