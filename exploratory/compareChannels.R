library(broom)
library(tidyverse)

myStudy <- readRDS("analysis/data/derived_data/second_training/second.rds")
source('R/mutateDets.R')
dets <- getClickData(myStudy)
dets <- mutateDets(dets)

sig_ev <- dets$allCh %>%
  filter(eventId != "BNGRNG.OE1") %>%
  nest(data = -eventId) %>%
  mutate(test = map(.x = data, ~wilcox.test(peak~Channel, data = .x) %>% tidy)) %>%
  unnest(test) %>%
  filter(p.value < 0.05) %>%
  select(eventId, p.value)
