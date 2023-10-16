library(randomForest)
library(rfPermute)

rf_data <- slide_clicks %>%
  select(species, peak, centerkHz_3dB, fmin_3dB, fmax_3dB, BW_3dB, duration) %>%
  as.data.frame()

rf_data <- rf_data[complete.cases(rf_data), ]

ss <- balancedSampsize(rf_data$species)

rf <- randomForest(formula = species ~ .,
                   data = rf_data,
                   sampsize = ss,
                   proximity = TRUE,
                   importance = TRUE)
saveRDS(rf, file = "analysis/data/derived_data/clickRF.rds")
rf <- readRDS(file = "analysis/data/derived_data/clickRF.rds")
plotImportance(rf) #visualize importance
plotTrace(rf)   #model stability w/number of trees (this should be flat!)
plotInbag(rf)
plotPredictedProbs(rf)
plotProximity(rf) #takes a long time\


# FIGURES FOR RIP TALK -----------------------------------------------------

p <- plotImpPreds(rf, rf_data, "species")  #distribution of predictors
# p$data <- mutate(p$data,
#                  `.class.` = case_when(`.class.` == "Phda" ~ "*P. dalli*",
#                                        `.class.` == "Kosp" ~ "*Kogia* spp.",
#                                         .default = "*P. phocoena*"))
cols = c("Dall's porpoise" = '#F8766D', "*Kogia* spp." = '#00BA38', "Harbor porpoise" = '#619CFF')
p + labs(title = "Click Parameters of NBHF Species") +
         # subtitle = "*Spectral values given RE 3dB*") +
  aes(color = `.class.`) +
  facet_wrap(~ var, strip.position = "left", scales = "free_y", nrow = 2,
             labeller = as_labeller(c(BW_3dB = "Bandwidth (kHz)",
                                   fmax_3dB = "Frequency~max~ (kHz)",
                                   centerkHz_3dB = "Frequency~center~ (kHz)",
                                   peak = "Frequency~peak~(kHz)",
                                   fmin_3dB = "Frequency~min~ (kHz)",
                                   duration = "Duration(\u03bcs)"))) +
  scale_colour_manual(values = cols) +
  # theme_classic() +
  # scale_color_manual(values = cols) +
  theme(axis.text.x = element_markdown(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text.y.left = element_markdown(),
        strip.placement = "outside",
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_markdown(hjust = 0.5, size = 12, color = 'grey30'),
        legend.position = "none",
        panel.spacing.x = unit(1, "cm"),
        panel.spacing.y = unit(1, "cm"))


p2 <- plotConfMat(rf)
p2 + labs(title = "Confusion Matrix of NBHF Click Classification Model") +
  theme(axis.text.y = element_markdown(), axis.text.x.top = element_markdown(),
        plot.title = element_text(hjust = 0.5, size = 18))
