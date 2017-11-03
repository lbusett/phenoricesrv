
load("/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Datasets/statistics_modis/extract_rast.RData")

# Plot Regressions - Dates ####

data_sen <- data_sen %>%
  dplyr::filter(band_name %in% c("SOS_1", "SOS_2", "EOS_1", "EOS_2", "FOS_1", "FOS_2",
                                 "LG_1", "LG_2", "VGT_LG_1", "VGT_LG_2",
                                 "VGTCEVI_1", "VGTCEVI_2"))

data_sen <- data_sen %>%
  dplyr::filter(band_name %in% c("SOS_1", "SOS_2", "EOS_1",
                                 "EOS_2", "FOS_1", "FOS_2"),
                !is.na(avg)) %>%
  dplyr::mutate(date = doytodate(avg, 2000),
                datesdplus  = date + sd/n_pix_val^0.5,
                datesdminus = date - sd/n_pix_val^0.5,
                band_name = factor(band_name,
                                   labels = c("Sowing Date - Dry Season",
                                              "Sowing Date - Wet Season",
                                              "Flowering Date - Dry Season",
                                              "Flowering Date - Wet Season",
                                              "Harvesting Date - Dry Season",
                                              "Harvesting Date - Wet Season"),
                                   levels = c("SOS_1", "SOS_2", "FOS_1",
                                              "FOS_2", "EOS_1", "EOS_2")))
a    <- lb_linregr(data_sen, "year", "date", "band_name")
plot <- lb_plotregr(data_sen, "year", "date", GP_VAR = "band_name",
                    common_range = T)
plot <- plot + geom_errorbar(aes(ymin = datesdminus, ymax = datesdplus )) +
  theme_bw() + labs(y = "Date") + xlab("Year")
ggsave("/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Manuscript/Figures/Fig_6_dates.png",
       width = 8.5, height = 8, units = "in")

# Plot Regressions - Lengths ####

load("/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Datasets/statistics_modis/extract_rast.RData")

data_lgt <- data_sen %>%
  filter(band_name %in% c("LG_1", "LG_2", "VGT_LG_1", "VGT_LG_2",
                          "VGTCEVI_1", "VGTCEVI_2")) %>%
dplyr::mutate(val =avg,
              valsdplus  = avg + sd/n_pix_val^0.5,
              valsdminus = avg - sd/n_pix_val^0.5,
              band_name = factor(band_name,
                                 labels = c("Season Length - Dry Season",
                                            "Season Length - Wet Season",
                                            "Vegetative Season Length - Dry Season",
                                            "Vegetative Season Length - Wet Season",
                                            "Vegetative Season EVI - Dry Season",
                                            "Vegetative Season EVI - Wet Season"
                                 ),
                                 levels = c("LG_1", "LG_2", "VGT_LG_1",
                                            "VGT_LG_2", "VGTCEVI_1", "VGTCEVI_2")))

lgts <- subset(data_lgt, !(band_name %in% c("Vegetative Season EVI - Dry Season",
                                            "Vegetative Season EVI - Wet Season")))

a    <- lb_linregr(lgts, "year", "avg", "band_name")
names(lgts)[6] = "Number of Days"
plot <- lb_plotregr(lgts, "year", "Number of Days", GP_VAR = "band_name",
                    common_range = T)
plot <- plot + geom_errorbar(aes(ymin = valsdminus, ymax = valsdplus )) +
  xlab("Year") +
  theme_bw()


ggsave("/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Manuscript/Figures/Fig_6_lengths.png",
       width = 8.5, height = 5.33, units = "in")


cumevi <- subset(data_lgt, (band_name %in% c("Vegetative Season EVI - Dry Season",
                                              "Vegetative Season EVI - Wet Season")))
names(lgts)[6] = "Cumulated EVI"

plot <- lb_plotregr(cumevi, "year", "val", GP_VAR = "band_name",
                    common_range = T)
plot <- plot + geom_errorbar(aes(ymin = valsdminus, ymax = valsdplus )) +
  xlab("Year") +
  theme_bw()

ggsave("/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Manuscript/Figures/Fig_6_EVI.png",
       width = 8.5, height = 2.665, units = "in")


regr$labels <- paste0("slope = ", round(regr$slope,3), " R2 = ", round(regr$r.squared, 3))

ggplot(data_sen, aes(x = year, y = date)) +
  geom_point() +
  geom_errorbar(aes(x = year, ymin = datesdminus, ymax = datesdplus)) +
  geom_smooth(method = "lm", se = T, color = "black") +

  # geom_boxplot(aes(x = as.factor(year), y = date)) +
  facet_wrap(~band_name, scale = "free_y", ncol = 2) +
  theme_light() + scale_y_date(expand = expand_scale(mult = 0.4)) +
  geom_text(data = regr, x = -Inf, y = Inf, aes(label = lab),
            hjust = -0.05, vjust = 1.5)