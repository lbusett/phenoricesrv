
# load("/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Datasets/statistics_modis/Final_short/extract_Phenorice_Final_alldata.RData")
# load("/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Datasets/statistics_modis/Final/extract_Phenorice_Final_alldata.RData")
#
# load("/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Datasets/statistics_modis/Final/lgtthresh_noshp_100/extract_Phenorice_Final_alldata.RData")
#
# load("/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Datasets/statistics_modis/Final/lgtthresh_noshp_100/extract_Phenorice_Final_alldata.RData")
# Plot Regressions - Dates ####

load("/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Datasets/statistics_modis/Final/extract_Phenorice_Final_alldata.RData")

data_sum <- data_sen %>%
  dplyr::filter(band_name %in% c("SOS_1", "SOS_2", "EOS_1", "EOS_2", "FOS_1", "FOS_2",
                                 "LG_1", "LG_2", "VGT_LG_1", "VGT_LG_2",
                                 "VGTCEVI_1", "VGTCEVI_2")) %>%
  dplyr::group_by(band_name, year) %>%
  dplyr::summarize(avg = mean(value, na.rm = TRUE),
                   sd  = sd(value, na.rm = TRUE),
                   n_pix_val = n_pix_val[1]) %>%
  ungroup()

data_sum <- data_sum %>%
  dplyr::filter(band_name %in% c("SOS_1", "SOS_2", "EOS_1",
                                 "EOS_2", "FOS_1", "FOS_2"),
                !is.na(avg)) %>%
  dplyr::mutate(date = doytodate(avg, 2000),
                datesdplus  = date + sd/(n_pix_val^0.5),
                datesdminus = date - sd/(n_pix_val^0.5),
                band_name = factor(band_name,
                                   labels = c("Sowing Date - Dry Season",
                                              "Sowing Date - Wet Season",
                                              "Flowering Date - Dry Season",
                                              "Flowering Date - Wet Season",
                                              "Harvesting Date - Dry Season",
                                              "Harvesting Date - Wet Season"),
                                   levels = c("SOS_1", "SOS_2", "FOS_1",
                                              "FOS_2", "EOS_1", "EOS_2")))
a    <- lb_linregr(data_sum, "year", "date", "band_name")
plot <- lb_plotregr(data_sum, "year", "date", GP_VAR = "band_name",
                    common_range = T) +  scale_y_date("Date", date_labels = "%d-%b")
plot <- plot + geom_errorbar(aes(ymin = datesdminus, ymax = datesdplus )) +
  theme_light() + xlab("Year")


ggsave("/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Manuscript/Figures/Figure_10a.png",
       width = 8.5, height = 8.3, units = "in", plot)

# ggsave("/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Manuscript/Figures/Fig_6_dates.png",
#        width = 8.5, height = 8, units = "in")

# Plot Regressions - Lengths ####

# load("/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Datasets/statistics_modis/Final_short/extract_Phenorice_Final_alldata.RData")
# load("/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Datasets/statistics_modis/Final/extract_Phenorice_Final_alldata.RData")
#
# load("/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Datasets/statistics_modis/Final/lgtthresh/extract_Phenorice_Final_senegal_only.RData")
#
# load("/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Datasets/statistics_modis/Final/lgtthresh_noshp/extract_Phenorice_Final_alldata.RData")
# Plot Regressions - Dates ####

load("/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Datasets/statistics_modis/Final/extract_Phenorice_Final_alldata.RData")

data_sum <- data_sen %>%
  dplyr::filter(band_name %in% c("LG_1", "LG_2", "VGT_LG_1", "VGT_LG_2",
                                 "VGTCEVI_1", "VGTCEVI_2")) %>%
  dplyr::group_by(band_name, year) %>%
  dplyr::summarize(avg = mean(value, na.rm = TRUE),
                   sd  = sd(value, na.rm = TRUE),
                   n_pix_val = n_pix_val[1]) %>%
  ungroup()

data_sum <- data_sum %>%
  dplyr::mutate(val = avg,
                valsdplus  = avg + sd/(n_pix_val^0.5),
                valsdminus = avg - sd/(n_pix_val^0.5),
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

lgts <- subset(data_sum, (band_name %in% c("Vegetative Season Length - Dry Season",
                                           "Vegetative Season Length - Wet Season")))

a    <- lb_linregr(lgts, "year", "avg", "band_name")
names(lgts)[6] = "Number of Days"



plot <- lb_plotregr(lgts, "year", "Number of Days", GP_VAR = "band_name",
                    common_range = T)
 # + scale_y_date("Date", date_labels = "%d-%b")
plot <- plot + geom_errorbar(aes(ymin = valsdminus, ymax = valsdplus )) +
  theme_light() + xlab("Year") + ylim(65, 80)


ggsave("/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Manuscript/Figures/Figure_10b.png",
       width = 8.5, height = 5.33, units = "in", plot)
