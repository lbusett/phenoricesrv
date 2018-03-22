# Figure area comparison PhenoRice vs Official

library(dplyr)
library(ggplot2)
library(tidyr)
library(lbscripts)
library(sprawl)
library(data.table)


#

# load("/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Datasets/statistics_modis/extract_rast_mask_srv.RData")
load("/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Datasets/statistics_modis/Final/extract_Phenorice_Final_senegal_only.RData")

# load("/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Datasets/statistics_modis/extract_Phenorice_srv_nov_17.RData")
in_areafile = "/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Datasets/Area_Validation/Official_Areas.csv"
off_area <- read.csv(in_areafile)

tot_area <- off_area %>%
  filter(Region == "SRV") %>%
  mutate(Year = Year + 1) %>%
  mutate(Season = as.factor(Season)) %>%
  group_by(Year, Season) %>%
  summarize(Area = sum(Area, na.rm = T)) %>%
  group_by(Season) %>%
  mutate(delta_mean = 100 * ((Area) - mean(Area)) / mean(Area, na.rm = T))

# tot_area$Area[(tot_area$Year %in% c(2011 )) ] = NA
# Barplot official areas

p_off <- ggplot(tot_area, aes(x = factor(Year), y = Area, fill = Season)) +
  geom_bar(stat = "identity") + theme_bw() +
  scale_fill_manual("Season", labels = c("Dry-hot", "Wet-cold"),
                    values = c("darkred", "darkgreen")) +
  ylab("Area [ha]") +
  labs(title = "Rice cultivated area in SVN - Official statistics") +
  theme(plot.title = element_text(family = "AvantGarde")) +
  theme(legend.position = c(0.1,0.90), legend.box.background = element_rect(),
        legend.box.margin = margin(1,1,1,1)) +
  xlab("Year")
p_off


# Compute the PhenoRice areas

data_srv_dry <- data_sen %>%
  filter(band_name == "SOS_1") %>%
  mutate(area_mod = n_pix_val * 231.656358^2 / 10000) %>%
  mutate(Season = "Dry")

data_srv_wet <- data_sen %>%
  filter(band_name == "SOS_2") %>%
  mutate(area_mod = n_pix_val * 231.656358^2 / 10000) %>%
  mutate(Season = "Wet")

data_mod <- rbind(data_srv_dry, data_srv_wet) %>%
  rename(Year = year) %>%
  mutate(Season = as.factor(Season)) %>%
  dplyr::select(Year, Season, area_mod) %>%
  group_by(Season) %>%
  mutate(delta_mean_mod = 100 * ((area_mod) - mean(area_mod)) / mean(area_mod, na.rm = T))

data_all <- dplyr::left_join(tot_area, data_mod) %>%
  filter(!is.na(area_mod))  %>%
  filter(!is.na(Area))  %>%
  dplyr::rename(Off_Area = Area, PhenoRice_Area = area_mod)

data_all_long <- data_all %>%
  gather(method, estarea, -c(Season, Year))

p_comp <- ggplot(data_all_long, aes(x = Year, y = estarea, fill = Season ,position = "dodge")) +
  geom_bar(stat = "identity") + theme_bw() +
  scale_fill_manual(values = c("darkred", "darkgreen")) +
  facet_wrap(~method)

data_all_tot <- data_all %>%
  group_by(Year) %>%
  summarize(Off_Area = sum(Off_Area, na.rm = T), PhenoRice_Area = sum(PhenoRice_Area, na.rm = T)) %>%
  mutate(delta_mean_mod = 100 * (PhenoRice_Area - mean(PhenoRice_Area, na.rm = T)) / mean(PhenoRice_Area, na.rm = T)) %>%
  mutate(delta_mean_off = 100 * (Off_Area - mean(Off_Area, na.rm = T)) / mean(Off_Area, na.rm = T))


# p1 <- ggplot(data_all_tot, aes(x = Off_Area, y = PhenoRice_Area)) +
#   geom_point() + theme_bw()
#
#
# p2 <- ggplot(data_all, aes(x = Off_Area, y = PhenoRice_Area)) +
#   geom_point() + facet_wrap(~Season, scales = "free") +
#   stat_smooth(method = "lm") + theme_bw()
#
p1 <- lb_plotregr(data_all_tot,
                  X_VAR = "Off_Area",
                  Y_VAR = "PhenoRice_Area",
                  LABEL_VAR = "Year",
                  nudge_y = 500) +
  theme_light() +
  scale_y_continuous("PhenoRice Rice Area") +
  xlab("Official Rice Area")

p2 <- lb_plotregr(data_all,
                  X_VAR = "Off_Area",
                  Y_VAR = "PhenoRice_Area",
                  GP_VAR = "Season",
                  LABEL_VAR = "Year",
                  nudge_y = 500, common_range = F) +
  theme_light() + xlab("Official Rice Area") + scale_y_continuous("PhenoRice Rice Area")

gridExtra::grid.arrange(p1,p2)


p1 <- lb_plotregr(data_all_tot,
                  X_VAR = "delta_mean_off",
                  Y_VAR ="delta_mean_mod",
                  LABEL_VAR = "Year",
                  nudge_y = 5, common_range = T) +
  theme_light() + scale_y_continuous("PhenoRice Area [% diff to 2003-2016 average]") +
  xlab("Official Area [% diff to 2003-2016 average]") +
  ggtitle("Interannual variations in rice area - PhenoRice vs Official Statistics")+
  theme(plot.title = element_text(family = "AvantGarde"))

levels(data_all$Season) <- c("Dry-hot Season", "Wet-cold Season")
p2 <- lb_plotregr(data_all,
                  X_VAR = "delta_mean",
                  Y_VAR = "delta_mean_mod",
                  GP_VAR = "Season",
                  LABEL_VAR = "Year",
                  nudge_y = 5,
                  common_range = FALSE) +
  theme_light() + xlab("Official Rice Area") + scale_y_continuous("PhenoRice Area [% diff to 2003-2016 average]") +
  xlab("Official Area [% diff to 2003-2016 average]")
gridExtra::grid.arrange(p1,p2)


ggplot(data_all, aes(x = Off_Area, y = PhenoRice_Area, color = Season, label = Year)) +
  geom_point() + theme_bw() + geom_text(nudge_y = 1000) + geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, colour = "grey70", size = 0.5) +
  geom_vline(xintercept = 0, colour = "grey70", size = 0.5) +
  xlim(-1000, 45000) + ylim(-1000,45000) +
  scale_colour_manual(values = c("darkred", "darkgreen")) +
  ylab("PhenoRice estimated rice area [ha]") + xlab("SEAD rice area [ha]") +
  geom_abline(slope = 1)

