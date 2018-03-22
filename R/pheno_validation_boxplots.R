
library(dplyr)
library(ggplot2)
library(tidyr)
library(lbscripts)
library(sprawl)
library(data.table)
library(tibble)
library(sf)
library(raster)

load("/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Datasets/statistics_modis/extract_Phenorice_final_maskirrig_newvalvalley.RData")
load("/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Datasets/statistics_modis/extract_Phenorice_final_maskirrig_newvalvalley_november_1000_valleybig.RData")
load("/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Datasets/statistics_modis/extract_Phenorice_final_maskirrig_newvalvalley_november_1000_valleybig_alldata.RData")
# load("/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Datasets/statistics_modis/extract_Phenorice_srv_nov_17.RData")
gd_folder  <- "/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/"

validation_delta <- read_vect(
  file.path(gd_folder, "Datasets/Pheno_Validation/validation_delta.shp")) %>%
  st_transform((proj4string(r))) %>%
  dplyr::select(Name)

validation_valley <- read_vect(
  file.path(gd_folder, "Datasets/Pheno_Validation/validation_valley.shp")) %>%
  st_transform((proj4string(r))) %>%
  dplyr::select(id)

inpheno <- read.csv("/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/", stringsAsFactors = FALSE)
inpheno <- as_tibble(inpheno) %>%
  dplyr::mutate(sowdate = as.Date(sowdate, format = "%d/%m/%y")) %>%
  dplyr::mutate(harvdate = as.Date(harvdate, format = "%d/%m/%y")) %>%
  dplyr::filter(season == "Hivern") %>%
  dplyr::mutate(year = as.numeric(year)) %>%
  dplyr::mutate(sowdoy = datetodoy(sowdate, abort = FALSE), harvdoy = datetodoy(harvdate, abort = FALSE))
inpheno$harvdoy[which(inpheno$harvdoy < 200)] <- inpheno$harvdoy[which(inpheno$harvdoy < 200)] + 365

inpheno <- inpheno %>%
  dplyr::mutate(sowddate_tmp = doytodate(sowdoy, 2007), harvdate_tmp = doytodate(harvdoy, 2007)) %>%
  dplyr::mutate(village = factor(village))

valid_data <- inpheno %>%
  group_by(season, year, site) %>%
  summarize(sowdate  = mean(sowddate_tmp, na.rm = T),
            harvdate = mean(harvdate_tmp, na.rm = T),
            sow_sd   = sd(sowddate_tmp, na.rm = T),
            harv_sd  = sd(harvdate_tmp, na.rm = T),
            obs = n())
valid_data$site = factor(valid_data$site, labels = c("Boundoum (delta)", "Podor (middle valley)"))

valid_data <- valid_data %>%
  dplyr::filter(season == "Hivern")

mod_data_delta <- data_delta %>%
  dplyr::filter(band_name %in% c("SOS_1", "SOS_2", "EOS_1", "EOS_2")) %>%
  dplyr::select(c(3, 5, 6, 8, 10, 11)) %>%
  separate(band_name, sep = "_", into = c("Metric", "Season")) %>%
  mutate(Season = factor(Season , labels = c("Dry", "Wet"))) %>%
  mutate(site = "Boundoum (delta)")

mod_data_valley <- data_valley %>%
  dplyr::filter(band_name %in% c("SOS_1", "SOS_2", "EOS_1", "EOS_2")) %>%
  dplyr::select(c(3, 5, 6, 8, 10, 11)) %>%
  separate(band_name, sep = "_", into = c("Metric", "Season")) %>%
  mutate(Season = factor(Season , labels = c("Dry", "Wet"))) %>%
  mutate(site = "Podor (middle valley)")

mod_data <- rbind(mod_data_delta, mod_data_valley)

names(mod_data)[4:5] = c("avg_mod", "sd_mod")


# graph for SOS

sos_valid <- valid_data %>%
  ungroup() %>%
  dplyr::select(year, sowdate, sow_sd, site) %>%
  dplyr::rename(avg = sowdate, sd = sow_sd) %>%
  dplyr::mutate(avg = doytodate(datetodoy(avg), 2007))

mod_sos <- mod_data %>%
  dplyr::filter(Season == "Wet" & Metric == "SOS") %>%
  dplyr::select(year, avg_mod, sd_mod, site) %>%
  dplyr::mutate(avg_mod = doytodate(avg_mod, 2007))

sos_valid_data <- left_join(sos_valid, mod_sos) %>%
  dplyr::mutate(diff = as.numeric(avg - avg_mod)) %>%
  dplyr::group_by( site)

lb_linregr(sos_valid_data, "avg", "avg_mod", "site")

# diff <- sos_valid_data$avg - sos_valid_data$avg_mod

sos_valid_data <- sos_valid_data %>%
  filter(year > 2002 & year < 2010) %>%
  mutate(year = as.numeric(year))

pp <- ggplot(sos_valid_data, aes(x = year - 0.2, y = avg)) +
  geom_point(color = "darkblue") +
  geom_errorbar(aes(x = year - 0.2, ymin = avg - sd, ymax = avg + sd), color = "darkblue", width = 0.3) +
  geom_point(aes(x = year + 0.2, y = avg_mod), color = "darkred") +
  geom_errorbar(aes(x = year + 0.2, ymin = avg_mod - sd_mod, ymax = avg_mod + sd_mod), color = "darkred",
                , width = 0.3) +
  theme_light() +
  scale_y_date(limits = c(as.Date("2007-06-23") , as.Date("2007-10-07")), date_labels = "%d-%b") +
  scale_x_continuous("Year", limits = c(2002.6, 2009.4),
                     breaks = 2003:2009) +
  scale_color_manual("", values = c("darkblue", "darkred")) +
  ggtitle("Observed vs PhenoRice Sowing Dates") +
  theme(plot.title = element_text(family = "AvantGarde")) +
  facet_wrap(~site) +
  ylab("Sowing Date") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ggplot2::theme(panel.grid.minor = element_line(color = "transparent"))


# graph for EOS
eos_valid <- valid_data %>%
  ungroup() %>%
  dplyr::select(year, harvdate, harv_sd, site) %>%
  dplyr::rename(avg = harvdate, sd = harv_sd) %>%
  dplyr::mutate(avg = doytodate(datetodoy(avg), 2007)) %>%
  mutate(type = "Observed")

mod_eos <- mod_data %>%
  dplyr::filter(Season == "Wet" & Metric == "EOS") %>%
  dplyr::select(year, avg_mod, sd_mod, site) %>%
  dplyr::mutate(avg_mod = doytodate(avg_mod, 2007)) %>%
  # mutate(type = "PhenoRice") %>%
  mutate(obs = n())

# eos_valid_data <- rbind(eos_valid, mod_eos) %>%
#   filter(year > 2002 & year < 2010)

eos_valid_data <- left_join(eos_valid, mod_eos) %>%
  dplyr::mutate(diff = as.numeric(avg - avg_mod)) %>%
  dplyr::group_by( site)

regr_eos <- lb_linregr(eos_valid_data, "avg", "avg_mod", "site") %>%
  dplyr::mutate(Metric = "Harvesting Date") %>%
  dplyr::select(site, Metric, slope, intercept, r2, p, ME, RMSE) %>%
  dplyr::rename(Site = site, Intercept = intercept, Slope = slope)

regr_sos <- lb_linregr(sos_valid_data, "avg", "avg_mod", "site")%>%
  dplyr::mutate(Metric = "Sowing Date") %>%
  dplyr::select(site, Metric, slope, intercept, r2, p, ME, RMSE) %>%
  dplyr::rename(Site = site, Intercept = intercept, Slope = slope)

regr_table <- rbind(regr_sos, regr_eos) %>%
  xtable::xtable(digits = 3) %>%
  print(include.rownames = FALSE)

%>%
  print(include.rownames = FALSE)



pp2 <- ggplot(eos_valid_data, aes(x = year - 0.2, y = avg)) +
  geom_point(color = "darkblue") +
  geom_errorbar(aes(x = year - 0.2, ymin = avg - sd, ymax = avg + sd), color = "darkblue", width = 0.3) +
  geom_point(aes(x = year + 0.2, y = avg_mod), color = "darkred") +
  geom_errorbar(aes(x = year + 0.2, ymin = avg_mod - sd_mod, ymax = avg_mod + sd_mod), color = "darkred",
                , width = 0.3) +
  theme_light() +
  scale_y_date("Harvesting Date", limits = c(as.Date("2007-09-23") , as.Date("2008-02-01")), date_labels = "%d-%b") +
  scale_x_continuous("Year", limits = c(2002.6, 2009.4),
                     breaks = 2003:2009) +
  scale_color_manual("", values = c("darkblue", "darkred")) +
  ggtitle("Observed vs PhenoRice Harvesting Dates") +
  theme(plot.title = element_text(family = "AvantGarde")) +
  facet_wrap(~site) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ggplot2::theme(panel.grid.minor = element_line(color = "transparent"))

pp <- ggplot(eos_valid_data, aes(x = factor(year), y = avg)) +
  geom_point(aes(color = type), position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(color = type, ymin = avg - sd, ymax = avg + sd),
                position = position_dodge(width = 0.6)) +
  theme_light() +
  scale_y_date(limits = c(as.Date("2007-09-30") , as.Date("2008-01-30")), date_labels = "%d-%b") +
  scale_color_manual("", values = c("darkblue", "darkred")) +
  ggtitle("Observed vs PhenoRice Harvesting Dates") +
  theme(plot.title = element_text(family = "AvantGarde")) +
  facet_wrap(~site) +
  xlab("Year") + ylab("Harvesting Date")



ggplot(sos_valid_data, aes(x = avg, y = avg_mod, color = site, label =  year)) +
  geom_point() + theme_bw() + geom_text(nudge_y = 2) +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, colour = "grey70", size = 0.5)+
  geom_vline(xintercept = 0, colour = "grey70", size = 0.5) +
  scale_colour_manual(values = c("darkred", "darkgreen")) +
  ylab("PhenoRice average sos") + xlab("SEAD average sos") +
  facet_wrap(~site, scales = "free_y") + geom_abline(slope = 1) +
  coord_cartesian(xlim = as.Date(c("2007-07-01", "2007-09-15")),
                  ylim = as.Date(c("2007-07-01", "2007-09-15"))) +
  theme(legend.position = "bottom")

ggplot(eos_valid_data, aes(x = avg, y = avg_mod, color = site, label =  year)) +
  geom_point() + theme_bw() + geom_text(nudge_y = 2) +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, colour = "grey70", size = 0.5)+
  geom_vline(xintercept = 0, colour = "grey70", size = 0.5) +
  scale_colour_manual(values = c("darkred", "darkgreen")) +
  ylab("PhenoRice average eos") + xlab("SEAD average eos") +
  facet_wrap(~site, scales = "free_y") + geom_abline(slope = 1) +
  coord_cartesian(xlim = as.Date(c("2007-10-15", "2007-12-31")),
                  ylim = as.Date(c("2007-10-15", "2007-12-31"))) +
  theme(legend.position = "bottom")