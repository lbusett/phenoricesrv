# Script used to retrieve statistics of sow and harvest dates and create the
# validation plots


library(dplyr)
library(ggplot2)
library(tidyr)
library(lbscripts)
library(sprawl)
library(data.table)
library(tibble)
library(sf)
library(raster)

load("/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Datasets/statistics_modis/Final/extract_Phenorice_Final_senegal_only.RData")
gd_folder  <- "/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/"

validation_delta <- read_vect(
  file.path(gd_folder, "Datasets/Pheno_Validation/validation_delta.shp")) %>%
  st_transform((proj4string(r))) %>%
  dplyr::select(Name)

validation_valley <- read_vect(
  file.path(gd_folder, "Datasets/Pheno_Validation/validation_valley.shp")) %>%
  st_transform((proj4string(r))) %>%
  dplyr::select(id)

inpheno <- read.csv("/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Datasets/Pheno_Validation/donnnees_vallee_du_fleuve_senegal_sander_final.csv", stringsAsFactors = FALSE)
inpheno <- as_tibble(inpheno) %>%
  dplyr::mutate(sowdate = as.Date(sowdate, format = "%d/%m/%y")) %>%
  dplyr::mutate(harvdate = as.Date(harvdate, format = "%d/%m/%y")) %>%
  dplyr::filter(season == "Hivern") %>%
  dplyr::mutate(year = as.numeric(year)) %>%
  dplyr::mutate(sowdoy = datetodoy(sowdate, abort = FALSE), harvdoy = datetodoy(harvdate, abort = FALSE))
err_harv <- which(inpheno$harvdoy < 200)
inpheno$harvdoy[err_harv] <- inpheno$harvdoy[err_harv] + 365

inpheno <- inpheno %>%
  dplyr::mutate(village = factor(village))


valid_data <- inpheno %>%
  group_by(season, year, site) %>%
  summarize(sow_avg  = mean(sowdate, na.rm = T),
            harv_avg = mean(harvdate, na.rm = T),
            sow_sd   = sd(sowdate, na.rm = T),
            harv_sd  = sd(harvdate, na.rm = T),
            obs = n())
valid_data$site = factor(valid_data$site, labels = c("Boundoum (delta)", "Podor (middle valley)"))

# inpheno contains now all the data retrieved from the validation DB: valid_data
# contains averages and stnadard deviations

valid_data <- valid_data %>%
  dplyr::filter(season == "Hivern")

inpheno <- inpheno %>%
  dplyr::filter(season == "Hivern")

# Bring in phenorice results

mod_data_delta <- data_delta %>%
  dplyr::filter(band_name %in% c("SOS_1", "SOS_2", "EOS_1", "EOS_2")) %>%
  # dplyr::select(c(3, 5, 6, 8, 10, 11)) %>%
  tidyr::separate(band_name, sep = "_", into = c("Metric", "Season")) %>%
  mutate(Season = factor(Season , labels = c("Dry", "Wet"))) %>%
  mutate(site = "Boundoum (delta)")

mod_data_valley <- data_valley %>%
  dplyr::filter(band_name %in% c("SOS_1", "SOS_2", "EOS_1", "EOS_2")) %>%
  # dplyr::select(c(3, 5, 6, 8, 10, 11)) %>%
  tidyr::separate(band_name, sep = "_", into = c("Metric", "Season")) %>%
  mutate(Season = factor(Season , labels = c("Dry", "Wet"))) %>%
  mutate(site = "Podor (middle valley)")

mod_data <- rbind(mod_data_delta, mod_data_valley) %>% tibble::as_tibble() %>%
  dplyr::filter(Season == "Wet")

# summarize to get acg and sd
mod_data_summ <- mod_data %>%
  group_by(Metric, Season, site, year) %>%
  dplyr::filter(Season == "Wet") %>%
  summarize(avg_mod = mean(value, na.rm = TRUE), sd_mod = sd(value, na.rm = TRUE))

# graph for SOS

sos_valid <- valid_data %>%
  ungroup() %>%
  dplyr::select(year, sow_avg, sow_sd, site) %>%
  dplyr::rename(avg = sow_avg, sd = sow_sd) %>%
  dplyr::mutate(avg = doytodate(datetodoy(avg), 2007))

mod_sos <- mod_data_summ %>%
  dplyr::filter(Season == "Wet" & Metric == "SOS") %>%
  dplyr::select(year, avg_mod, sd_mod, site) %>%
  dplyr::mutate(avg_mod = doytodate(avg_mod, 2007))

sos_valid_data <- left_join(sos_valid, mod_sos) %>%
  dplyr::mutate(diff = as.numeric(avg - avg_mod)) %>%
  dplyr::group_by( site)

sos_valid_data <- sos_valid_data %>%
  filter(year > 2002 & year < 2011) %>%
  mutate(year = as.numeric(year))

lb_linregr(sos_valid_data, "avg", "avg_mod", "site")

# diff <- sos_valid_data$avg - sos_valid_data$avg_mod

Figure_5a <- ggplot(sos_valid_data, aes(x = year - 0.2, y = avg)) +
  geom_point(color = "darkblue") +
  geom_errorbar(aes(x = year - 0.2, ymin = avg - sd, ymax = avg + sd), color = "darkblue", width = 0.3) +
  geom_point(aes(x = year + 0.2, y = avg_mod), color = "darkred") +
  geom_errorbar(aes(x = year + 0.2, ymin = avg_mod - sd_mod, ymax = avg_mod + sd_mod), color = "darkred",
                , width = 0.3) +
  theme_light() +
  scale_y_date(limits = c(as.Date("2007-06-01") , as.Date("2007-10-01")), date_labels = "%d-%b") +
  scale_x_continuous("Year", limits = c(2002.6, 2010.4),
                     breaks = 2003:2010) +
  scale_color_manual("aaa", values = c("darkblue", "darkred"),
                     guide = guide_legend(override.aes = list(color = c("darkblue", "darkred")))) +
  ggtitle("Observed vs PhenoRice Sowing Dates") +
  theme(plot.title = element_text(family = "AvantGarde")) +
  facet_wrap(~site) +
  ylab("Sowing Date") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ggplot2::theme(panel.grid.minor = element_line(color = "transparent"))
ggsave("/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Manuscript/Figures/Figure_5a.png",
       width = 10, height = 5, dpi = 600, plot = Figure_5a)

# graph for EOS
eos_valid <- valid_data %>%
  ungroup() %>%
  dplyr::select(year, harv_avg, harv_sd, site) %>%
  dplyr::rename(avg = harv_avg, sd = harv_sd) %>%
  dplyr::mutate(avg = doytodate(datetodoy(avg), 2007)) %>%
  mutate(type = "Observed")

mod_eos <- mod_data_summ %>%
  dplyr::filter(Season == "Wet" & Metric == "EOS") %>%
  dplyr::select(year, avg_mod, sd_mod, site) %>%
  dplyr::mutate(avg_mod = doytodate(avg_mod, 2007)) %>%
  # mutate(type = "PhenoRice") %>%
  mutate(obs = n())

# eos_valid_data <- rbind(eos_valid, mod_eos) %>%
#   filter(year > 2002 & year < 2010)

eos_valid_data <- left_join(eos_valid, mod_eos) %>%
  dplyr::mutate(diff = as.numeric(avg - avg_mod)) %>%
  dplyr::group_by(site)

eos_valid_data <- eos_valid_data %>%
  filter(year > 2002 & year < 2012) %>%
  mutate(year = as.numeric(year))


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


Figure_5b <- ggplot(eos_valid_data, aes(x = year - 0.2, y = avg)) +
  geom_point(color = "darkblue") +
  geom_errorbar(aes(x = year - 0.2, ymin = avg - sd, ymax = avg + sd), color = "darkblue", width = 0.3) +
  geom_point(aes(x = year + 0.2, y = avg_mod), color = "darkred") +
  geom_errorbar(aes(x = year + 0.2, ymin = avg_mod - sd_mod, ymax = avg_mod + sd_mod), color = "darkred",
                , width = 0.3) +
  theme_light() +
  scale_y_date("Harvesting Date", limits = c(as.Date("2007-10-01") , as.Date("2008-02-01")), date_labels = "%d-%b") +
  scale_x_continuous("Year", limits = c(2002.6, 2010.4),
                     breaks = 2003:2010) +
  scale_color_manual("", values = c("darkblue", "darkred")) +
  ggtitle("Observed vs PhenoRice Harvesting Dates") +
  theme(plot.title = element_text(family = "AvantGarde")) +
  facet_wrap(~site) + ylab("Harvesting Date") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ggplot2::theme(panel.grid.minor = element_line(color = "transparent"))

ggsave("/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Manuscript/Figures/Figure_5b.png",
       width = 10, height = 5, dpi = 600, plot = Figure_5b)

#boxplots alternatives
#
inpheno$sowdate_tmp <- doytodate(inpheno$sowdoy, 2007)
inphenobox <- inpheno %>%
  dplyr::select(Season = season, sowdate = sowdate_tmp, Year = year, site = site) %>%
  dplyr::mutate(Metric = "SOS") %>%
  dplyr::mutate(Season = "Wet") %>%
  dplyr::mutate(type   = "Observed") %>%
  dplyr::mutate(doy = datetodoy(sowdate))
inphenobox$site = factor(inphenobox$site, labels = c("Boundoum (delta)", "Podor (middle valley)"))

modpheno <- mod_data %>%
  dplyr::filter(Metric == "SOS") %>%
  dplyr::select(Season = Season, sowdate = value, Year = year, site = site, Metric) %>%
  dplyr::mutate(type = "PhenoRice") %>%
  dplyr::mutate(sowdate = doytodate(sowdate, 2007)) %>%
  dplyr::mutate(doy = datetodoy(sowdate))

sowdata_box <- rbind(inphenobox, modpheno) %>%
  dplyr::filter(Year > 2002 & Year <= 2010)

Figure_5a_box <- ggplot(sowdata_box, aes(y = sowdate, color = type)) +
  geom_boxplot(aes(x = factor(Year)), outlier.colour = "transparent",
               position = position_dodge(width = 0.9), width = 0.75) + facet_wrap(~site)  +
  theme_light() +
  scale_y_date(limits = c(as.Date("2007-06-01") , as.Date("2007-10-01")), date_labels = "%d-%b") +
  scale_color_manual("", values = c("darkblue", "darkred"))+
  theme(legend.position = c(0.1, 0.1)) +
  xlab("Year") + ylab("Sowing Date")

Figure_5a_box

ggsave("/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Manuscript/Figures/Figure_5a_box.png",
       width = 10, height = 5, dpi = 600, plot = Figure_5a_box)


inpheno$harvdate_tmp <- doytodate(inpheno$harvdoy, 2007)
inphenobox <- inpheno %>%
  dplyr::select(Season = season, sowdate = harvdate_tmp, Year = year, site = site) %>%
  dplyr::mutate(Metric = "EOS") %>%
  dplyr::mutate(Season = "Wet") %>%
  dplyr::mutate(type   = "Observed") %>%
  dplyr::mutate(doy = datetodoy(sowdate))
inphenobox$site = factor(inphenobox$site, labels = c("Boundoum (delta)", "Podor (middle valley)"))

modpheno <- mod_data %>%
  dplyr::filter(Metric == "EOS") %>%
  dplyr::select(Season = Season, sowdate = value, Year = year, site = site, Metric) %>%
  dplyr::mutate(type = "PhenoRice") %>%
  dplyr::mutate(sowdate = doytodate(sowdate, 2007)) %>%
  dplyr::mutate(doy = datetodoy(sowdate))  %>%
 dplyr::filter(sowdate > as.Date("2007-10-31"))

sowdata_box <- rbind(inphenobox, modpheno) %>%
  dplyr::filter(Year > 2002 & Year <= 2010)

Figure_5b_box <- ggplot(sowdata_box, aes(y = sowdate, color = type)) +
  geom_boxplot(aes(x = factor(Year)), outlier.colour = "transparent",
               position = position_dodge(width = 0.9), width = 0.75) + facet_wrap(~site)  +
  scale_y_date("Harvesting Date", limits = c(as.Date("2007-10-01") , as.Date("2008-02-01")), date_labels = "%d-%b") +
  theme_light() +
  scale_color_manual("", values = c("darkblue", "darkred")) +
  theme(legend.position = c(0.1, 0.1)) +
  theme(legend.position = c(0.1, 0.1)) +
  xlab("Year") + ylab("Sowing Date")


ggsave("/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Manuscript/Figures/Figure_5b_box.png",
       width = 10, height = 5, dpi = 600, plot = Figure_5b_box)

# pp <- ggplot(eos_valid_data, aes(x = factor(year), y = avg)) +
#   geom_point(aes(color = type), position = position_dodge(width = 0.6)) +
#   geom_errorbar(aes(color = type, ymin = avg - sd, ymax = avg + sd),
#                 position = position_dodge(width = 0.6)) +
#   theme_light() +
#   scale_y_date(limits = c(as.Date("2007-09-30") , as.Date("2008-01-30")), date_labels = "%d-%b") +
#   scale_color_manual("", values = c("darkblue", "darkred")) +
#   ggtitle("Observed vs PhenoRice Harvesting Dates") +
#   theme(plot.title = element_text(family = "AvantGarde")) +
#   facet_wrap(~site) +
#   xlab("Year") + ylab("Harvesting Date")
#
#
#
ggplot(sos_valid_data, aes(x = avg, y = avg_mod, color = site, label =  stringr::str_sub(year, 3,4))) +
  theme_light() +
  geom_point(pch = 24, show.legend = F) + geom_text(nudge_y = 4) +
  geom_smooth(method = "lm", linetype = 4, size = 0.8, show.legend = F) +
  geom_hline(yintercept = 0, colour = "grey70", size = 0.5) +
  geom_vline(xintercept = 0, colour = "grey70", size = 0.5) +
  scale_colour_manual(values = c("darkred", "darkgreen")) +
  ylab("PhenoRice Average Sowing Date") + xlab("Observed Average Sowing Date") +
  facet_wrap(~site, scales = "free_y") + geom_abline(slope = 1) +
  coord_cartesian(xlim = as.Date(c("2007-07-01", "2007-09-01")),
                  ylim = as.Date(c("2007-07-01", "2007-09-01"))) +
  theme(legend.position = "none")

ggplot(eos_valid_data, aes(x = avg, y = avg_mod, color = site, label =  year)) +
  theme_light() +
  geom_point(pch = 24, show.legend = F) + geom_text(nudge_y = 4) +
  geom_smooth(method = "lm", linetype = 4, size = 0.8, show.legend = F) +
  geom_hline(yintercept = 0, colour = "grey70", size = 0.5) +
  geom_vline(xintercept = 0, colour = "grey70", size = 0.5) +
  scale_colour_manual(values = c("darkred", "darkgreen")) +
  ylab("PhenoRice Average Harvesting Date") + xlab("Observed Average Harvesting Date") +
  facet_wrap(~site, scales = "free_y") + geom_abline(slope = 1) +
  coord_cartesian(xlim = as.Date(c("2007-10-15", "2008-01-15")),
                  ylim = as.Date(c("2007-10-15", "2008-01-15"))) +
  theme(legend.position = "none")




