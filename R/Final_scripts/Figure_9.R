#Figure_histograms
#
#
# res_folder <- "/home/lb/nr_working/shared/PhenoRice/Processing/Senegal/Final/mask_irrig/thresh_1000/outputs/"
# res_folder <- "/home/lb/nr_working/shared/PhenoRice/Processing/Senegal/processing_Nov_17/mask_irrig/thresh_1000_16days/shortseas/outputs/"

res_folder <- "/home/lb/nr_working/shared/PhenoRice/Processing/Senegal/Final/lgtthresh_noshape/outputs/"
setwd(res_folder)
in_files <- list.files(res_folder, pattern = ".dat$", recursive = T, full.names = T)

years <- (2003:2016)
data_all = list()
for (ff in seq_along(in_files)) {

  file <- in_files[ff]
  in_rast <- read_rast(file)
  data_dry <- as.numeric(na.omit(getValues(in_rast[[2]])))
  data_wet <- as.numeric(na.omit(getValues(in_rast[[3]])))
  data_all[[ff]] <- data.frame(year = years[ff], doys = c(data_dry, data_wet))

}

data_all_df <- data.table::rbindlist(data_all) %>%
  dplyr::mutate(year = as.factor(year)) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(date = doytodate(doys, 2000))

Figure_9a <- ggplot(data_all_df, aes(x = date)) +   theme_light() +
  geom_histogram(aes(y =100*(..count.. / (sapply(PANEL, FUN=function(x) sum(count[PANEL == x]))))),
                 binwidth = 8, fill = "grey75", color = "black") +
  facet_wrap(~year, scales = "free_x", ncol = 3) +
  ylab("Frequency [%]") + xlab("Sowing Date" ) +
  scale_x_date(breaks = scales::date_breaks('1 month'), labels = scales::date_format("%b"),
               limits = doytodate(c(1,365), 2000)) +
  theme(axis.text.x = element_text(angle = 45, hjust  = 1 , vjust = 1))

ggsave(filename = "/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Manuscript/Figures/Figure_9a.png",
       dpi = 400, height = 8, width = 8 , plot = Figure_9a)


#test ridges ----
#
# pl +
#   theme(plot.title = element_text(family = "AvantGarde")) +
#   ggtitle("")
#
# pl <- ggplot(data_all_df, aes(x = date, y = year, fill = year)) +
#   geom_density_ridges(na.rm = T, bandwidth = 8, panel_scaling = T) +
#   theme_ridges() +
#   scale_fill_cyclical(values = c("grey75", "grey50")) + xlab("Sowing Date") +
#   scale_x_date(limits = as.Date(c("2000-01-01", "2001-01-01")), date_breaks = "2 months",
#                date_labels = "%b") + ylab("Year") +
#   ggtitle("Density Distribution of Estimated Sowing Dates")
# pl

# Harvesting dates ----
#
data_all = list()
for (ff in seq_along(in_files)) {

  file <- in_files[ff]
  in_rast <- read_rast(file)
  data_dry <- as.numeric(na.omit(getValues(in_rast[[6]])))
  data_wet <- as.numeric(na.omit(getValues(in_rast[[7]])))
  data_all[[ff]] <- data.frame(year = years[ff], doys = c(data_dry, data_wet))

}

data_all_df <- data.table::rbindlist(data_all) %>%
  dplyr::mutate(year = as.factor(year)) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(date = doytodate(doys, 2000))

Figure_9b <- ggplot(data_all_df, aes(x = date)) +   theme_light() +
  geom_histogram(aes(y = 100*(..count.. / (sapply(PANEL, FUN=function(x) sum(count[PANEL == x]))))),
                 binwidth = 8, fill = "grey75", color = "black") +
  facet_wrap(~year, scales = "free_x", ncol = 3) +
  ylab("Frequency [%]") + xlab("Harvesting Date" ) +
  scale_x_date(breaks = scales::date_breaks('1 month'), labels = scales::date_format("%b"),
               limits = doytodate(c(1,365), 2000) + 120) +
  theme(axis.text.x = element_text(angle = 45, hjust  = 1 , vjust = 1))

ggsave(filename = "/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Manuscript/Figures/Figure_9b.png",
       dpi = 400, height = 8, width = 8 , plot = Figure_9b)


# test ridges ----
# pl +
#   theme(plot.title = element_text(family = "AvantGarde")) +
#   ggtitle("")
#
# pl <- ggplot(data_all_df, aes(x = date, y = year, fill = year)) +
#   geom_density_ridges(na.rm = T, bandwidth = 8, panel_scaling = T) +
#   theme_ridges() +
#   scale_fill_cyclical(values = c("grey75", "grey50")) + xlab("Harvesting Date") +
#   scale_x_date(limits = as.Date(c("2000-04-01", "2001-04-01")), date_breaks = "2 months",
#                date_labels = "%b") + ylab("Year") +
#   ggtitle("Density Distribution of Estimated Harvesting Dates")
# pl


# Season length ----
#
data_all = list()
for (ff in seq_along(in_files)) {

  file <- in_files[ff]
  in_rast <- read_rast(file)
  # data_dry <- as.numeric(na.omit(getValues(in_rast[[14]])))
  data_wet <- as.numeric(na.omit(getValues(in_rast[[15]])))
  data_all[[ff]] <- data.frame(year = years[ff], doys = data_wet)

}

data_all_df <- data.table::rbindlist(data_all) %>%
  dplyr::mutate(year = as.factor(year)) %>%
  tibble::as_tibble()

# Figure_9c <- ggplot(data_all_df, aes(x = doys)) +   theme_light() +
#   geom_histogram(aes(y = 100*(..count.. / (sapply(PANEL, FUN=function(x) sum(count[PANEL == x]))))),
#                  binwidth = 8, fill = "grey75", color = "black") +
#   facet_wrap(~year, ncol = 3) +
#   ylab("Frequency [%]") + xlab("Harvesting Date" ) +
#   theme(axis.text.x = element_text(angle = 45, hjust  = 1 , vjust = 1))

pl <- ggplot(data_all_df, aes(x = doys, y = year, fill = year)) +
  geom_density_ridges(na.rm = T, bandwidth = 5, panel_scaling = F) +
  theme_ridges() + xlim(50,200)


+ facet_wrap()
  scale_fill_cyclical(values = c("grey75", "grey50")) + xlab("Harvesting Date") +
  scale_x_date(limits = as.Date(c("2000-04-01", "2001-04-01")), date_breaks = "2 months",
               date_labels = "%b") + ylab("Year") +
  ggtitle("Density Distribution of Estimated Harvesting Dates")
