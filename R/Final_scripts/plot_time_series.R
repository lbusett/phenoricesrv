# plot vi time series
get_data <- function() {

  # function to get the time series data for 2003 and 2016 and put everything
  # togheter
  #
  in_folder <- "/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Datasets/statistics_modis/Final/time_series/"

  # Any data ----
  get(load(file.path(in_folder, "2003_any_ts.RData" )))
  any_evi_2003 <- any_data$alldata %>%
    dplyr::mutate(year = 2003, type = "any")
  any_ndfi_2003 <- any_ndfi$alldata %>%
    dplyr::mutate(year = 2003, type = "any")
  any_doys_2003 <- any_doys$alldata %>%
    dplyr::mutate(year = 2003, type = "any")

  get(load(file.path(in_folder, "2016_any_ts.RData" )))
  any_evi_2016 <- any_data$alldata %>%
    dplyr::mutate(year = 2016, type = "any")
  any_ndfi_2016 <- any_ndfi$alldata %>%
    dplyr::mutate(year = 2016, type = "any")
  any_doys_2016 <- any_doys$alldata %>%
    dplyr::mutate(year = 2016, type = "any")

  # both data ----
  get(load(file.path(in_folder, "2003_both_ts.RData" )))
  both_evi_2003 <- both_data$alldata %>%
    dplyr::mutate(year = 2003, type = "both")
  both_ndfi_2003 <- both_ndfi$alldata %>%
    dplyr::mutate(year = 2003, type = "both")
  both_doys_2003 <- both_doys$alldata %>%
    dplyr::mutate(year = 2003, type = "both")

  get(load(file.path(in_folder, "2016_both_ts.RData" )))
  both_evi_2016 <- both_data$alldata %>%
    dplyr::mutate(year = 2016, type = "both")
  both_ndfi_2016 <- both_ndfi$alldata %>%
    dplyr::mutate(year = 2016, type = "both")
  both_doys_2016 <- both_doys$alldata %>%
    dplyr::mutate(year = 2016, type = "both")

  # wet data ----
  get(load(file.path(in_folder, "2003_wet_ts.RData" )))
  wet_evi_2003 <- wet_data$alldata %>%
    dplyr::mutate(year = 2003, type = "wet")
  wet_ndfi_2003 <- wet_ndfi$alldata %>%
    dplyr::mutate(year = 2003, type = "wet")
  wet_doys_2003 <- wet_doys$alldata %>%
    dplyr::mutate(year = 2003, type = "wet")

  get(load(file.path(in_folder, "2016_wet_ts.RData" )))
  wet_evi_2016 <- wet_data$alldata %>%
    dplyr::mutate(year = 2016, type = "wet")
  wet_ndfi_2016 <- wet_ndfi$alldata %>%
    dplyr::mutate(year = 2016, type = "wet")
  wet_doys_2016 <- wet_doys$alldata %>%
    dplyr::mutate(year = 2016, type = "wet")

  # dry data ----
  get(load(file.path(in_folder, "2003_dry_ts.RData" )))
  dry_evi_2003 <- dry_data$alldata %>%
    dplyr::mutate(year = 2003, type = "dry")
  dry_ndfi_2003 <- dry_ndfi$alldata %>%
    dplyr::mutate(year = 2003, type = "dry")
  dry_doys_2003 <- dry_doys$alldata %>%
    dplyr::mutate(year = 2003, type = "dry")

  get(load(file.path(in_folder, "2016_dry_ts.RData" )))
  dry_evi_2016 <- dry_data$alldata %>%
    dplyr::mutate(year = 2016, type = "dry")
  dry_ndfi_2016 <- dry_ndfi$alldata %>%
    dplyr::mutate(year = 2016, type = "dry")
  dry_doys_2016 <- dry_doys$alldata %>%
    dplyr::mutate(year = 2016, type = "dry")

  # Put everything togheter
  #
  evi_data <- rbind(any_evi_2003, any_evi_2016,
                    dry_evi_2003, dry_evi_2016,
                    wet_evi_2003, wet_evi_2016,
                    both_evi_2003, both_evi_2016)

  ndfi_data <- rbind(any_ndfi_2003, any_ndfi_2016,
                     dry_ndfi_2003, dry_ndfi_2016,
                     wet_ndfi_2003, wet_ndfi_2016,
                     both_ndfi_2003, both_ndfi_2016)

  doys_data <- rbind(any_doys_2003, any_doys_2016,
                     dry_doys_2003, dry_doys_2016,
                     wet_doys_2003, wet_doys_2016,
                     both_doys_2003, both_doys_2016)

  save(evi_data, ndfi_data, doys_data,
       file = "/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Datasets/statistics_modis/Final/time_series/alldata.RData")
}

get(load("/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Datasets/statistics_modis/Final/time_series/alldata.RData"))


delta_shape <- file.path("/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Datasets/Pheno_Validation/validation_delta.shp") %>%
  st_read()

data_evi <- evi_data %>%
  sf::st_intersection(st_transform(delta_shape, get_proj4string(evi_data)))

stats_evi <- data_evi %>%
  group_by(date, type, year) %>%
  summarize(avg = mean(value, na.rm = TRUE),
            sd  = sd(value, na.rm = TRUE),
            fifth = quantile(value, probs = 0.05),
            ninetyfifth = quantile(value, probs = 0.95))

data_doys <- doys_data %>%
  sf::st_intersection(st_transform(delta_shape, get_proj4string(doys_data)))

stats_doy <- data_doys %>%
  group_by(band_n, band_name, type, year) %>%
  summarize(avg = mean(value, na.rm = TRUE),
            sd  = sd(value, na.rm = TRUE),
            fifth = quantile(value, probs = 0.05),
            ninetyfifth = quantile(value, probs = 0.95)) %>%
  ungroup()

data_ndfi <- ndfi_data %>%
  sf::st_intersection(st_transform(delta_shape, get_proj4string(doys_data)))

stats_ndfi <- data_ndfi %>%
  group_by(date, type, year) %>%
  summarize(avg = mean(value, na.rm = TRUE),
            sd  = sd(value, na.rm = TRUE),
            fifth = quantile(value, probs = 0.05),
            ninetyfifth = quantile(value, probs = 0.95)) %>%
  ungroup()

doy_toplot <- dplyr::filter(stats_doy, band_n %in% c(2,3,4,5,6,7)) %>%
  mutate(date = doytodate(avg, 2016))

stats_evi$year <- as.factor(stats_evi$year)
stats_ndfi$year <- as.factor(stats_ndfi$year)
doy_toplot$year <- as.factor(doy_toplot$year)
ggplot(stats_evi) + theme_light() +
  geom_point(aes (x = date, y = avg, color = year, group = year)) +
  geom_line(aes (x = date, y = avg, color = year, group = year)) +
  geom_line(aes(x = date, y = fifth, group = year), color = "grey75") +
  geom_line(aes(x = date, y = ninetyfifth, group = year), color = "grey75") +
  scale_x_date(limits = as.Date(c("2016-01-01", "2017-02-01")), date_breaks = "1 month",
               date_labels = "%b") +
  geom_vline(data = doy_toplot, aes(xintercept = date, color = band_name,
                                    linetype = year)) +
  geom_line(data = stats_ndfi, aes(x = date, color = year, y = avg, group = year)) +
  facet_wrap(~type)


test_profs <- test$alldata %>%
  dplyr::filter(id_feat == 120 & N == 2) %>%
  dplyr::mutate(date = doytodate(313, 2015) + 8 * (1:nlayers(stack(in_smoothrast)) - 1))


aa <- both_data$alldata %>% filter(id_feat == 10 & N == 1)
bb <- both_doys$stats %>% filter(id_feat == 10 & band_n %in% c(2,4,6) & id == 1) %>%
  mutate(sowdate = doytodate(avg, 2016))

ggplot(aa, aes(x = date, y =  avg)) +
  geom_line() + geom_point() + theme_light() +
  geom_line(aes(y =  max)) +
  geom_line(aes(y =  min)) +
  scale_x_date(limits = as.Date(c("2015-11-01", "2016-11-01"))) +
  geom_vline(data = bb, aes(xintercept = sowdate), color = "red", linetype = 2)


aa <- both_data$alldata %>% filter(id_feat == 656 & N == 8)
bb <- both_doys$alldata %>% filter(id_feat == 656 & band_n %in% c(2,4,6) & N == 8) %>%
  mutate(sowdate = doytodate(value, 2016))


aa <- wet_data$alldata %>% filter(id_feat == 100  & N == 1)
bb <- wet_doys$alldata %>% filter(id_feat == 100 & band_n %in% c(3,5,7) & N == 1) %>%
  mutate(sowdate = doytodate(value - 8, 2016) )

ggplot(aa, aes(x = date, y =  value)) +
  geom_line() + geom_point() + theme_light() +
  # geom_line(aes(y =  max)) +
  # geom_line(aes(y =  min)) +
  # scale_x_date(limits = as.Date(c("2015-10-01", "2016-10-01"))) +
  geom_vline(data = bb, aes(xintercept = sowdate), color = "red", linetype = 2)


aa <- wet_data$stats %>% filter(id_feat == 100)
bb <- wet_doys$stats %>% filter(id_feat == 100 & band_n %in% c(3,5,7)) %>%
  mutate(sowdate = doytodate(avg, 2016))

ggplot(aa, aes(x = date, y =  avg)) +
  geom_line() + geom_point() + theme_light() +
  geom_line(aes(y =  max)) +
  geom_line(aes(y =  min)) +
  # scale_x_date(limits = as.Date(c("2015-11-01", "2016-11-01"))) +
  geom_vline(data = bb, aes(xintercept = sowdate), color = "red", linetype = 2)

aa <- both_data$stats %>% filter(id_feat == 190)
bb <- both_doys$stats %>% filter(id_feat == 190 & band_n %in% c(2,3,4,5,6,7)) %>%
  mutate(sowdate = doytodate(avg-8, 2016))

ggplot(aa) +
  geom_line(aes(x = date, y =  avg + sd)) +
  geom_line(aes(x = date, y =  avg)) + geom_point(aes(x = date, y =  avg)) + theme_light() +
  geom_line(aes(x = date, y =  avg - sd)) +
  # scale_x_date(limits = as.Date(c("2015-11-01", "2016-11-01"))) +
  geom_vline(data = bb, aes(xintercept = sowdate), color = "red", linetype = 2)


aa <- any_data$stats %>% filter(id_feat == 729)
bb <- any_doys$stats %>% filter(id_feat == 729 & band_n %in% c(2,3,4,5,6,7)) %>%
  mutate(sowdate = doytodate(avg-8, 2016))
cc <- any_ndfi$stats %>% filter(id_feat == 729) %>%
  mutate(date = dates_ndfi_files$date[NDFI_dates])

ggplot(aa) +
  geom_line(aes(x = date, y =  max)) +
  geom_line(aes(x = date, y =  avg)) + geom_point(aes(x = date, y =  avg)) + theme_light() +
  geom_line(aes(x = date, y =  min)) +
  scale_x_date(limits = as.Date(c("2016-01-01", "2017-01-01")), date_breaks = "1 month",
               date_labels = "%b") +
  geom_vline(data = bb, aes(xintercept = sowdate,  color = band_name), linetype = 2) +
  geom_line(data = cc, aes(x = date, y =  avg), color = "blue", linetype = 2)   +
  scale_y_continuous("Index") + coord_cartesian(ylim = c(-1500, 6000))

ggplot(aa, aes(x = date, y =  value)) +
  geom_line() + geom_point() + theme_light() +
  # geom_line(aes(y =  max)) +
  # geom_line(aes(y =  min)) +
  scale_x_date(limits = as.Date(c("2016-01-01", "2017-01-01"))) +
  geom_vline(data = bb, aes(xintercept = sowdate), color = "red", linetype = 2)



validation_delta <- read_vect(
  file.path(gd_folder, "Datasets/Pheno_Validation/validation_delta.shp")) %>%
  st_transform((proj4string(r))) %>%
  dplyr::select(Name)