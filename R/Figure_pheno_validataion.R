load("/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Datasets/statistics_modis/extract_rast_mask_ONLY_SEN.RData")
gd_folder  <- "/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/"

validation_delta <- read_vect(
  file.path(gd_folder, "Datasets/Pheno_Validation/validation_delta.shp")) %>%
  st_transform((proj4string(r))) %>%
  dplyr::select(Name)

validation_valley <- read_vect(
  file.path(gd_folder, "Datasets/Pheno_Validation/validation_valley.shp")) %>%
  st_transform((proj4string(r))) %>%
  dplyr::select(Name)

inpheno <- read.csv2("/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Datasets/Pheno_Validation/Pheno_Validation_data_arrondissements.csv", stringsAsFactors = TRUE)
inpheno <- as_tibble(inpheno) %>%
  dplyr::mutate(sowdate = as.Date(sowdate, format = "%d/%m/%Y"),
         harvdate =  as.Date(harvdate, format = "%d/%m/%Y")) %>%
  dplyr::filter(season == "Hivern") %>%
  dplyr::mutate(year = as.numeric(year)) %>%
  dplyr::mutate(sowdoy = datetodoy(sowdate), harvdoy = datetodoy(harvdate)) %>%
  dplyr::mutate(sowddate_tmp = doytodate(sowdoy, 2007), harvdate_tmp = doytodate(harvdoy, 2007)) %>%
  dplyr::mutate(village = factor(village))

valid_data <- inpheno %>%
  group_by(season, year, site) %>%
  summarize(sowdate = mean(sowddate_tmp, na.rm = T),
            harvdate = mean(harvdate_tmp, na.rm = T),
            sow_sd = sd(sowdoy, na.rm = T),
            harv_sd = sd(harvdoy, na.rm = T))

mod_data_delta <- data_delta %>%
  dplyr::filter(band_name %in% c("SOS_1", "SOS_2", "EOS_1", "EOS_2")) %>%
  dplyr::select(c(3, 5, 6, 8, 10)) %>%
  as.tibble() %>%
  separate(band_name, sep = "_", into = c("Metric", "Season")) %>%
  print()



avg_sow <- inpheno %>%
  group_by(year, Admin.area) %>%
  summarize(avgsow = mean(sowddate_tmp, na.rm = TRUE),
            sdsow  = sd(sowdoy, na.rm = TRUE),
            count = n()) %>%
  mutate(Admin.area = as.factor(Admin.area)) %>%
  filter(Admin.area != "") %>%
  droplevels().
levels(avg_sow$Admin.area)[3] = "Thille Boubacar"
names(avg_sow)[2] = "NAME_3"
