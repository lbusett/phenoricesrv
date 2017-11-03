# Funcrion to extract Sowing dates data from full phenorice results
# + add the administrative boundaries codes and names to each point
# with a valid PhenoRice estimation

library(raster)
library(gdalUtils)
library(data.table)
library(dplyr)
library(sf)
library(mapview)
library(sprawl)

setwd("/home/lb/nr_working/shared/PhenoRice/Processing/Senegal/Final")

in_folder       <- file.path(getwd(), "/Outputs_MaskIrrig")
out_folder      <- "/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Datasets/statistics_modis"
gd_folder       <- "/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/"
out_stats_file  <- file.path(out_folder, "Stats_Phenorice_srv.RData")
out_stats_file_extract  <- file.path(out_folder,"extract_Phenorice_srv.RData")

r = read_rast(file.path(in_folder, "/2003/Phenorice_out_2002_321_2004_089.dat"))

years      <- paste0(seq(2003,2016,1),"/")
in_folders <- file.path(in_folder, years)

gadm_data_3 <- read_vect(file.path(gd_folder, "/Datasets/Ancillary/gadm/SEN_adm3.shp")) %>%
  st_transform((proj4string(r)))

gadm_data_4 <- read_vect(file.path(gd_folder, "Datasets/Ancillary/gadm/SEN_adm4.shp")) %>%
  st_transform((proj4string(r)))

validation_delta <- read_vect(file.path(gd_folder, "Datasets/Pheno_Validation/validation_delta.shp")) %>%
  st_transform((proj4string(r))) %>%
  st_as_sf()

validation_valley <- read_vect(file.path(gd_folder, "Datasets/Pheno_Validation/validation_valley.shp")) %>%
  st_transform((proj4string(r))) %>%
  st_as_sf()
#
# validation_delta = readOGR("/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Datasets/Pheno_Validation/", "validation_delta") %>%
#   st_as_sf() %>%
#   st_transform((proj4string(r)))


validation_valley = read_vect("/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Datasets/Pheno_Validation/", "validation_valley") %>%
  st_as_sf() %>%
  st_transform((proj4string(r)))

# gadm_data_3 <- st_transform(gadm_data, (proj4string(r)))
# for (i in 1:93) xmins[[i]] = st_bbox(gadm_data[i,])[1]
# for (i in 1:93) ymaxs[[i]] = st_bbox(gadm_data[i,])[4]
# gadm_data$xmin = as.numeric(xmins)
# gadm_data$ymax = as.numeric(ymaxs)
gadm_data_3  = as(gadm_data_3, "Spatial")
gadm_data_4  = as(gadm_data_4, "Spatial")
validation_delta = as(st_zm(validation_delta), "Spatial")
validation_valley = as(st_zm(validation_valley), "Spatial")

gadm_raster3   = rasterize(gadm_data_3, r, field = "ID_3")
gadm_raster4   = rasterize(gadm_data_4, r, field = "ID_4")
valid_raster   = rasterize(validation_delta,  r, field = "Name")
valley_raster  = rasterize(validation_valley, r, field = "Name")

yy = 1
years <- seq(2003,2016,1)
stats <- list()

for (yy in 1:length(years)) {

  print(years[yy])
  in_file    <- list.files(in_folders[[yy]], pattern = 'Phenorice_out?', full.names = TRUE)[1]
  inrast     <- stack(in_file)
  nseasons   <- getValues(inrast[[1]])
  okpixs     <- which(nseasons > 0)

  stats[[yy]] <- data.frame(year         <- years[yy],

                            nseasons      <- nseasons[okpixs],
                            sowdates_dry  <- getValues(inrast[[2]])[okpixs],
                            sowdates_wet  <- getValues(inrast[[3]])[okpixs],

                            flowdates_dry <- getValues(inrast[[4]])[okpixs],
                            flowdates_wet <- getValues(inrast[[5]])[okpixs],

                            harvdates_dry <- getValues(inrast[[6]])[okpixs],
                            harvdates_wet <- getValues(inrast[[7]])[okpixs],

                            cumEVI_dry    <- getValues(inrast[[8]])[okpixs],
                            cumEVI_wet    <- getValues(inrast[[9]])[okpixs],

                            lgtveg_dry    <- getValues(inrast[[4]])[okpixs] - getValues(inrast[[2]])[okpixs],
                            lgtveg_wet    <- getValues(inrast[[5]])[okpixs] - getValues(inrast[[2]])[okpixs],

                            lgttot_dry    <- getValues(inrast[[6]])[okpixs] - getValues(inrast[[2]])[okpixs],
                            lgttot_wet    <- getValues(inrast[[7]])[okpixs] - getValues(inrast[[3]])[okpixs],

                            xcoord        <- xyFromCell(inrast,okpixs)[,1],
                            ycoord        <- xyFromCell(inrast,okpixs)[,2],
                            ID_3          <- getValues(gadm_raster3)[okpixs],
                            ID_4          <- getValues(gadm_raster4)[okpixs],
                            valid_delta   <- getValues(valid_raster)[okpixs],
                            valid_valley  <- getValues(valley_raster)[okpixs]
  )
}

statsdt <- rbindlist(stats)
names(statsdt) <- c("year", "nseasons", "sowdoy_dry", "sowdoy_wet", "flowdoy_dry", "flowdoy_wet",
                    "harvdoy_dry", "harvdoy_wet", "cumEVI_dry", "cumEVI_wet", "lgtveg_dry", "lgtveg_wet",
                    "lgttot_dry", "lgttot_wet", "lon", "lat", "ID_3", "ID_4", "valid_delta", "valid_valley")

statsdt <- statsdt %>%
  as_tibble() %>%
  mutate(year            = as.character(year),
         nseasons        = as.character(nseasons),
         ID_3            = as.numeric(ID_3),
         sowdate_dry     = doytodate(sowdoy_dry, year),
         sowdate_wet     = doytodate(sowdoy_wet, year),
         flowdate_dry    = doytodate(flowdoy_dry, year),
         flowdate_wet    = doytodate(flowdoy_wet, year),
         harvdate_dry    = doytodate(harvdoy_dry, year),
         harvdate_wet    = doytodate(harvdoy_wet, year),
         sowddate_dry_t  = doytodate(sowdoy_dry, 2007),
         sowdate_wet_t   = doytodate(sowdoy_wet, 2007),
         flowdate_dry_t  = doytodate(flowdoy_dry, 2007),
         flowdate_wet_t  = doytodate(flowdoy_wet, 2007),
         harvdate_dry_t  = doytodate(harvdoy_dry, 2007),
         harvdate_wet_t  = doytodate(harvdoy_wet, 2007)
  ) %>%
  left_join(gadm_data_4@data, by = "ID_4") %>%
  filter(!is.na(ID_3.y)) %>%
  filter(!is.na(ID_4)) %>%
  select(-c(ISO, NAME_0, ID_0, ID_1, NAME_1, ID_2, ID_3.y, NAME_2, VARNAME_4, CCN_4, CCA_4, TYPE_4, ENGTYPE_4)) %>%
  arrange(year, NAME_4) %>%
  mutate(NAME_3 = as.factor(NAME_3), NAME_4 = as.factor(NAME_4), year = as.factor(year), nseasons = as.factor(nseasons))

statsdt_melt <- statsdt %>%
  gather(variable, value, c(3:14, 19:32)) %>%
  filter(!is.na(value))
  dir.create(dirname(out_stats_file))

 save(stats, file = out_stats_file_extract)
 save(statsdt, statsdt_melt,  file = out_stats_file)

