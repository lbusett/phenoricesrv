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


setwd("/home/lb/nr_working/shared/PhenoRice/Processing/Senegal/Final/Outputs_MaskIrrig/")

setwd("/home/lb/nr_working/shared/PhenoRice/Processing/Senegal/processing_Nov_17/mask_irrig/thresh_1000/outputs/")
in_folder  <- file.path(getwd())
out_folder <- "/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Datasets/statistics_modis"
gd_folder  <- "/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/"
# out_stats_file  <- file.path(out_folder, "Stats_Phenorice_srv.RData")
# out_stats_file_extract  <- file.path(out_folder,"extract_Phenorice_final_maskirrig.RData")

# extracting here all the data (not only on Senegal!!!!)
out_stats_file_extract  <- file.path(out_folder,"extract_Phenorice_final_maskirrig_newvalvalley_november_1000_valleybig_alldata.RData")

# "2003/Phenorice_out_2002_313_2004_129.dat"

# r = read_rast(file.path(in_folder, "2003/Phenorice_out_2002_321_2004_089.dat"))
r = read_rast("2003/Phenorice_out_2002_313_2004_129.dat")

years      <- paste0(seq(2003,2016,1),"/")
in_folders <- file.path(in_folder, years)

# use this to extract  only on Senegal!!!!! (i.e., for area comparison purposes)
sen_bounds <- get_boundaries("SEN", level = 0)


# use this to extract not only on Senegal!!!!!
#
# # sen_bounds <- read_vect("/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Datasets/SRV units.kmz") %>%
# #   dissolve_vect(dissolve_var = "description")
#

# sen_bounds <- read_vect("/home/lb/nr_working/shared/PhenoRice/Processing/Senegal/Final/ancillary/mask_srv.shp")


validation_delta <- read_vect(
  file.path(gd_folder, "Datasets/Pheno_Validation/validation_delta.shp")) %>%
  st_transform((proj4string(r))) %>%
  dplyr::select(Name)

validation_valley <- read_vect(
  file.path(gd_folder, "Datasets/Pheno_Validation/validation_valley.shp")) %>%
  st_transform((proj4string(r)))
# %>%
#   dplyr::select(id)



# summarize data of input raster ####
yy = 1
years <- seq(2003,2016,1)

data_delta_l  <- list()
data_valley_l <- list()
data_sen_l    <- list()

for (yy in 1:length(years)) {

  print(years[yy])
  in_file <- list.files(in_folders[[yy]], pattern = 'Phenorice_out?',
                        full.names = TRUE)[1]
  inrast  <- read_rast(in_file)
  # if (yy != 14) {
  #   names(inrast) <- c("N_seas", "SOS_1", "SOS_2", "FOS_1", "FOS_2", "EOS_1",
  #                      "EOS_2", "VGTCEVI_1", "VGTCEVI_2", "VGT_LG_1", "VGT_LG_2", "LG_1",
  #                      "LG_2")
  # } else {
    names(inrast) <- c("N_seas", "SOS_1", "SOS_2", "FOS_1", "FOS_2", "EOS_1",
                       "EOS_2", "CEVI_1", "CEVI_2", "VGTCEVI_1", "VGTCEVI_2",
                       "VGT_LG_1", "VGT_LG_2", "LG_1", "LG_2")
  # }
  NAvalue(inrast)   <- -999
  data_delta  <- extract_rast(inrast, validation_delta, join_geom = FALSE,
                              join_feat_tbl = F, full_data = TRUE, verbose = FALSE)
  data_valley <- extract_rast(inrast, validation_valley, join_geom = FALSE,
                              join_feat_tbl = F, full_data = TRUE, verbose = FALSE)
  data_sen    <- extract_rast(inrast, sen_bounds, join_geom = FALSE,
                              join_feat_tbl = F, full_data = TRUE, verbose = FALSE)
  data_delta$alldata$year  <- years[yy]
  data_delta$stats$year    <- years[yy]
  data_valley$alldata$year <- years[yy]
  data_valley$stats$year   <- years[yy]
  data_sen$alldata$year    <- years[yy]
  data_sen$stats$year      <- years[yy]

  data_delta_l[[yy]]  <- data_delta
  data_valley_l[[yy]] <- data_valley
  data_sen_l[[yy]]    <- data_sen

}

data_delta  <- data.table::rbindlist(do.call(c,lapply(data_delta_l, "[", 2)))
data_valley <- data.table::rbindlist(do.call(c,lapply(data_valley_l, "[", 2)))
data_sen    <- data.table::rbindlist(do.call(c,lapply(data_sen_l, "[", 2)))


save(data_delta,
     data_valley,
     data_sen,
     file = out_stats_file_extract)


# save(data_sen,
#      file = "/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Datasets/statistics_modis/extract_rast_mask_ONLY_SEN.RData")




# data_delta$band_name <- factor(data_delta$band_name)

load("/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Datasets/statistics_modis/extract_rast.RData")

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



in_files <- list.files(in_folder, pattern = '.dat$', full.names = TRUE,
                         recursive = T)
in_stack <- raster::stack(in_files)
extracted_data <- sprawl::extract_rast(in_stack, sen_bounds, join_geom = FALSE,
                                       join_feat_tbl = TRUE)


b =  a$n_pix_val * 233.656^2 / 10000
