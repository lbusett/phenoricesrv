library(raster)
library(fasterize)
library(dplyr)
library(sprawl)
library(sf)

in_folder  <- "/home/lb/nr_working/shared/PhenoRice/Processing/Senegal/Final/"
out_folder <- "/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Datasets/statistics_modis/Final/time_series/"
dir.create(out_folder, recursive = TRUE)
years      <- paste0(seq(2003,2016,1),"/")
in_folders <- file.path(in_folder, years)

# Open a EVI smoothed file
year <- 2016
in_smoothrast <- list.files(file.path(in_folder, "/mask_srv/inputs/", year, "VI_Smoothed/"),
                            pattern = "\\.dat$",
                            full.names = TRUE)[2] %>%
  raster::stack() %>%
  setZ(doytodate(313, 2015) + 8 * (1:nlayers(stack(in_smoothrast))))

# create NDFI rasterStack with the corresponding dates
in_ndfi_fold <- "/home/lb/projects/ermes/datasets/rs_products/MODIS/WA/VI_16Days_250m_v5/NDFI/"
ndfi_files   <- list.files(in_ndfi_fold, pattern = "\\.dat$", full.names = TRUE)
dates_ndfi_files <- data.frame(file = basename(ndfi_files)) %>%
  mutate(doy  = as.numeric(stringr::str_sub(file, 19 ,21)),
         year = as.numeric(stringr::str_sub(file, 14 ,17))) %>%
  mutate(date = as.Date(doy - 1, origin = paste0(year, "-01-01")))

NDFI_dates <- which(dates_ndfi_files$date >= range(getZ(in_smoothrast))[1] &
      dates_ndfi_files$date <= range(getZ(in_smoothrast))[2])

vrtfile <- tempfile(fileext = ".vrt")
gdalUtils::gdalbuildvrt(ndfi_files[NDFI_dates], vrtfile,
                        te = sprawl::get_extent(in_smoothrast)@extent,
                        separate = T)
in_ndfi_stack <- raster::stack(vrtfile) %>%
  raster::setZ(dates_ndfi_files$date[NDFI_dates])

# Open PhenoRice results file
results_folder <- file.path(in_folder, "lgtthresh_noshape/outputs/", year)
in_pheno_res   <- raster::stack(list.files(results_folder, full.names = TRUE, pattern = "\\.dat$"))

# find pixels with detection in dry, wet, both or any seasons. These are lately
# used to extract the time series from the smoothed EVI and from NDFI

which_dry  <- which(!is.na(getValues(in_pheno_res[[2]])) & is.na(getValues(in_pheno_res[[3]])))
which_wet  <- which(!is.na(getValues(in_pheno_res[[3]])) & is.na(getValues(in_pheno_res[[2]])))
which_both <- which(!is.na(getValues(in_pheno_res[[2]])) & !is.na(getValues(in_pheno_res[[3]])))
which_any  <- which(!is.na(getValues(in_pheno_res[[2]])) | !is.na(getValues(in_pheno_res[[3]])))

# get data for pixels with detection only in dry season ----
drytemp <- raster(in_pheno_res)
drytemp[which_dry] <- 1
outshp <- tempfile(fileext = ".shp")
dry_shape <- gdal_polygonizeR(drytemp, outshp, overwrite = T)
rm(drytemp)
gc()
dry_shape <- dry_shape %>%
  dplyr::filter(id == 1)
dry_data <- extract_rast(in_smoothrast, dry_shape, full_data = TRUE, parallel = FALSE, comp_freq = T)
dry_doys <- extract_rast(in_pheno_res, dry_shape, full_data = TRUE, parallel = FALSE, comp_freq = T)
dry_ndfi <- extract_rast(in_ndfi_stack, dry_shape, full_data = TRUE, parallel = FALSE, comp_freq = T)
gc()
rm(dry_shape)
save(dry_data, dry_doys, dry_ndfi,
     file = file.path(out_folder, paste0(year, "_dry_ts.RData")))
rm(dry_data, dry_doys, dry_ndfi)
gc()

# get data for pixels with detection only in wet season ----
wettemp <- raster(in_pheno_res)
wettemp[which_wet] <- 1
outshp <- tempfile(fileext = ".shp")
wet_shape <- gdal_polygonizeR(wettemp, outshp, overwrite = T)
rm(wettemp)
gc()
wet_shape <- wet_shape %>%
  dplyr::filter(id == 1)
wet_data <- extract_rast(in_smoothrast, wet_shape, full_data = TRUE, parallel = FALSE, comp_freq = T)
wet_doys <- extract_rast(in_pheno_res, wet_shape, full_data = TRUE, parallel = FALSE, comp_freq = T)
wet_ndfi <- extract_rast(in_ndfi_stack, wet_shape, full_data = TRUE, parallel = FALSE, comp_freq = T)
rm(wet_shape)
gc()
save(wet_data, wet_doys, wet_ndfi,
     file = file.path(out_folder, paste0(year, "_wet_ts.RData")))
rm(wet_data, wet_doys, wet_ndfi)
gc()

# get data for pixels with detection in both seasons
bothtemp <- raster(in_pheno_res)
bothtemp[which_both] <- 1
outshp <- tempfile(fileext = ".shp")
both_shape <- gdal_polygonizeR(bothtemp, outshp, overwrite = T)
rm(bothtemp)
gc()
both_shape <- both_shape %>%
  dplyr::filter(id == 1)
both_data <- extract_rast(in_smoothrast, both_shape, full_data = TRUE, parallel = FALSE, comp_freq = T)
both_doys <- extract_rast(in_pheno_res, both_shape, full_data = TRUE, parallel = FALSE, comp_freq = T)
both_ndfi <- extract_rast(in_ndfi_stack, both_shape, full_data = TRUE, parallel = FALSE, comp_freq = T)
rm(both_shape)
gc()
save(both_data, both_doys, both_ndfi,
     file = file.path(out_folder, paste0(year, "_both_ts.RData")))
rm(both_data, both_doys, both_ndfi)
gc()

# get data for pixels with detection in any seasons
anytemp <- raster(in_pheno_res)
anytemp[which_any] <- 1
outshp <- tempfile(fileext = ".shp")
any_shape <- gdal_polygonizeR(anytemp, outshp, overwrite = T)s
rm(anytemp)
gc()
any_shape <- any_shape %>%
  dplyr::filter(id == 1)
any_data <- extract_rast(in_smoothrast, any_shape, full_data = TRUE, parallel = FALSE, comp_freq = T)
any_doys <- extract_rast(in_pheno_res, any_shape, full_data = TRUE, parallel = FALSE, comp_freq = T)
any_ndfi <- extract_rast(in_ndfi_stack, any_shape, full_data = TRUE, parallel = FALSE, comp_freq = T)
rm(any_shape)
gc()
save(any_data, any_doys, any_ndfi,
     file = file.path(out_folder, paste0(year, "_any_ts.RData")))
rm(any_data, any_doys, any_ndfi)
gc()


