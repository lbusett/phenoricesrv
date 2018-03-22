# FIgures areas all years
#
res_folder <- "/home/lb/nr_working/shared/PhenoRice/Processing/Senegal/Final/Outputs_MaskIrrig/"
setwd(res_folder)
in_files <- list.files(res_folder, pattern = ".dat$", recursive = T, full.names = T)
for (ff in seq_along(files))  {
  print(ff)
  file <- in_files[ff]
  in_rast <- read_rast(file)
  year <- basename(dirname(file))
  which_dry <- which(!is.na(getValues(in_rast[[2]])))
  which_wet <- which(!is.na(getValues(in_rast[[3]])))
  which_both <- intersect(which_dry, which_wet)
  out_rast <- raster(in_rast[[1]])
  out_rast[which_dry] = 1
  out_rast[which_wet] = 2
  out_rast[which_both] = 3

  fig_extent <- new("sprawlext", extent = c(-1756882, 1633301, -1292967, 1875359),
                    proj4string = get_proj4string(out_rast))
  sow_fish <- create_fishnet(out_rast, cellsize = 5000, shape = "hex")
  out_rast <- crop_rast(out_rast, fig_extent)

  aggr_data <- extract_rast(out_rast,
                                     sow_fish,
                                     full_data = TRUE)
  in_data <- aggr_data$alldata
  st_geometry(in_data) = NULL
  summ_data_16 <- in_data %>%
    dplyr::select(id_feat, value) %>%
    dplyr::filter(value > 0) %>%
    dplyr::count(id_feat, value) %>%
    tidyr::spread(value, n, fill = 0, sep = "_") %>%
    dplyr::rename_at(c(2:4), funs(c("dry","wet","both"))) %>%
    dplyr::left_join(aggr_data$stats) %>%
    dplyr::select_at(c(1:4, 7, 15)) %>%
    sf::st_as_sf()


  area_data_16 <- summ_data_16 %>%
    dplyr::mutate(area_dry = round(1e-04 * (dry * res(out_rast)[1]^2 + both * res(out_rast)[1]^2)),
                  area_wet = round(1e-04 *( wet * res(out_rast)[1]^2 + both * res(out_rast)[1]^2)),
                  area_both = round(1e-04 * (both * res(out_rast)[1]^2)),
                  area = as.numeric(area)) %>%
    dplyr::mutate(fc_dry = (area_dry / area ),
                  fc_wet = area_wet / area,
                  fc_both = area_both / area) %>%
    dplyr::mutate(sum = dry + wet + both) %>%
    dplyr::mutate(prop_dry = dry/sum,
                  prop_wet = wet/sum,
                  prop_both = both/sum) %>%
    mutate(color = rgb(prop_dry, prop_wet, prop_both)) %>%
    mutate(area = as.numeric(area)) %>%
    select(-sum) %>%
    sf::st_as_sf()

  river = read_vect("/home/lb/Downloads/ne_10m_rivers_lake_centerlines.shp")
  sen_borders <- get_boundaries("SEN", 2) %>%
    tibble::as_tibble() %>%
    sf::st_as_sf() %>%
    sf::st_transform(get_proj4string(out_rast))
  mau_borders <- get_boundaries("MRT", 2) %>%
    tibble::as_tibble() %>%
    sf::st_as_sf() %>%
    sf::st_transform(get_proj4string(out_rast))
  borders <- rbind(sen_borders, mau_borders) %.>%
    dplyr::mutate(.,
                  lon = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[1]]),
                  lat = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[2]])
    ) %>%
    sf::st_as_sf()

  pl = plot_vect(area_data_16, fill_var = "area_dry",
                 borders_layer = borders,
                 palette_name = "YlOrBr",
                 borders_txt_field = "NAME_2",
                 scale_transform = "log",
                 leg_breaks = c(1,10,100,1000),
                 na.color = "transparent",
                 leg_position = "bottom",
                 zlims = c(1,1000),
                 outliers_style = "to_minmax") +
    geom_sf(data = river, color = "blue") +
    coord_sf(xlim = c(-1756882, -1292967), ylim = c(1633301,1875359 ))
}
