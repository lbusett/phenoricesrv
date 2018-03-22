# For Figure_8
library(raster)
library(sprawl)
library(ggspatial)
library(gridExtra)
library(magrittr)
library(tidyr)
library(data.table)
library(sf)


# Get inputs
res_folder <- "/home/lb/nr_working/shared/PhenoRice/Processing/Senegal/Final/lgtthresh_noshape/outputs/"
setwd(res_folder)
files <- list.files(res_folder, pattern = ".dat$", recursive = T, full.names = T)

# Get data for 2003 and 2016

in_2003 <- raster::stack("2003/Phenorice_out_2002_313_2004_129.dat")
in_2016 <- raster::stack("2016/Phenorice_out_2015_313_2017_129.dat")

# Reshuffle phenorice outputs to get info on wheter a pixel in a year was
# rice in the Wet (1), the Dry (2) or Both(3) seasons

which_dry_2003 <- which(!is.na(getValues(in_2003[[2]])))
which_wet_2003 <- which(!is.na(getValues(in_2003[[3]])))

which_dry_2016 <- which(!is.na(getValues(in_2016[[2]])))
which_wet_2016 <- which(!is.na(getValues(in_2016[[3]])))

which_both_2003 <- which(!is.na(getValues(in_2003[[2]])) & !is.na(getValues(in_2003[[3]])))
which_both_2016 <- which(!is.na(getValues(in_2016[[2]])) & !is.na(getValues(in_2016[[3]])))

out_rast_2003 <- out_rast_2016 <- raster(in_2003)

out_rast_2003[which_dry_2003] = 1
out_rast_2003[which_wet_2003] = 2
out_rast_2003[which_both_2003] = 3

out_rast_2016[which_dry_2016] = 1
out_rast_2016[which_wet_2016] = 2
out_rast_2016[which_both_2016] = 3

a = "PhenoRice Results"
out_2016 <- raster::stack(out_rast_2016)
out_2003 <- raster::stack(out_rast_2003)
names(out_2016) = c("PhenoRice_2016")
names(out_2003) = c("PhenoRice_2003")


# define extent for the figure and crop on it
fig_extent <- new("sprawlext", extent = c(-1756882, 1633301, -1292967, 1875359),
                  proj4string = get_proj4string(out_2016)) %>%
  as("SpatialPolygons") %>% st_as_sf()

out_2016 <- crop_rast(out_2016, fig_extent)
out_2003 <- crop_rast(out_2003, fig_extent)

# create the fishnet
sow_fish <- create_fishnet(out_2016, cellsize = 5000, shape = "hex")

# extract info on the cells
aggr_data_nseas_16 <- extract_rast(out_2016,
                                   sow_fish,
                                   full_data = TRUE)

aggr_data_nseas_03 <- extract_rast(out_2003,
                                   sow_fish,
                                   full_data = TRUE)

# reshuffle data for plotting - 2016
in_data_16 <- aggr_data_nseas_16$alldata
st_geometry(in_data_16) <- NULL

summ_data_16 <- in_data_16 %>%
  dplyr::select(id_feat, value) %>%
  dplyr::filter(value > 0) %>%
  dplyr::count(id_feat, value) %>%
  tidyr::spread(value, n, fill = 0, sep = "_") %>%
  dplyr::rename_at(c(2:4), funs(c("dry","wet","both"))) %>%
  dplyr::left_join(aggr_data_nseas_16$stats) %>%
  sf::st_as_sf() %>%
  dplyr::select_at(c(1:4, 7)) %>%
  dplyr::mutate(area = (dry + wet + both) * res(out_2016)[1]^2)

area_data_16 <- summ_data_16 %>%
  dplyr::mutate(area_dry  = dry * res(out_2016)[1]^2 + both * res(out_2016)[1]^2,
                area_wet  = wet * res(out_2016)[1]^2 + both * res(out_2016)[1]^2,
                area_both = both * res(out_2016)[1]^2,
                area = as.numeric(area)) %>%
  dplyr::mutate(fc_dry = area_dry / area,
                fc_wet = area_dry / area,
                fc_both = area_dry / area) %>%
  dplyr::mutate(sum = dry + wet + both) %>%
  dplyr::mutate(prop_dry = dry/sum,
                prop_wet = wet/sum,
                prop_both = both/sum) %>%
  dplyr::mutate(color = rgb(prop_dry, prop_wet, prop_both)) %>%
  dplyr::mutate(area = as.numeric(area)) %>%
  dplyr::select(-sum)

# reshuffle data for plotting - 2003
in_data_03 <- aggr_data_nseas_03$alldata
st_geometry(in_data_03) = NULL
summ_data_03 <- in_data_03 %>%
  dplyr::select(id_feat, value) %>%
  dplyr::filter(value > 0) %>%
  dplyr::count(id_feat, value) %>%
  tidyr::spread(value, n, fill = 0, sep = "_") %>%
  dplyr::rename_at(c(2:4), funs(c("dry","wet","both"))) %>%
  dplyr::left_join(aggr_data_nseas_03$stats) %>%
  sf::st_as_sf() %>%
  dplyr::select_at(c(1:4, 7)) %>%
  dplyr::mutate(area = (dry + wet + both) * res(out_2016)[1]^2)

area_data_03 <- summ_data_03 %>%
  dplyr::mutate(area_dry = dry * res(out_2003)[1]^2 + both * res(out_2003)[1]^2,
                area_wet = wet * res(out_2003)[1]^2 + both * res(out_2003)[1]^2,
                area_both = both * res(out_2003)[1]^2,
                area = as.numeric(area)) %>%
  dplyr::mutate(fc_dry = area_dry / area,
                fc_wet = area_dry / area,
                fc_both = area_dry / area) %>%
  dplyr::mutate(sum = dry + wet + both) %>%
  dplyr::mutate(prop_dry = dry/sum,
                prop_wet = wet/sum,
                prop_both = both/sum) %>%
  dplyr::mutate(color = rgb(prop_dry, prop_wet, prop_both)) %>%
  dplyr::mutate(area = as.numeric(area)) %>%
  dplyr::select(-sum)

# Figure_6 ----
# load ancillary vectors
river = read_vect("/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Datasets/Ancillary/ne_10m_rivers_lake_centerlines.shp")
river <- sf::st_set_crs(river, 4326)
sen_borders <- get_boundaries("SEN", 2) %>%
  tibble::as_tibble() %>%
  sf::st_as_sf() %>%
  sf::st_transform(get_proj4string(in_2016))
mau_borders <- get_boundaries("MRT", 2) %>%
  tibble::as_tibble() %>%
  sf::st_as_sf() %>%
  sf::st_transform(get_proj4string(in_2016))
borders <- rbind(sen_borders, mau_borders) %>%
  dplyr::mutate(lon = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[1]]),
                lat = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[2]])
  ) %>% sf::st_as_sf()

Figure_6_b  <- plot_area(area_data_16, "area_dry", title = "Detected Rice Area 2016 - Dry Season", legend = FALSE)
Figure_6_d  <- plot_area(area_data_16, "area_wet", title = "Detected Rice Area 2016 - Wet Season", legend = FALSE)
Figure_6_f  <- plot_area(area_data_16, "area_both", title = "Detected Rice Area 2016 - Both Seasons", legend = FALSE)

Figure_6_a  <- plot_area(area_data_03, "area_dry", title = "Detected Rice Area 2003 - Dry Season", legend = FALSE)
Figure_6_c  <- plot_area(area_data_03, "area_wet", title = "Detected Rice Area 2003 - Wet Season", legend = FALSE)
Figure_6_e   <- plot_area(area_data_03, "area_both", title = "Detected Rice Area 2003 - Both Seasons", legend = TRUE)

Figure_6 <- grid.arrange(Figure_6_a, Figure_6_b, Figure_6_c, Figure_6_d, Figure_6_e, Figure_6_f, ncol = 2)

ggsave(filename = "/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Manuscript/Figures/Figure_6_b.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = Figure_6_b)
ggsave(filename =  "/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Manuscript/Figures/Figure_6_d.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = Figure_6_d)
ggsave(filename =  "/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Manuscript/Figures/Figure_6_f.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = Figure_6_f)

ggsave(filename = "/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Manuscript/Figures/Figure_6_a.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = Figure_6_a)
ggsave(filename =  "/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Manuscript/Figures/Figure_6_c.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = Figure_6_c)
ggsave(filename =  "/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Manuscript/Figures/Figure_6_e.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = Figure_6_e)

ggsave(filename =  "/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Manuscript/Figures/Figure_6_leg.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = Figure_6_e)


# ggsave(filename =  "/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Manuscript/Figures/Figure_6.tiff",
#        dpi = 400, width = 6.57, height = 13, plot = g)

plot_area <- function(data, var, title ,legend) {
  data <- filter_at(data, var, any_vars(. > 0 ))
  area_dr_2016 <- ggplot(data) +
    geom_sf(data = sen_borders, fill = "grey95", size = 0.3, alpha = 0.8)  +
    geom_sf(data = mau_borders, fill = "#FFEFDB", size = 0.3, alpha = 0.6) +
    geom_sf(aes_string(fill = paste0(var, "/10000")), size = 0.1) +
    scale_fill_distiller("Detected Rice Area [ha]", type = "seq",
                         palette = "YlOrBr", trans = "log",
                         breaks = c(1, 10, 100, 1000),
                         limits = c(1, 1350),
                         oob = scales::squish,
                         direction = 1) +
    xlim(get_extent(summ_data_16)@extent[c(1,3)]) +
    ylim(get_extent(summ_data_16)@extent[c(2,4)]) +
    geom_text(data = borders, aes(label = NAME_2, x = lon, y = lat), size = 2) +
    geom_sf(data = borders, fill = "transparent", size = 0.2) +
    ggplot2::theme_light() +
    theme(panel.ontop = T, panel.background = element_rect(fill = "transparent",
                                                           color = "grey30")) +
    geom_sf(data = river, color = "#378DD4", alpha = 0.1, size = 0.2) +
    theme(plot.margin = margin(0.1,0.1,0.1,0.1, unit = "cm"),
          panel.grid.major = element_line(color = "grey45", size = 0.5)) +
    labs(title = title, x = "", y = "")

    if (legend == TRUE) {
      area_dr_2016 <- area_dr_2016 + ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(title = "Detected Rice Area [ha]",
                                      title.position = "bottom",
                                      title.hjust = 0.5,
                                      direction = "horizontal",
                                      label.position = "top",
                                      label.hjust = 0.5,
                                      barwidth = unit(0.4, "npc"))) +
      theme(legend.margin = margin(0,0,0,0, unit = "cm")) +
       theme(legend.box.spacing = unit(0, "mm"))
    } else {
      area_dr_2016 <- area_dr_2016 + theme(legend.position = "None")
    }
}

# Figure_8 - triangle plots -  ----
data03 = area_data_16 %>% dplyr::mutate(Year = 2016)
data16 = area_data_03%>% dplyr::mutate(Year = 2003)
data = rbind(data03, data16)
Figure_8 <- ggplot(data) +
  geom_sf(data = sen_borders, fill = "grey95", size = 0.11, alpha = 0.8)  +
  geom_sf(data = mau_borders, fill = "#FFEFDB", size = 0.1, alpha = 0.6) +
  geom_sf(aes(fill = color), size = 0.1) +
  scale_fill_identity() +
  xlim(get_extent(area_data_16)@extent[c(1,3)]) +
  ylim(get_extent(area_data_16)@extent[c(2,4)]) +
  geom_text(data = borders, aes(label = NAME_2, x = lon, y = lat), size = 2) +
  geom_sf(data = sen_borders, fill = "transparent", size = 0.2) +
  ggplot2::theme_bw() +
  theme(panel.ontop = T, panel.background = element_rect(fill = "transparent",
                                                         color = "grey30"))  +
  ggplot2::theme(legend.position = "none") +

  theme(legend.margin = margin(0,0,0,0, unit = "cm")) +
  theme(plot.margin = margin(0.1,0.1,0.1,0.1, unit = "cm"),
        panel.grid.major = element_line(color = "grey45", size = 0.5)) +
  ggplot2::labs(title = "Proportions of areas detected in the different seasons") +
  xlab("") + ylab("") +
  theme(legend.box.spacing = unit(0, "mm")) +
  geom_sf(data = river, color = "#378DD4", alpha = 0.1, size = 0.5) +
  facet_wrap(~Year, ncol = 1)

ggsave(filename =  "/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Manuscript/Figures/area_prop_13.tiff",
       dpi = 400, width = 6.46*2, height = 7.46*2, plot = Figure_8)

