# FIgure 3
#
library(raster)
library(sprawl)
library(ggspatial)
library(gridExtra)
library(magrittr)
library(tidyr)
library(data.table)
library(lubridate)
library(mapedit)
library(wrapr)
library(mapview)
res_folder <- "/home/lb/nr_working/shared/PhenoRice/Processing/Senegal/Final/Outputs_MaskIrrig/"

res_folder <- "/home/lb/nr_working/shared/PhenoRice/Processing/Senegal/Final/mask_irrig/thresh_1000/outputs/"

res_folder <- "/home/lb/nr_working/shared/PhenoRice/Processing/Senegal/Final/lgtthresh_noshape/outputs/"
setwd(res_folder)
in_2016 <- read_rast("2016/Phenorice_out_2015_313_2017_129.dat")

sen_borders <- get_boundaries("SEN", 2) %>%
  tibble::as_tibble() %>%
  sf::st_as_sf() %>%
  sf::st_transform(get_proj4string(in_2016))
mau_borders <- get_boundaries("MRT", 2) %>%
  tibble::as_tibble() %>%
  sf::st_as_sf() %>%
  sf::st_transform(get_proj4string(in_2016))
borders <- rbind(sen_borders, mau_borders) %>%
  dplyr::mutate(.,
                lon = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[1]]),
                lat = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[2]])
  ) %>%
  sf::st_as_sf()

river = read_vect("/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Datasets/Ancillary/ne_10m_rivers_lake_centerlines.shp")
river <- sf::st_set_crs(river, 4326)

# save(ext, file = "/home/lb/Source/git/phenoricesrv/extent_maps.RData")
sow_dry <- in_2016[[2]]
sow_fish <- create_fishnet(sow_dry, cellsize = 5000, shape = "hex")

in_rast <- in_2016[[c(2,3,4,5,6,7)]]
names(in_rast) <- c("Sow_Dry" ,"Sow_Wet", "Head_Dry", "Head_Wet", "Harv_Dry",
                    "Harv_Wet")

aggr_data <- extract_rast(in_rast, sow_fish, full_data = FALSE)$stats
aggr_data_filt <- aggr_data %>%
  dplyr::filter(avg > 0, n_pix_val > 1) %>%
  dplyr::mutate(.,
                lon = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[1]]),
                lat = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[2]])
  ) %>%
  sf::st_as_sf()


#   ____________________________________________________________________________
#   Sow_dry                                                                 ####
data_ds   <- dplyr::filter(aggr_data_filt, band_name == "Sow_Dry") %>%
  dplyr::mutate(rice_area = log(0.01 + n_pix_val * 231.656^2 / 10000)) %>%
  dplyr::mutate(rice_area = rice_area / max(rice_area))
labels <- as.Date("2016-02-15") %m+% days(c(0, 15, 30, 46, 60, 75))
breaks <- datetodoy(labels)
labels <- format(labels, "%d/%m")
zlims <- quantile(data_ds$avg, probs = c(0.02, 0.98))

fig_extent <- new("sprawlext", extent = c(-1756882, 1633301, -1292967, 1875359),
                  proj4string = get_proj4string(data_ds)) %>%
  as("SpatialPolygons") %>% st_as_sf()

p_dry_sow <- plot_dates(data_ds, "avg", sen_borders, mau_borders, river,
                        leg_labels = labels, leg_breaks = breaks,
                        title = "Average Sowing Date - Dry Season 2016") +
    xlim(-1756882 ,-1292967 ) + ylim(1633301, 1875359)

#   ____________________________________________________________________________
#   Sow_wet                                                                 ####
data_ws   <- dplyr::filter(aggr_data_filt, band_name == "Sow_Wet")%>%
  dplyr::mutate(rice_area = log(0.01 + n_pix_val * 231.656^2 / 10000)) %>%
  dplyr::mutate(rice_area = rice_area / max(rice_area))
labels <- as.Date("2016-07-15") %m+% days(c(0, 15, 30, 46, 60, 75))
breaks <- datetodoy(labels)
labels <- format(labels, "%d/%m")
zlims <- quantile(data_ws$avg, probs = c(0.02,0.98))
p_wet_sow <- plot_dates(data_ws, "avg",sen_borders, mau_borders, river,
                        leg_labels = labels, leg_breaks = breaks,
                        title = "Average Sowing Date - Wet Season 2016") +
  xlim(-1756882 ,-1292967 ) + ylim(1633301, 1875359)


#   ____________________________________________________________________________
#   Head_dry                                                                 ####
data_ds   <- dplyr::filter(aggr_data_filt, band_name == "Head_Dry")
labels <- as.Date("2016-05-01") %m+% days(c(0, 14, 31, 47, 61, 76))
breaks <- datetodoy(labels)
labels <- format(labels, "%d/%m")
# zlims <- quantile(data_ds$avg, probs = c(0.02,0.98))
p_dry_head <- plot_dates(data_ds, "avg", sen_borders, mau_borders, river,
                        leg_labels = labels, leg_breaks = breaks,
                        title = "Average Flowering Date - Dry Season 2016") +
  # theme(legend.box.spacing = unit(0, "mm")) +
  xlim(-1756882 ,-1292967 ) + ylim(1633301, 1875359)

#   ____________________________________________________________________________
#   Head_wet                                                                 ####
data_ws   <- dplyr::filter(aggr_data_filt, band_name == "Head_Wet")
labels <- as.Date("2016-09-15") %m+% days(c(0, 16, 31, 47, 61, 77))
breaks <- datetodoy(labels)
labels <- format(labels, "%d/%m")
zlims <- quantile(data_ws$avg, probs = c(0.02,0.98))
p_wet_head <- plot_dates(data_ws, "avg", sen_borders, mau_borders, river,
                        leg_labels = labels, leg_breaks = breaks,
                        title = "Average Harvesting Date - Wet Season 2016") +
  # theme(legend.box.spacing = unit(0, "mm")) +
  xlim(-1756882 ,-1292967 ) + ylim(1633301, 1875359)


#   ____________________________________________________________________________
#   Harv_dry                                                                 ####
data_ds   <- dplyr::filter(aggr_data_filt, band_name == "Harv_Dry")
labels <- as.Date("2016-06-15") %m+% days(c(0, 16, 31, 47, 61, 76))
breaks <- datetodoy(labels)
labels <- format(labels, "%d/%m")
zlims <- quantile(data_ds$avg, probs = c(0.02,0.98))
p_dry_harv <- plot_dates(data_ds, "avg", sen_borders, mau_borders, river,
                        leg_labels = labels, leg_breaks = breaks,
                        title = "Average Harvesting Date - Dry Season 2016") +
  # theme(legend.box.spacing = unit(0, "mm")) +
  xlim(-1756882 ,-1292967 ) + ylim(1633301, 1875359)

#   ____________________________________________________________________________
#   Harv_wet                                                               ####
data_ws   <- dplyr::filter(aggr_data_filt, band_name == "Harv_Wet")
labels <- as.Date("2016-10-15") %m+% days(c(0, 16, 31, 47, 61, 77))
breaks <- datetodoy(labels)
labels <- format(labels, "%d/%m")
zlims <- quantile(data_ws$avg, probs = c(0.02,0.98))
p_wet_harv <- plot_dates(data_ws, "avg", sen_borders, mau_borders, river,
                        leg_labels = labels, leg_breaks = breaks,
                        title = "Average Flowering Date - Wet Season 2016") +
  # theme(legend.box.spacing = unit(0, "mm")) +
  xlim(-1756882 ,-1292967 ) + ylim(1633301, 1875359)


ggsave(filename = "/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Manuscript/Figures/sow_dry_16.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = p_dry_sow)
ggsave(filename = "/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Manuscript/Figures/sow_wet_16.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = p_wet_sow)
ggsave(filename = "/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Manuscript/Figures/head_dry_16.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = p_dry_head)
ggsave(filename = "/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Manuscript/Figures/head_wet_16.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = p_wet_head)
ggsave(filename = "/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Manuscript/Figures/harv_wet_16.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = p_dry_harv)
ggsave(filename = "/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Manuscript/Figures/harv_dry_16.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = p_wet_harv)


plot_dates <- function(in_data, var, sen_borders, mau_borders, river, leg_labels, leg_breaks, title) {

   plot <- ggplot() +
     geom_sf(data = sen_borders, fill = "grey95", size = 0.11, alpha = 0.8)  +
     geom_sf(data = mau_borders, fill = "#FFEFDB", size = 0.1, alpha = 0.6) +
    geom_sf(data = in_data, aes_string(fill = var), size = 0.1) +
    # geom_text(data = borders, aes(label = NAME_2, x = lon, y = lat), size = 2.5) +
    xlim(get_extent(in_data)@extent[c(1,3)]) +
    ylim(get_extent(in_data)@extent[c(2,4)]) +
    ggplot2::theme_bw() +
    scale_fill_distiller(type = "div",
                         palette = "RdYlGn",
                         labels = leg_labels,
                         limits = c(min(leg_breaks),max(leg_breaks)),
                         oob = scales::squish,
                         breaks = leg_breaks,
                         direction = 1) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(title = "",
                                      title.position = "bottom",
                                      title.hjust = 0.5,
                                      label.position = "top",
                                      label.hjust = 0.5,
                                      barwidth = unit(0.5, "npc"))) +
     geom_text(data = borders, aes(label = NAME_2, x = lon, y = lat), size = 2) +

    # geom_sf(data = borders, fill = "transparent", size = 0.2) +

    theme(panel.ontop = T, panel.background = element_rect(fill = "transparent",
                                                           color = "grey30"))  +
    theme(legend.margin = margin(0,0,0,0, unit = "cm")) +
    theme(plot.margin = margin(0.1,0.1,0.1,0.1, unit = "cm"),
          panel.grid.major = element_line(color = "grey45", size = 0.5)) +
    labs(title = title,
         x = "", y = "") +
    theme(legend.box.spacing = unit(0, "mm")) +
    geom_sf(data = river, color = "#378DD4", alpha = 0.1, size = 0.2)
  plot

}
