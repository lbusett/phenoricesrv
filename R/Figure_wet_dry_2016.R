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
setwd(res_folder)
in_2016 <- read_rast("2016/Phenorice_out_2015_321_2017_089.dat")
# names(in_2016) <- c("Sowing_Date_Dry", "Sowing_Date_Wet")

# ext <- select_extent(in_2016[[1]], transparency = 0.5)
#
#                     "Flowering_Date_Wet", "Flowering_Date_Dry",
#                     "Harvesting_Date_Wet", "Harvesting_Date_Dry")

sen_borders <- get_boundaries("SEN", 2) %>%
  tibble::as_tibble() %>%
  sf::st_as_sf() %>%
  sf::st_transform(get_proj4string(in_2016))
mau_borders <- get_boundaries("MRT", 2) %>%
  tibble::as_tibble() %>%
  sf::st_as_sf() %>%
  sf::st_transform(get_proj4string(in_2016))
borders <- rbind(sen_borders, mau_borders) %.>%
  dplyr::mutate(.,
                lon = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[1]]),
                lat = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[2]])
  ) %>%
  sf::st_as_sf()
# save(ext, file = "/home/lb/Source/git/phenoricesrv/extent_maps.RData")
sow_dry <- in_2016[[2]]
sow_fish <- create_fishnet(sow_dry, cellsize = 5000, shape = "hex")

in_rast <- in_2016[[c(2,3,4,5,6,7)]]
names(in_rast) <- c("Sow_Dry" ,"Sow_Wet", "Head_Dry", "Head_Wet", "Harv_Dry",
                    "Harv_Wet")

aggr_data <- extract_rast(in_rast, sow_fish, full_data = FALSE)$stats
aggr_data_filt <- aggr_data %>%
  dplyr::filter(avg > 0, n_pix_val > 1) %.>%
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
p_dry_sow <- plot_dates(data_ds, "avg",borders,
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
p_wet_sow <- plot_dates(data_ws, "avg",borders,
                        leg_labels = labels, leg_breaks = breaks,
                        title = "Average Sowing Date - Wet Season 2016") +
  # theme(legend.box.spacing = unit(0, "mm")) +
  xlim(-1756882 ,-1292967 ) + ylim(1633301, 1875359)


#   ____________________________________________________________________________
#   Head_dry                                                                 ####
data_ds   <- dplyr::filter(aggr_data_filt, band_name == "Head_Dry")
labels <- as.Date("2016-05-01") %m+% days(c(0, 14, 31, 47, 61, 76))
breaks <- datetodoy(labels)
labels <- format(labels, "%d/%m")
# zlims <- quantile(data_ds$avg, probs = c(0.02,0.98))
p_dry_head <- plot_dates(data_ds, "avg",borders,
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
p_wet_head <- plot_dates(data_ws, "avg",borders,
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
p_dry_harv <- plot_dates(data_ds, "avg",borders,
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
p_wet_harv <- plot_dates(data_ws, "avg",borders,
                        leg_labels = labels, leg_breaks = breaks,
                        title = "Average Flowering Date - Wet Season 2016") +
  # theme(legend.box.spacing = unit(0, "mm")) +
  xlim(-1756882 ,-1292967 ) + ylim(1633301, 1875359)


ggsave(filename = "/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Manuscript/Figures/sow_dry_16.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = p_dry_sow)
ggsave(filename = "/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Manuscript/Figures/sow_wet_16.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = p_wet_sow)
ggsave(filename = "/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Manuscript/Figures/head_dry_16.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = p_dry_head)
ggsave(filename = "/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Manuscript/Figures/head_wet_16.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = p_wet_head)
ggsave(filename = "/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Manuscript/Figures/harv_wet_16.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = p_dry_harv)
ggsave(filename = "/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Manuscript/Figures/harv_dry_16.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = p_wet_harv)



library(gridExtra)
grid.arrange(p_dry_sow, p_wet_sow, p_dry_head, p_wet_head, p_dry_harv, p_wet_harv,
              ncol = 2)

plot_dates <- function(in_data, var, borders, leg_labels, leg_breaks, title) {

   plot <- ggplot() +
    geom_sf(data = borders, fill = "grey95", size = 0.3)  +
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
    geom_sf(data = borders, fill = "transparent", size = 0.2) +

    theme(panel.ontop = T, panel.background = element_rect(fill = "transparent",
                                                           color = "grey30"))  +
    theme(legend.margin = margin(0,0,0,0, unit = "cm")) +
    theme(plot.margin = margin(0.1,0.1,0.1,0.1, unit = "cm"),
          panel.grid.major = element_line(color = "grey45", size = 0.5))+
    labs(title = title,
         x = "", y = "") +
    theme(legend.box.spacing = unit(0, "mm")) +
    geom_sf(data = river, color = "#378DD4", alpha = 0.1, size = 0.2)
  plot

}



# aggr_data_filt <- sf::st_transform(aggr_data_filt, "+init=epsg:3857")
data(World)
data(rivers)
data(land)

plot_gg <-  tm_shape(aggr_data_filt, bbox = as(get_extent(aggr_data_filt), "Extent")) +
  tm_polygons("avg", colorNA = "transparent", palette = "RdYlGn", style = "cont",
              auto.palette.mapping = FALSE, title = "",
              breaks = breaks, labels = labels,
              legend.is.portrait = FALSE) +
  # tm_shape(land, bbox = as(get_extent(aggr_data_filt), "Extent")) +
  # tm_raster("cover_cls", bbox = as(get_extent(aggr_data_filt), "Extent")) +
  tm_shape(sen_borders) +
  tm_borders() +
  tm_text("NAME_2", size = 0.7) +
  tm_shape(mau_borders) +
  tm_borders() +
  tm_text("NAME_2", size = 0.7) +
  tm_legend(legend.bg.color = "white",
            legend.bg.alpha = 1,
            legend.frame = T,
            # legend.position = c("center","bottom"),
            # legend.width = 50,
            legend.just = "center",
            legend.position = c("right","top")) +
  tm_scale_bar()

ggplot2::labs(title = "New plot title")




sow_dry_plot <- plot_rast_gg(sow_dry, basemap = "stamenbw",
                             xlims = c(-1830000, -1700000),
                             ylims = c(1825000, 1875000), scalebar_dist = 25,
                             palette_type = "diverging", zlims = c(0.02, 0.98),
                             zlims_type = "percs",
                             band_names = "Sowing Date - dry Season 2016",
                             leg_breaks = breaks,
                             leg_labels = labels,
                             leg_type = "continuous",
                             palette_name = "RdYlGn",
                             title = "Sowing Dates - Dry Season 2016")

sow_dry_plot + ggplot2::theme(legend.position = "bottom") +
  ggplot2::guides(fill = ggplot2::guide_colourbar(title = "",
                                                  title.position = "top",
                                                  title.hjust = 0.5,
                                                  label.position = "bottom",
                                                  label.hjust = 0.5, barwidth = unit(0.8, "npc")))

sow_dry_plot + ggplot2::theme(legend.position = "bottom") +
  ggplot2::guides(fill = ggplot2::guide_legend(title = "",
                                               title.position = "top",
                                               title.hjust = 0.5,
                                               label.position = "bottom",
                                               label.hjust = 0.5, keywidth = 5))
myext <- new("sprawlext", extent = c(-1830000,1825000,-1700000, 1875000 ))
sow_wet <- crop(in_2016[[3]],

                labels <- as.Date("2016-07-01") %m+% days(c(0,15, 29, 45))
                breaks <- datetodoy(labels)
                labels <- format(labels, "%d\ %B")

                sow_wet_plot <- plot_rast_gg(sow_wet, basemap = "stamenbw",
                                             xlims = c(-1830000, -1700000),
                                             ylims = c(1825000, 1875000), scalebar_dist = 25,
                                             palette_type = "diverging", zlims = c(0.02, 0.98),
                                             zlims_type = "percs",
                                             band_names = "Sowing Date - wet Season 2016",
                                             # leg_breaks = breaks,
                                             # leg_labels = labels,
                                             leg_type = "continuous",
                                             palette_name = "RdYlGn",
                                             na.color = "transparent", oob_style = "expand",
                                             title = "Sowing Dates - Wet Season 2016"
                )

                sow_wet_plot + ggplot2::theme(legend.position = "bottom") +
                  ggplot2::guides(fill = ggplot2::guide_colourbar(title = "",
                                                                  title.position = "top",
                                                                  title.hjust = 0.5,
                                                                  label.position = "bottom",
                                                                  label.hjust = 0.5, barwidth = unit(0.8, "npc")))

                sow_wet_plot + ggplot2::theme(legend.position = "bottom") +
                  ggplot2::guides(fill = ggplot2::guide_legend(title = "",
                                                               title.position = "top",
                                                               title.hjust = 0.5,
                                                               label.position = "bottom",
                                                               label.hjust = 0.5, keywidth = 5))



