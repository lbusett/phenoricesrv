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

ext <- select_extent(in_2016[[1]], transparency = 0.5)
#
#                     "Flowering_Date_Wet", "Flowering_Date_Dry",
#                     "Harvesting_Date_Wet", "Harvesting_Date_Dry")

save(ext, file = "/home/lb/Source/git/phenoricesrv/extent_maps.RData")
sow_dry <- in_2016[[2]]
sow_fish <- create_fishnet(sow_dry, pix_for_cell = 10)


aggr_data <- extract_rast(sow_dry, sow_fish, full_data = FALSE)$stats
aggr_data_filt <- aggr_data %>%
  filter(avg > 0)
aggr_data_filt <- sf::st_transform(aggr_data_filt, "+init=epsg:3857")
data(World)
data(rivers)
data(land)

labels <- as.Date("2016-02-01") %m+% days(c(0, 15, 30, 46, 60, 75))
breaks <- datetodoy(labels)
labels <- as.character(format(labels, "%d/%m"), srt = 45)

sen_borders <- get_boundaries("SEN", 2) %>%
  tibble::as_tibble() %>%
  sf::st_as_sf() %>%
  sf::st_transform(3857)
mau_borders <- get_boundaries("MRT", 2) %>%
  tibble::as_tibble() %>%
  sf::st_as_sf() %>%
  sf::st_transform(3857)
borders <- rbind(sen_borders, mau_borders) %.>%
  dplyr::mutate(.,
    lon = purrr::map_dbl(geometry, ~st_centroid(.x)[[1]]),
    lat = purrr::map_dbl(geometry, ~st_centroid(.x)[[2]])
    )


plot <- ggplot() +
  geom_sf(data = aggr_data_filt, aes(fill = avg), size = 0.1) +
  geom_sf(data = borders, fill = "transparent") +
  geom_text(data = borders, aes(label = NAME_2, x = lon, y = lat), size = 3) +
  xlim(get_extent(aggr_data_filt)@extent[c(1,3)]) +
  ylim(get_extent(aggr_data_filt)@extent[c(2,4)]) +
  theme_bw() + coord_sf(crs = "+init=epsg:3857")  +
  scale_fill_distiller(type = "div",
                       palette = "RdYlGn",
                       labels = labels,
                       limits = c(min(breaks), max(breaks)),
                       oob = scales::squish,
                       breaks = breaks,
                       direction = 1) +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::guides(
    fill = ggplot2::guide_colourbar(title = "",
                                    title.position = "top",
                                    title.hjust = 0.5,
                                    label.position = "bottom",
                                    label.hjust = 0.5,
                                    barwidth = unit(0.5, "npc"))) +
  theme(legend.margin = margin(0,0,0,0, unit="cm")) +
  theme(plot.margin = margin(0.1,0.1,0.1,0.1, unit = "cm"))+
  labs(title = "Average Sowing Date - Dry Season 2016",
         x = "Latitude", y = "Longitude")


sen_borders <- sf::st_transform(sen_borders, 3857) %>%
  as("Spatial") %>%
  crop_vect(get_extent(aggr_data_filt))

ggplot() +  geom_spatial(data = sen_borders, fill = "transparent", color = "black") +
geom_osm(as(aggr_data_filt, "Spatial"), type = "osm") + coord_map() +
  geom_spatial(as(aggr_data_filt, "Spatial"),
               aes(fill = avg), color = "grey75", size = 0.11) +

  scale_fill_distiller(type = "div", palette = "RdYlGn",
                       labels = labels, limits = c(min(breaks), max(breaks)),
                       oob = scales::squish,
                       breaks = breaks,
                       direction = 1)

  + coord_map()
+
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::guides(
    fill = ggplot2::guide_colourbar(title = "",
                                    title.position = "top",
                                    title.hjust = 0.5,
                                    label.position = "bottom",
                                    label.hjust = 0.5,
                                    barwidth = unit(0.5, "npc"))) +
  xlim(get_extent(aggr_data_filt)@extent[c(1,3)]) +
  ylim(get_extent(aggr_data_filt)@extent[c(2,4)])


# plot <- ggplot()





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



