# For Figure_2
#
library(raster)
library(sprawl)
library(ggspatial)
library(gridExtra)
library(magrittr)
library(tidyr)
library(data.table)

res_folder <- "/home/lb/nr_working/shared/PhenoRice/Processing/Senegal/Final/Outputs_MaskIrrig/"
setwd(res_folder)
in_2003 <- raster::stack("2003/Phenorice_out_2002_321_2004_089.dat")
in_2016 <- raster::stack("2016/Phenorice_out_2015_321_2017_089.dat")

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

fig_extent <- new("sprawlext", extent = c(-1756882, 1633301, -1292967, 1875359),
                  proj4string = get_proj4string(out_2016))

out_2016 <- crop_rast(out_2016, fig_extent)
out_2003 <- crop_rast(out_2003, fig_extent)

aggr_data_nseas_16 <- extract_rast(out_2016,
                                sow_fish,
                                full_data = TRUE)

aggr_data_nseas_03 <- extract_rast(out_2003,
                                sow_fish,
                                full_data = TRUE)
in_data_16 <- aggr_data_nseas_16$alldata
st_geometry(in_data_16) = NULL
summ_data_16 <- in_data_16 %>%
  dplyr::select(id_feat, value) %>%
  dplyr::filter(value > 0) %>%
  dplyr::count(id_feat, value) %>%
  tidyr::spread(value, n, fill = 0, sep = "_") %>%
  dplyr::rename_at(c(2:4), funs(c("dry","wet","both"))) %>%
  dplyr::left_join(aggr_data_nseas_16$stats) %>%
  sf::st_as_sf() %>%
  dplyr::select_at(c(1:4, 7))

area_data_16 <- summ_data_16 %>%
  dplyr::mutate(area_dry = dry * res(out_2016)[1]^2 + both * res(out_2016)[1]^2,
                area_wet = wet * res(out_2016)[1]^2 + both * res(out_2016)[1]^2,
                area_both = both * res(out_2016)[1]^2,
                area = as.numeric(area)) %>%
  dplyr::mutate(fc_dry = area_dry / area,
                fc_wet = area_dry / area,
                fc_both = area_dry / area) %>%
  dplyr::mutate(sum = dry + wet + both) %>%
  dplyr::mutate(prop_dry = dry/sum,
                prop_wet = wet/sum,
                prop_both = both/sum) %>%
  mutate(color = rgb(prop_dry, prop_wet, prop_both)) %>%
  mutate(area = as.numeric(area)) %>%
  select(-sum)

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
  dplyr::select_at(c(1:4, 7))

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
  mutate(color = rgb(prop_dry, prop_wet, prop_both)) %>%
  mutate(area = as.numeric(area)) %>%
  select(-sum)


# area_data_xx <- area_data %>%
#   gather(season, area, area_dry, area_wet, area_both)
river = read_vect("/home/lb/Temp/ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines.shp")
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

dry_2016  <- plot_area(area_data_16, "area_dry", title = "2016 - Dry Season")
wet_2016  <- plot_area(area_data_16, "area_wet", title = "2016 - Wet Season")
both_2016 <- plot_area(area_data_16, "area_both", title = "2016 - Both Seasons")

dry_2003  <- plot_area(area_data_03, "area_dry", title = "2003 - Dry Season")
wet_2003  <- plot_area(area_data_03, "area_wet", title = "2003 - Wet Season")
both_2003 <- plot_area(area_data_03, "area_both", title = "2003 - Both Seasons")

ggsave(filename = "/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Manuscript/Figures/area_dry_16.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = dry_2016)
ggsave(filename =  "/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Manuscript/Figures/area_wet_16.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = wet_2016)
ggsave(filename =  "/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Manuscript/Figures/area_both_16.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = both_2016)

ggsave(filename = "/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Manuscript/Figures/area_dry_03.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = dry_2003)
ggsave(filename =  "/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Manuscript/Figures/area_wet_03.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = wet_2003)
ggsave(filename =  "/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Manuscript/Figures/area_both_03.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = both_2003)

plot_area <- function(data, var, title) {
  data <- filter_at(data, var, any_vars(. > 0 ))
  area_dr_2016 <- ggplot(data) +
    geom_sf(data = borders, fill = "grey95", size = 0.3)  +
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
    ggplot2::theme_bw() +
    theme(panel.ontop = T, panel.background = element_rect(fill = "transparent",
                                                           color = "grey30"))  +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(title = "Detected Rice Area [ha]",
                                      title.position = "bottom",
                                      title.hjust = 0.5,
                                      label.position = "top",
                                      label.hjust = 0.5,
                                      barwidth = unit(0.5, "npc"))) +
    theme(legend.margin = margin(0,0,0,0, unit = "cm")) +
    theme(plot.margin = margin(0.1,0.1,0.1,0.1, unit = "cm"),
          panel.grid.major = element_line(color = "grey45", size = 0.5)) +
    labs(title = title,
         x = "", y = "") +
    theme(legend.box.spacing = unit(0, "mm")) +
    geom_sf(data = river, color = "#378DD4", alpha = 0.1, size = 0.2)
}

# triangle plots
data = area_data_16
data = area_data_03
 triang_2016 <- ggplot(data) +
    geom_sf(data = borders, fill = "grey95", size = 0.3)  +
    geom_sf(aes(fill = color), size = 0.1) +
    scale_fill_identity() +
    xlim(get_extent(area_data_16)@extent[c(1,3)]) +
    ylim(get_extent(area_data_16)@extent[c(2,4)]) +
    geom_text(data = borders, aes(label = NAME_2, x = lon, y = lat), size = 2) +
    geom_sf(data = borders, fill = "transparent", size = 0.2) +
    ggplot2::theme_bw() +
    theme(panel.ontop = T, panel.background = element_rect(fill = "transparent",
                                                           color = "grey30"))  +
    ggplot2::theme(legend.position = "none") +
    # ggplot2::guides(
    #   fill = ggplot2::guide_colourbar(title = "Detected Rice Area [ha]",
    #                                   title.position = "bottom",
    #                                   title.hjust = 0.5,
    #                                   label.position = "top",
    #                                   label.hjust = 0.5,
    #                                   barwidth = unit(0.5, "npc"))) +
    theme(legend.margin = margin(0,0,0,0, unit = "cm")) +
    theme(plot.margin = margin(0.1,0.1,0.1,0.1, unit = "cm"),
          panel.grid.major = element_line(color = "grey45", size = 0.5)) +
    ggplot2::labs(title = "Proportions of areas detected in the different seasons - 2003") +
   xlab("") +ylab("") +
    theme(legend.box.spacing = unit(0, "mm")) +
    geom_sf(data = river, color = "#378DD4", alpha = 0.1, size = 0.2)

ggsave(filename =  "/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Manuscript/Figures/area_prop_13.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = triang_2016)
#
# plot_rast_gg(out_stack)
#
# out_aggr <- raster::aggregate(out_stack,9 )
# out_aggr <- raster(out_aggr)
#
# prova <- aggregate_rast(out_stack, out_aggr)
#
# # Create tha base plot
# Fig_2 <- plot_rast_gg(
#   out_stack,
#   palette_type = "categorical", labels = c("Dry", "Wet", "Both"),
#   basemap = "osm", scalebar_dist = 10, no_labels = F,
#   xlims = c(-1830000, -1700000),
#   ylims = c(1825000, 1875000))
#
# # tweak legend etcetera
# Fig_2 + ggplot2::theme(legend.position = "bottom") +
#   ggplot2::guides(fill = ggplot2::guide_legend(title = "Detected Rice Seasons",
#                              title.position = "top",
#                              title.hjust = 0.5,
#                              label.position = "bottom",
#                              label.hjust = 0.5, keywidth = 5)) +
#   # ggtitle(title = "Detected rice seasons in 2003 and 2016") +
#   theme(plot.title = element_text("Detected rice seasons in 2003 and 2016", hjust = 0.5))
#
#

# raster_df_2016 <- fortify(out_rast_2016)
# raster_df_2003 <- fortify(out_rast_2003)
#
# out_rast_2016_3857   <- projectRaster(out_rast_2016, crs = "+init=epsg:3857", method = "ngb")
# out_rast_2016_3857_f <- fortify(out_rast_2016) %>%
#   as.data.table() %>%
#   na.omit("band1")
#
# out_rast_2003_3857 <- projectRaster(out_rast_2003, crs="+init=epsg:3857", method="ngb")
# out_rast_2003_3857_f <- fortify(out_rast_2003_3857) %>%
#   filter(!is.na(band1))
#
# out_rast_2016_f <- fortify(out_rast_2016_3857) %>%
#   as.data.table() %>%
#   na.omit("band1")
#
#
# plot_rast_gg <- function(in_rast,
#                          xlims          = NULL, ylims = NULL,
#                          background_map = NULL, zoomin = -1,
#                          scalebar       = TRUE, scalebar_dist = NULL,
#                          na.color       = "transparent",
#                          palette_type   = "gradient", palette = NULL,
#                          no_axis        = TRUE,
#                          title          = NULL, subtitle = NULL) {
#
#   assertthat::assert_that(palette_type %in% c("categorical", "gradient", "diverging"),
#                           msg = "Invalid palette_type. Aborting!")
#
# #   ____________________________________________________________________________
# #   Set default palettes for different categories                           ####
#   def_palettes <- list(categorical = "Set1",
#                        gradient    = "Greens",
#                        diverging   = "RdYlGn")
#
#   if (is.null(palette)) {
#     palette <- as.character(def_palettes[palette_type])
#   }
#
#   #   ____________________________________________________________________________
#   #   Fortify the raster to allow ggplotting                                  ####
#
#   ext <- get_extent(in_rast)
#   in_rast_fort <- ggplot2::fortify(in_rast) %>%
#     data.table::as.data.table()
#
#   #   ____________________________________________________________________________
#   #   if transparent NA, remove the NAs from the data to speed-up rendering   ####.
#
p_2016 + guides(fill = guide_legend(title = "Detected Rice Seasons", title.position = "top",

#
#   if (na.color == "transparent")  {
#     in_rast_fort <- na.omit(in_rast_fort, 3)
#   }
#
#   #   ____________________________________________________________________________
#   #   Reproject to 3857 to allow overlap with background map                  ####
#
#   if (!is.null(background_map)) {
#     new_coords <- sf::st_as_sf(in_rast_fort, coords = c(1,2)) %>%
#       sf::st_set_crs(ext@projstring) %>%
#       sf::st_transform(crs = 3857) %>%
#       sf::st_coordinates()
#
#     in_rast_fort$x <- new_coords[,1]
#     in_rast_fort$y <- new_coords[,2]
#   }
#
#   #   ____________________________________________________________________________
#   #   If no limits passed, compute them from the input extent                 ####
#
#   if (is.null(xlims)) {
#     xlims <- c(ext@extent["xmin"], ext@extent["xmax"])
#     ylims <- c(ext@extent["ymin"], ext@extent["ymax"])
#   }
#
#   #   ____________________________________________________________________________
#   #   If no scalebar dist passed, compute automatically from lonfgitude range ####
#   if (is.null(scalebar_dist)) {
#     km_extent  <- round(diff(xlims)/1000)
#     scale_dist <- round(km_extent/100)*10
#   }
#
#   if (palette_type == "categorical") {
#     in_rast_fort[[3]] <- factor(in_rast_fort[[3]])
#   }
#
#   #   ____________________________________________________________________________
#   #   Initialize the plot                                                     ####
#
#   plot_gg <- ggplot2::ggplot() +
#     ggplot2::theme_bw() +
#     ggplot2::xlim(xlims[1], xlims[2]) +
#     ggplot2::ylim(ylims[1], ylims[2])
#   # +
#   #   ggplot2::ggtitle(title, subtitle = subtitle)
#
#   #   ____________________________________________________________________________
#   #   add background map - need to do this here to prevent shadowing          ####
#
#   if (!is.null(background_map)) {
#     plot_gg <- plot_gg + geom_osm(zoomin = zoomin,
#                                   type   = background_map)
#   }
#
#   #   ____________________________________________________________________________
#   #   add the raster layer          ####
#
#   plot_gg <- plot_gg +
#     ggplot2::geom_raster(data = in_rast_fort, ggplot2::aes(x, y, fill = as.factor(band1)))
#
#   #   ____________________________________________________________________________
#   #   Modify the palette according to varaible type and palette               ####
#
#   if (palette_type == "categorical") {
#     plot_gg <- plot_gg +
#       ggplot2::scale_fill_brewer(type = "qual", palette = palette)
#   } else {
#   if (palette_type == "sequential") {
#      plot_gg <- plot_gg +
#       ggplot2::scale_fill_brewer(type = "seq", palette = palette)
#   } else {
#       plot_gg <- plot_gg +
#       ggplot2::scale_fill_brewer(type = "div", palette = palette)
#    }
#   }
#
#   #   ____________________________________________________________________________
#   #   Add scalebar               ####
#
#   if (scalebar) {
#     # ggplot2::coord_cartesian(xlim = xlims, ylim = ylims) +
#     plot_gg <- plot_gg +
#       ggsn::scalebar(dd2km = FALSE, dist = scale_dist,
#                      x.min = xlims[1], x.max = xlims[2],
#                      y.min = ylims[1], y.max = ylims[2],
#                      location = "bottomright", st.size = 3,
#                      st.bottom = FALSE, model = NULL,
#                      st.dist = 0.03)
#   }
#
#   #   ____________________________________________________________________________
#   #   Finalize and return               ####
#
#   plot_gg <- plot_gg +
#     ggplot2::coord_fixed()
#   return(plot_gg)
# }
#
# p_2016 <- plot_rast_gg(out_rast_2016_3857, background_map = "stamenwatercolor",
#                        palette_type = "categorical",
#                        xlims = c(-1830000, -1700000),
#                        ylims = c(1825000, 1875000)) +
#   theme(legend.position = "bottom")
#
# +
#   ggplot2::
#
#   geom_spatial(out_rast_2016, aes(fill = as.factor(band1)), crsfrom = get_projstring(out_rast_2016)) +
#   # coord_fixed() +
#   xlim(-1750000, -1400000) + ylim(1750000, 1875000) +
#   scale_fill_manual("" ,breaks = c(1,2,3), values = c(c("brown3", "#2531DB", "#317837")),
#                     labels = c("Dry Season", "Wet Season", "Both Seasons")) +
#   theme_bw() +
#   theme(legend.position = c(0.9,0.20)) +
#   ggtitle("Detected rice areas in the Dry and Wet seasons", subtitle = "2003") +
#   xlab("Easting [m]") + ylab("Northing[m]")
#
#
# p_2003 <- ggplot(out_rast_2003_3857_f, aes(x = x, y = y, fill = as.factor(band1))) +
#   geom_osm(zoomin =  -1, type = "stamenwatercolor") +
#   geom_raster(na.rm = T) +
#   coord_fixed() +
#   xlim(-1830000, -1700000) + ylim(1825000, 1875000) +
#   scale_fill_manual("" ,breaks = c(1,2,3), values = c(c("brown3", "#2531DB", "#317837")),
#                     labels = c("Dry Season", "Wet Season", "Both Seasons")) +
#   theme_bw() +
#   theme(legend.position = c(0.9,0.20)) +
#   ggtitle("Detected rice areas in the Dry and Wet seasons", subtitle = "2003") +
#   xlab("Easting [m]") + ylab("Northing[m]")
#
# p_2016 <- ggplot(out_rast_2016_3857_f, aes(x = x, y = y, fill = as.factor(band1))) +
#   geom_osm(zoomin = -1, type = "stamenbw") +
#   geom_raster(na.rm = T) +
#   coord_fixed() +
#   xlim(-1830000, -1700000) + ylim(1825000, 1875000) +
#   scale_fill_manual("" ,breaks = c(1,2,3), values = c(c("brown3", "#2531DB", "#317837")),
#                     labels = c("Dry Season", "Wet Season", "Both Seasons")) +
#   theme_bw() +
#   theme(legend.position = c(0.9,0.20)) +
#   ggtitle("", subtitle = "2016") +
#   xlab("Easting [m]") + ylab("Northing[m]")
#
# # aaa <- grid.arrange(p_2003, p_2016, nrow = 2)
# tiff("/home/lb/Temp/Figure_2_riceseasone_2003_vs_2016.tif",
#      width = 16, height = 14, units = "cm", res = 400,
#      pointsize = 6)
# grid.arrange(p_2003, p_2016, nrow = 2)
# dev.off()
# ggsave("/home/lb/Temp/p_2003_2016.png", aaa, width = 16, units = "cm", dpi = 600)
#
