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
out_stack <- raster::stack(out_rast_2003, out_rast_2016)
names(out_stack) = c("PhenoRice_2003", "PhenoRice_2016")
plot_rast_gg(out_stack)

out_aggr <- raster::aggregate(out_stack,9 )
out_aggr <- raster(out_aggr)

prova <- aggregate_rast(out_stack, out_aggr)

# Create tha base plot
Fig_2 <- plot_rast_gg(
  out_stack,
  palette_type = "categorical", labels = c("Dry", "Wet", "Both"),
  basemap = "osm", scalebar_dist = 10, no_labels = F,
  xlims = c(-1830000, -1700000),
  ylims = c(1825000, 1875000))

# tweak legend etcetera
Fig_2 + ggplot2::theme(legend.position = "bottom") +
  ggplot2::guides(fill = ggplot2::guide_legend(title = "Detected Rice Seasons",
                             title.position = "top",
                             title.hjust = 0.5,
                             label.position = "bottom",
                             label.hjust = 0.5, keywidth = 5)) +
  # ggtitle(title = "Detected rice seasons in 2003 and 2016") +
  theme(plot.title = element_text("Detected rice seasons in 2003 and 2016", hjust = 0.5))



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
