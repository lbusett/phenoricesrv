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

res_folder <- "/home/lb/nr_working/shared/PhenoRice/Processing/Senegal/Final/Outputs_MaskIrrig/"
setwd(res_folder)
in_2016 <- raster::stack("2016/Phenorice_out_2015_321_2017_089.dat")
names(in_2016) <- c("Sowing_Date_Dry", "Sowing_Date_Wet")
#
#                     "Flowering_Date_Wet", "Flowering_Date_Dry",
#                     "Harvesting_Date_Wet", "Harvesting_Date_Dry")


sow_dry <- in_2016[[2]]

labels <- as.Date("2016-02-15") %m+% days(c(0,15, 29, 45))
breaks <- datetodoy(labels)
labels <- format(labels, "%d\ %B")

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



