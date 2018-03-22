# For Figure_7
library(raster)
library(sprawl)
library(ggspatial)
library(gridExtra)
library(magrittr)
library(tidyr)
library(data.table)
library(dplyr)

res_folder <- "/home/lb/nr_working/shared/PhenoRice/Processing/Senegal/Final/lgtthresh_noshape/outputs/"
setwd(res_folder)

files <- list.files(res_folder, pattern = ".dat$", recursive = T, full.names = T)

which_dry <- list()
which_wet <- list()
which_both <- list()
for (ff in seq_along(files))  {
  print(ff)
  file <- files[ff]
  year <- basename(dirname(file))
  which_dry_tmp <- which(!is.na(getValues(read_rast(file, bands = 2))))
  which_wet_tmp <- which(!is.na(getValues(read_rast(file, bands = 3))))
  which_both[[ff]] <- intersect(which_dry_tmp, which_wet_tmp)
  if (length(which_both[[ff]]) != 0 ) {
    which_dry[[ff]] <- setdiff(which_dry_tmp, which_both[[ff]])
    which_wet[[ff]] <- setdiff(which_wet_tmp, which_both[[ff]])
  } else {
    which_dry[[ff]] <- which_dry_tmp
    which_wet[[ff]] <- which_wet_tmp
  }
}

area_wet <- lapply(which_wet, FUN = function(x) length(x) * 233.656 * 233.656) %>%
  unlist()

area_dry <- lapply(which_dry, FUN = function(x) length(x) * 233.656 * 233.656) %>%
  unlist()

area_both <- lapply(which_both, FUN = function(x) length(x) * 233.656 * 233.656) %>%
  unlist()

areas <- data.frame(year = 2003:2016, `Wet` = area_wet, `Dry` = area_dry,
                    `Both` = area_both) %>%
  tidyr::gather(season, area, -year) %>%
  rename(Season = season)

Figure_7 <- ggplot(areas, aes(x = factor(year), y = area/10000, fill = Season)) +
  geom_bar(stat = "identity", color = "black") +
  theme_light() +
  scale_fill_manual(values = c("darkred", "darkgreen","orange")) +
  xlab("Year") + ylab("PhenoRice detected Area [ha]")

ggsave(filename = "/home/lb/Google_Drive/IREA/MI_Conference&paper/Busetto_PhenoRice_SRV/Manuscript/Figures/Figure_7.tiff",
       dpi = 400, width = 6.57, height = 4.23, plot = Figure_7)
