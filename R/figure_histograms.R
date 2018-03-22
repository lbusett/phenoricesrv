#Figure_histograms
#

res_folder <- "/home/lb/nr_working/shared/PhenoRice/Processing/Senegal/Final/mask_irrig/thresh_1000/outputs/"
setwd(res_folder)
in_files <- list.files(res_folder, pattern = ".dat$", recursive = T, full.names = T)

years <- (2003:2016)
data_all = list()
for (ff in seq_along(in_files)) {

  file <- in_files[ff]
  in_rast <- read_rast(file)
  data_dry <- as.numeric(na.omit(getValues(in_rast[[2]])))
  data_wet <- as.numeric(na.omit(getValues(in_rast[[3]])))
  data_all[[ff]] <- data.frame(year = years[ff], doys = c(data_dry, data_wet))

}

data_all_df <- data.table::rbindlist(data_all) %>%
  dplyr::mutate(year = as.factor(year)) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(date = doytodate(doys, 2000))

pl <- ggplot(data_all_df, aes(x = date)) +   theme_light() +
  geom_histogram(aes(y =100*(..count.. / (sapply(PANEL, FUN=function(x) sum(count[PANEL == x]))))),
                 binwidth = 8, fill = "grey75", color = "black") +
  facet_wrap(~year, scales = "free_x", ncol = 3) +
  ylab("Frequency [%]") + xlab("Date" ) +
  scale_x_date(breaks = scales::date_breaks('1 month'), labels = scales::date_format("%b"),
               limits = doytodate(c(1,365), 2000)) +
  theme(axis.text.x = element_text(angle = 45, hjust  = 1 , vjust = 1))

pl +
  theme(plot.title = element_text(family = "AvantGarde")) +
  ggtitle("")
