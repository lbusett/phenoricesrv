install.packages("rworldmap")
library(rworldmap)
library(sf)
library(dplyr)
library(sprawl)

mapped_data <- joinCountryData2Map(population_data, joinCode = "ISO1",
                                   nameJoinColumn = "Country.Code")
borders <- sf::st_as_sf(countriesLow) %>%
  dplyr::filter(ADMIN %in% c("Senegal", "Mauritania", "Mali", "Guinea-Conakry"))

plot_vect(borders)
