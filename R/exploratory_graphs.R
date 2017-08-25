library(tidyverse)
library(sf)
library(raster)
library(mapview)
library(anytime)
library(tibble)



#   ____________________________________________________________________________
#   plot correlations among MODIS estimates                                 ####


statsdt[statsdt == 0] = NA
numerics = statsdt[,c(2, 3:8, 11:14)]
numerics = statsdt[,c(2, 3:8)]
corr <- round(cor(numerics, use = "complete.obs"), 2)
p.mat <- cor_pmat(numerics, use = "pcomplete.obs")
ggcorrplot(corr, p.mat = p.mat, type = "lower",
           outline.col = "white", lab = T, insig = "blank", show.diag = T)

ggpairs(numerics, mapping = aes(color = nseasons)) + theme_bw()

pigs_types <- list(
  comboHorizontal = wrap(ggally_facethist, binwidth = 8)
)
ggpairs(numerics, mapping = aes(color = nseasons), types = pigs_types) + theme_bw()

statsdt <- statsdt %>%
  filter(NAME_3 != "Meur Momar Sarr")


#   ____________________________________________________________________________
#   plot correlations among MODIS estimates                                 ####

stats_avg <- statsdt %>%
  group_by(NAME_3, year) %>%
  summarize_each(funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE)))


p = ggplot(stats_avg, aes(x = lon_mean, y = sowdoy_wet_mean)) +
  geom_point() + geom_errorbar(aes(ymax = sowdoy_wet_mean + sowdoy_wet_sd, ymin = sowdoy_wet_mean - sowdoy_wet_sd), color = "grey75") +
  facet_wrap(~year)
  geom_text(aes(label = NAME_3), nudge_y = -2)
p + theme_bw()


p = ggplot(statsdt, aes(x = lon, y = sowdoy_wet)) +
  geom_hex(binwidth = c(20000,8)) + facet_wrap(~year)
geom_errorbar(aes(ymax = sowdoy_wet_mean + sowdoy_wet_sd, ymin = sowdoy_wet_mean - sowdoy_wet_sd), color = "grey75") +
  geom_text(aes(label = NAME_3), nudge_y = -2)
p + theme_bw()

#   ____________________________________________________________________________
#   plot regression of variables against time                               ####
#
stats_avg <- statsdt %>%
  # filter(NAME_3 %in% c("Thile Boubacar")) %>%
  group_by(year, nseasons) %>%
  filter(nseasons == 2) %>%
  summarize_each(funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE)))

p = ggplot(stats_avg, aes(x = harvdoy_dry_mean, y = sowdoy_wet_mean, color = nseasons, fill = nseasons)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  geom_text(aes(label = year)) +
  theme_bw()
p

p = ggplot(stats_avg, aes(x = year, y = harvdoy_dry_mean)) + geom_point(color = "red")+ geom_point (aes(y = sowdoy_wet_mean), color = "blue")


statsdt_melt <- stats_avg %>%
  select(c(1, 2:8, 11:14)) %>%
  gather(ke, value, -year, -nseasons)

p <- ggplot(statsdt_melt, aes(x = as.numeric(year), y = value)) +
  geaom_point() + geo
  facet_wrap(~ke) +
  theme_bw()


  my.formula <- y ~ x
  p <- ggplot(data = statsdt_melt, aes(x = as.numeric(as.character(year), color = nseasons), y = value)) +
    facet_wrap(~ke, scales = "free", ncol = 2) +
    geom_smooth(method = "lm", se=T, color="black", formula = my.formula) +
    stat_poly_eq(formula = my.formula,
                 eq.with.lhs = "italic(hat(y))~`=`~",
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")),
                 parse = TRUE) +
   p = p + geom_point(aes(color = nseasons))  + theme_bw()
  p


  p <- ggplot(data = subset(statsdt_melt, ke %in% c("sowdoy_dry_mean",  "sowdoy_wet_mean",
                                                    "harvdoy_dry_mean", "harvdoy_wet_mean",
                                                    "lgttot_dry_mean", "lgttot_wet_mean")) ,
                            aes(x = as.numeric(as.character(year)), y = value, fill = nseasons, color = nseasons)) +
    geom_point()  + theme_bw() +
    facet_wrap(~ke, scales = "free", ncol = 2) +
    geom_smooth(method = "lm", se = T, formula = my.formula) +
    stat_poly_eq(formula = my.formula,
                 eq.with.lhs = "italic(hat(y))~`=`~",
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")),
                 parse = TRUE)
  p



  #   ____________________________________________________________________________
  #   grafici analisi vari su stime MODIS                                     ####


  stats_summaries <- statsdt %>%
    as_tibble() %>%
    mutate(year          = as.character(year),
           nseasons      = as.character(nseasons),
           ID_3          = as.numeric(ID_3),
           sowdate_dry   = doytodate(sowdoy_dry, year),
           sowdate_wet   = doytodate(sowddoy_wet, year),
           flowdate_dry  = doytodate(flowdoy_dry, year),
           flowdate_wet  = doytodate(flowdoy_wet, year),
           harvdate_dry  = doytodate(harvdoy_dry, year),
           harvdate_wet  = doytodate(harvdoy_wet, year),
           sowdoy_dry_t  = doytodate(sowdoy_dry, 2007),
           sowdoy_wet_t  = doytodate(sowddoy_wet, 2007),
           flowdoy_dry_t = doytodate(flowdoy_dry, 2007),
           flowdoy_wet_t = doytodate(flowdoy_wet, 2007),
           harvdoy_dry_t = doytodate(harvdoy_dry, 2007),
           harvdoy_wet_t = doytodate(harvdoy_wet, 2007)
    ) %>%
    left_join(gadm_data@data, by = "ID_3") %>%
    filter(!is.na(ID_3)) %>%
    select(-c(ISO, NAME_0, ID_0, ID_1, NAME_1, ID_2, NAME_2, NL_NAME_3, VARNAME_3, TYPE_3, ENGTYPE_3))

  stats_summ <- stats_summaries %>%
    group_by(NAME_3) %>%
    summarize(avg_date = mean(sowdoy_wet_t, na.rm = T),
              sd_date = sd(sowddoy_wet, na.rm = T),
              avgdoy = mean(sowddoy_wet, na.rm = T),
              count = n(),
              xmin = xmin[1]) %>%
    arrange(xmin, NAME_3) %>%
    mutate(NAME_3 = factor(NAME_3,NAME_3)) %>%
    mutate(ymax = avg_date + sd_date, ymin = avg_date - sd_date) %>%
    filter(NAME_3 != "Keur Momar Sarr")

  p <- ggplot(stats_summ, aes(x = year, y = avg_date)) +
    geom_point() + theme_bw() + geom_errorbar(aes(ymin = ymin, ymax = ymax))+
    facet_wrap(~NAME_3)
  p


  prolm = stats_summ %>%
    # group_by(year) %>%
    do(glance(lm(avgdoy~xmin, data = .)))

  %>%
    glance()


  stats_summ <- stats_summaries %>%
    group_by(NAME_3, nseasons) %>%
    summarize(avg_date = mean(lgtveg_wet, na.rm = T),
              sd_date = sd(lgtveg_wet, na.rm = T),
              avgdoy = mean(lgtveg_wet, na.rm = T),
              count = n(),
              xmin = xmin[1]) %>%
    arrange(xmin,  NAME_3) %>%
    mutate(NAME_3 = factor(NAME_3,NAME_3)) %>%
    mutate(ymax = avg_date + sd_date, ymin = avg_date - sd_date) %>%
    filter(NAME_3 != "Keur Momar Sarr")

  p <- ggplot(stats_summ, aes(x = year, y = avg_date)) +
    geom_point() + theme_bw() + geom_errorbar(aes(ymin = ymin, ymax = ymax))+
    facet_wrap(~NAME_3)
  p


  stats_summ <- stats_summaries %>%
    group_by(NAME_3, year) %>%
    summarize(area_dry = length(which(is.na(sowdate_dry) == FALSE)),
              area_wet = length(which(is.na(sowdate_dry) == T)),
              xmin = xmin[1]) %>%
    arrange(xmin,  NAME_3) %>%
    filter(NAME_3 != "Keur Momar Sarr") %>%
    gather(key, value,-c(NAME_3,xmin,year))

  p <- ggplot(stats_summ, aes(x = year, y = value, fill = key)) +
    geom_bar(stat = "identity") + theme_bw() +
    facet_wrap(~NAME_3, scales = "free_y")
  p


