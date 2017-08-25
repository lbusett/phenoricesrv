library(tidyverse)
library(sf)
library(raster)
library(mapview)
library(anytime)
library(tibble)

rastfold <- "/home/lb/nr_working/shared/PhenoRice/Processing/Senegal/may_2017/Outputs_MaskIrrig/2014/"
subareas <- readshape("/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Datasets/Pheno_Validation/subareas.shp")

inpheno <- read.csv2("/home/lb/Google_Drive/IREA/Conference&paper/PhenoRice_SRV/Datasets/Pheno_Validation/Pheno_Validation_data_arrondissements.csv", stringsAsFactors = TRUE)
inpheno <- as_tibble(inpheno) %>%
  mutate(sowdate = as.Date(sowdate, format = "%d/%m/%Y"),
         harvdate =  as.Date(harvdate, format = "%d/%m/%Y")) %>%
  filter(season == "Hivern") %>%
  mutate(year = as.numeric(year)) %>%
  mutate(sowdoy = datetodoy(sowdate), harvdoy = datetodoy(harvdate)) %>%
  mutate(sowddate_tmp = doytodate(sowdoy, 2007), harvdate_tmp = doytodate(harvdoy, 2007)) %>%
  mutate(village = factor(village))


avg_sow <- inpheno %>%
  group_by(year, Admin.area) %>%
  summarize(avgsow = mean(sowddate_tmp, na.rm = TRUE),
            sdsow  = sd(sowdoy, na.rm = TRUE),
            count = n()) %>%
  mutate(Admin.area = as.factor(Admin.area)) %>%
  filter(Admin.area != "") %>%
  droplevels()

  levels(avg_sow$Admin.area)[3] = "Thille Boubacar"
  names(avg_sow)[2] = "NAME_3"

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
  select(-c(ISO, NAME_0, ID_0, ID_1, NAME_1, ID_2, NAME_2, NL_NAME_3, VARNAME_3, TYPE_3, ENGTYPE_3)) %>%
  mutate(NAME_3 = as.factor(NAME_3)) %>%
  filter(NAME_3 %in% levels(avg_sow$NAME_3)) %>%
  droplevels()

pro =  statsdt %>%
  filter(NAME_3 %in% levels(avg_sow$NAME_3)) %>%
  droplevels()

#   ____________________________________________________________________________
#   HERE !!!!                                                               ####



avg_sow_mod <- pro %>%
  mutate(year = as.numeric(as.character(year))) %>%
  group_by(year, NAME_3) %>%
  summarize(avgsow_mod = mean(sowdate_wet_t, na.rm = TRUE),
            sdsow_mod  = sd(sowdoy_wet, na.rm = TRUE),
            count = n())


p <- ggplot() + theme_bw()
p <- p +  geom_point(data = avg_sow, aes(x = year-0.05, y = avgsow), color = "red") +
          geom_errorbar(data = avg_sow, aes(x = year-0.05, ymin = avgsow - sdsow, ymax = avgsow + sdsow), color = "grey75")
p <- p +  geom_point(data = avg_sow_mod, aes(x = year+0.05, y = avgsow_mod), color = "blue") +
  geom_errorbar(data = avg_sow_mod, aes(x = year+0.05, ymin = avgsow_mod - sdsow_mod, ymax = avgsow_mod + sdsow_mod), color = "grey75")

p <- p + facet_wrap(~NAME_3, ncol = 1)+ ylim(as.Date("2007-06-01"), as.Date("2007-10-01"))+ xlim(2002,2016)
p




pro =  statsdt %>%
  filter(NAME_3 %in% levels(avg_sow$NAME_3)) %>%
  filter(valid_delta == 1) %>%
  droplevels() %>%
  filter (NAME_3 == "Ndiaye Mberess")



#   ____________________________________________________________________________
#   validation SOW  Delta                                                   ####

avg_sow_mod <- pro %>%
  mutate(year = as.numeric(as.character(year))) %>%
  group_by(year, NAME_3) %>%
  summarize(avgsow_mod = mean(sowdate_wet_t, na.rm = TRUE),
            sdsow_mod  = sd(sowdoy_wet, na.rm = TRUE),
            count = n())


avgsowsub = avg_sow %>%
  filter(NAME_3 == "Ndiaye Mberess")
p <- ggplot() + theme_bw()
p <- p +  geom_point(data = avgsowsub, aes(x = year-0.05, y = avgsow), color = "red") +
  geom_errorbar(data = avgsowsub, aes(x = year-0.05, ymin = avgsow - sdsow, ymax = avgsow + sdsow), color = "grey75")
p <- p +  geom_point(data = avg_sow_mod, aes(x = year+0.05, y = avgsow_mod), color = "blue") +
  geom_errorbar(data = avg_sow_mod, aes(x = year+0.05, ymin = avgsow_mod - sdsow_mod, ymax = avgsow_mod + sdsow_mod), color = "grey75")

p <- p + facet_wrap(~NAME_3, ncol = 1)+ ylim(as.Date("2007-06-01"), as.Date("2007-10-01"))+ xlim(2002,2016)
p


#   ____________________________________________________________________________
#   validation harvets  Delta                                                   ####

avg_harv_mod <- pro %>%
  mutate(year = as.numeric(as.character(year))) %>%
  group_by(year, NAME_3) %>%
  summarize(avgharv_mod = mean(harvdate_wet_t, na.rm = TRUE),
            sdharv_mod  = sd(harvdoy_wet, na.rm = TRUE),
            count = n())

avg_harv <- inpheno %>%
  group_by(year, Admin.area) %>%
  summarize(avgharv = mean(harvdate_tmp, na.rm = TRUE),
            sdharv  = sd(harvdoy, na.rm = TRUE),
            count = n()) %>%
  mutate(NAME_3 = as.factor(Admin.area)) %>%
  filter(NAME_3 != "") %>%
  droplevels()
levels(avg_harv$NAME_3) = c("Gamadji Sarre", "Ndiaye Mberess", "Thile Boubacar")

avgharvsub = avg_harv %>%
  filter(NAME_3 == "Ndiaye Mberess")


p <- ggplot() + theme_bw()
p <- p +  geom_point(data = avgharvsub, aes(x = year-0.05, y = avgharv), color = "red") +
  geom_errorbar(data = avgharvsub, aes(x = year-0.05, ymin = avgharv - sdharv, ymax = avgharv + sdharv), color = "grey75")

p <- p +  geom_point(data = avg_harv_mod, aes(x = year+0.05, y = avgharv_mod), color = "blue") +
  geom_errorbar(data = avg_harv_mod, aes(x = year+0.05, ymin = avgharv_mod - sdharv_mod, ymax = avgharv_mod + sdharv_mod), color = "grey75")

p <- p + facet_wrap(~NAME_3, ncol = 1)+ ylim(as.Date("2007-06-01"), as.Date("2007-10-01"))+ xlim(2002,2016)
p






p2 <- ggplot() + theme_bw()
p2 <- p2 +  geom_point(data = avg_sow_mod, aes(x = year, y = avgsow_mod), color = "red") +
  geom_errorbar(data = avg_sow_mod, aes(x = year, ymin = avgsow_mod - sdsow_mod, ymax = avgsow_mod + sdsow_mod), color = "grey75")
p2 <- p2 + facet_wrap(~NAME_3, ncol = 1) + ylim(as.Date("2007-06-01"), as.Date("2007-10-01"))+ xlim(2002,2016)


names(avg_sow_mod)[3:4] = c("avgsow_mod", "sdsow_mod")

pro = left_join(avg_sow, avg_sow_mod, by = c("year", "NAME_3"))

p3 = ggplot(pro, aes(x = avgsow, y = avgsow_mod)) +
  geom_point() + geom_smooth(method = "lm") + theme_bw() + facet_wrap(~NAME_3, scales = "free")+
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")),
               parse = TRUE) +
  geom_point()  + theme_bw()
p3

cor  = pro %>%
  group_by(NAME_3) %>%
  do(glance(lm(datetodoy(avgsow)~datetodoy(avgsow_mod), data = .)))


p3

p3 = ggplot(pro, aes(x = avgsow, y = avgsow_mod)) +
  geom_point() + geom_smooth(method = "lm") + theme_bw()
p3



library(gridExtra)
grid.arrange(p,p2, ncol = 2)
  geom_point() + theme_bw() + geom_errorbar(aes(ymin = ymin, ymax = ymax))


p <- ggplot(inpheno2, aes(x = village, y = count, fill = site))
p <- p + geom_bar(stat = "identity") + theme_bw()
p <- p + coord_flip() + facet_wrap(~year)
p




my.formula <- y ~ x
p <- ggplot(data = avg_sow_mod, aes(x = year, y = avgsow_mod)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")),
               parse = TRUE) +
  geom_point()  + theme_bw()
p



#   ____________________________________________________________________________
#   grafici analisi vari su stime MODIS                                     ####

p1 = ggplot(subset(statsdt2, variable == "sowdoy_wet_t"), aes(x = NAME_3, y = doytodate(value, 2007)))
p1 = p1 + geom_boxplot(outlier.colour = "transparent") +
  facet_wrap(~year) + theme_bw() + scale_y_date()
p1  + geom_jitter(alpha = 0.03)

+ coord_flip()



statsdt <- rbindlist(stats)
names(statsdt) <- c("year", "nseasons", "sowdoy_dry", "sowddoy_wet", "flowdoy_dry", "flowdoy_wet",
                    "harvdoy_dry", "harvdoy_wet", "cumEVI_dry", "cumEVI_wet", "lgtveg_dry", "lgtveg_wet",
                    "lgttot_dry", "lgttot_wet", "ID_3")

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


