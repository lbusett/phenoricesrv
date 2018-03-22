sos_valid_data$site[which(sos_valid_data$site == "Boundoum (delta)")] = "Boundoum Irrigation Scheme"

pp <- ggplot(subset(sos_valid_data, site == "Boundoum Irrigation Scheme"), aes(x = year - 0.2, y = avg)) +
  geom_point(color = "darkblue") +
  geom_errorbar(aes(x = year - 0.2, ymin = avg - sd, ymax = avg + sd), color = "darkblue", width = 0.3) +
  geom_point(aes(x = year + 0.2, y = avg_mod), color = "darkred") +
  geom_errorbar(aes(x = year + 0.2, ymin = avg_mod - sd_mod, ymax = avg_mod + sd_mod), color = "darkred",
                , width = 0.3) +
  theme_light() +
  scale_y_date(limits = c(as.Date("2007-06-23") , as.Date("2007-10-07")), date_labels = "%d-%b") +
  scale_x_continuous("Year", limits = c(2002.6, 2009.4),
                     breaks = 2003:2009) +
  scale_color_manual("", values = c("darkblue", "darkred")) +
  ggtitle("Observed vs PhenoRice Sowing Dates") +
  theme(plot.title = element_text(family = "AvantGarde")) +
  facet_wrap(~site) +
  ylab("Sowing Date") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ggplot2::theme(panel.grid.minor = element_line(color = "transparent"))


eos_valid_data$site[which(eos_valid_data$site == "Boundoum (delta)")] = "Boundoum Irrigation Scheme"
pp <- ggplot(subset(eos_valid_data, site == "Boundoum Irrigation Scheme"), aes(x = year - 0.2, y = avg)) +
  geom_point(color = "darkblue") +
  geom_errorbar(aes(x = year - 0.2, ymin = avg - sd, ymax = avg + sd), color = "darkblue", width = 0.3) +
  geom_point(aes(x = year + 0.2, y = avg_mod), color = "darkred") +
  geom_errorbar(aes(x = year + 0.2, ymin = avg_mod - sd_mod, ymax = avg_mod + sd_mod), color = "darkred",
                , width = 0.3) +
  theme_light() +
  scale_y_date("Harvesting Date", limits = c(as.Date("2007-10-15") , as.Date("2008-02-01")), date_labels = "%d-%b") +
  scale_x_continuous("Year", limits = c(2002.6, 2009.4),
                     breaks = 2003:2009) +
  scale_color_manual("", values = c("darkblue", "darkred")) +
  ggtitle("Observed vs PhenoRice Harvesting Dates") +
  theme(plot.title = element_text(family = "AvantGarde")) +
  facet_wrap(~site) +
  ylab("Harvesting Date") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ggplot2::theme(panel.grid.minor = element_line(color = "transparent"))