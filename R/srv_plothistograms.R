# Function to plot histogrms of sowing dates
#
#
sow_data_all = rbind(sow_data_dry, sow_data_wet)
sow_data_all$Date = lb_doytodate(sow_data_all$DOY, 2000)
p = ggplot(sow_data_all) + lb_theme_bw()
p = p + geom_histogram(aes (x = DOY), binwidth = 8) + facet_wrap(~year)
p


p = ggplot(sow_data_all, aes(Date))+theme_bw()
# p = p + geom_histogram(aes(y=100*..count../sum(..count..)),alpha=.5, position = 'identity', breaks =as.numeric(seq(min(sow_data_all$Date),max(sow_data_all$Date),'8 day')), color = 'black')
p = p + geom_histogram(aes(y =100*(..count.. / (sapply(PANEL, FUN=function(x) sum(count[PANEL == x]))))),alpha=.5, position = 'identity', breaks =as.numeric(seq(min(sow_data_all$Date),max(sow_data_all$Date),'8 day')), color = 'black')
# p = p + geom_density(aes(y = 100*(..count../(sapply(PANEL, FUN=function(x) sum(count[PANEL == x]))))), position = 'identity', fill = 'transparent', adjust = 1.2)
p = p + theme(plot.title = element_text(vjust = 1, hjust = 0)) + facet_wrap(~year)
p2_mins = p + theme(axis.title = element_text(size = 20)) + ylab('Frequency [%]') +
  scale_x_date(breaks = date_breaks('2 month'), labels = date_format("%b")) +
  theme(axis.text.x = element_text(angle = 45, hjust  = 1 , vjust = 1))
p2_mins + theme(axis.text = element_text(size = 18)) + theme(strip.text.x= element_text(size = 18))

