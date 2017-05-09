# Function to plot comparisons between PhenoRice areas and official statistics


# in_areafile = "D:/Documents/Personal/Articles/abstracts/ESA_LP_2016/Poster/Areas.csv"
# areadata = read_delim(in_areafile, delim = ';')
# # areamelt = melt(areadata, id.vars = c("Year", "MOD_Winter","MOD_total","MOD_Summer"))
# # names (areamelt)[6] = 'x'
# # areamelt = melt(areamelt, id.vars = c("Year", "variable","x"))
# # names (areamelt)[5] = 'y'
# areadata$yy = substr(areadata$Year,3,4)
#
# ggplot(data = areamelt, aes(x = x, y = y, label=y))+
#   stat_smooth(geom="text",method="lm",hjust=0,parse=TRUE) +
#   geom_smooth(method="lm",se=FALSE) +
#   geom_point() + facet_wrap(~variable)
#
#
# p = ggplot(areadata)
# p = p + geom_point(aes(y = MOD_Winter, x = Stat_winter), size = 5)
# p = p + geom_text(aes(y = MOD_Winter, x = Stat_winter, label = yy), hjust=0, vjust=1.1, size = 7)
# p = p + geom_smooth(aes(y = MOD_Winter, x = Stat_winter),method = "lm", se=FALSE, color="orange", formula = y~x, lty = 'dashed')
# p = p + lb_theme_bw() + xlim(-50,100) + ylim (-50,100)
# # p = p + geom_hline(yintercept = 0) + geom_vline(xintercept = 0 )
# p = p + ylab("Phenorice Rice Area\n % difference with 2003 values")
# p = p + xlab("Official Rice Area\n % difference with 2003 values")
# p = p + geom_text(aes(x = -65, y = 130, label = lm_eqn(lm(MOD_Winter ~ Stat_winter,areadata))), parse = TRUE, size = 7)
# p_wet= p + theme(axis.title = element_text(size = 20),
#     axis.text = element_text(size = 20),
#     plot.title = element_text(size = 20))
#
# p = ggplot(areadata)
# p = p + geom_point(aes(y = MOD_Summer, x = Stat_Summer), color = 'red', size = 5)
# p = p + geom_text(aes(y = MOD_Summer, x = Stat_Summer, label = yy), hjust=0, vjust=1.1, size = 7)
# p = p + geom_smooth(aes(y = MOD_Summer, x = Stat_Summer),method = "lm", se=FALSE, color="orange", formula = y~x, lty = 'dashed')
# p = p + lb_theme_bw() + xlim(-50,900) + ylim (-50,900)
# # p = p + geom_hline(yintercept = 0) + geom_vline(xintercept = 0 )
# p = p + ylab("Phenorice Rice Area\n % difference with 2003 values")
# p = p + xlab("Official Rice Area\n % difference with 2003 values")
# p = p + geom_text(aes(x = -65, y = 130, label = lm_eqn(lm(MOD_Summer ~ Stat_Summer,areadata))), parse = TRUE, size = 7)
# p_dry= p + theme(axis.title = element_text(size = 20),
#                  axis.text = element_text(size = 20),
#                  plot.title = element_text(size = 20))
#
# grid.arrange(p_wet, p_dry, ncol = 2)
#
# p = p + geom_point(aes(y = MOD_total, x = Stat_Total), color = 'orange')
# p = p + geom_text(aes(y = MOD_total, x = Stat_Total, label = Year), hjust=0, vjust=1.1)
#
#
#
# lm_eqn = function(m) {
#
#   l <- list(a = format(coef(m)[1], digits = 2),
#             b = format(abs(coef(m)[2]), digits = 2),
#             r2 = format(summary(m)$r.squared, digits = 3));
#
#   if (coef(m)[2] >= 0)  {
#     eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
#   } else {
#     eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
#   }
#
#   as.character(as.expression(eq));
# }
#
