library(readxl)
library(ggplot2)
library(extrafont)
loadfonts(device = "win")

rm(list = ls())

xls <- "C:/Users/user/Desktop/¡¯SF_2_1_Fertility_rates_LUNG.xlsx"
df <- as.data.frame(read_xlsx(xls, sheet = 1, range = "L5:R42", col_names = TRUE))
#colnames(df)
#cols <- c("country", "replacement rate", "1970", "1995", "2016", "OECD")
#df[ , cols] <- apply(df[ , cols], 2, as.factor)
#class(df$country)
#df <- fortify(df[order(df$"2016"), ])

#axis color condition
axiscolor <- ifelse(df$country == "Taiwan", "tan4", ifelse(df$country == "OECD average", "springgreen4", "black"))

gg <- ggplot(data = df, aes(x = reorder(`country`, `2016`)))

#barplot
bar <- geom_bar(aes(y = `2016`, fill = `OECD`, colour = "c1"), stat = "identity", width = 0.5)

#hline replacement rate
#hline <- geom_hline(aes(color = "red", yintercept = 2.1), size = 0.5, linetype = "longdash", show.legend = FALSE)

#point 1995
point1 <- geom_point(aes(y = `1995`, fill = "f2", colour = "c2", shape ="s1"), size = 2, show.legend = TRUE)
#point 1997
point2 <- geom_point(aes(y = `1970`, fill = "f3", colour = "c3", shape = "s2"), size = 2, show.legend = TRUE)
#point replacement rate
point3 <- geom_point(aes(y = `replacement rate`, fill = "f4", colour = "c4", shape = "s3"), size = 2, show.legend = TRUE)

#custom fill and color
fill <- scale_fill_manual("", 
                          labels = c("Taiwan" = "Taiwan", "A" = "OECD-average", "T" = "OECD", "f2" = "1995", "f3" = "1997", "f4" = "replacement rate"), 
                          values = c("Taiwan" = "tan1", "A" = "springgreen3", "T" = "turquoise2", "f2" = "whitesmoke", "f3" = "black", "f4" = "red"),
                          guide = "legend")

colour <- scale_colour_manual(values = c("c1" = "snow4", "c2" = "snow4", "c3" = "snow4", "c4" = "red"), guide = FALSE)

shape <- scale_shape_manual(values = c("s1" = 23, "s2" = 23, "s3" = 3), guide = "legend")
    
#labels
lab <- labs(title = "Chart SF2.1.A. Total fertility rate, 1970, 1995 and 2016 or latest available", 
            subtitle = "Average number of children born per woman over a lifetime given current age-specific fertility rates and assuming no female mortality during reproductive years", 
            size = 12)
    
#customization
theme <- theme(axis.text.x = element_text(family = "mono", angle = 45, hjust = 1, vjust = 1, colour = axiscolor), 
               plot.title = element_text(family = "sans", size = 14), #title
               plot.subtitle = element_text(family = "sans", size = 8), #subtitle
               axis.title = element_blank(), #axis title
               axis.line = element_line(colour = "snow4", size = 1, linetype = "solid"), 
               axis.ticks.y = element_line(colour = "snow4", size = 1, linetype = "solid"), 
               axis.ticks.length = unit(0.3, "cm"), 
               axis.ticks.x = element_blank(), 
               #panel.grid.minor = element_line(size = 0.1, colour = "grey28"), 
               #panel.grid.major= element_blank(), 
               panel.grid = element_blank(), 
               panel.background = element_blank(), 
               legend.position = "top"
               )
#axis y
axis.y <- scale_y_continuous(expand = c(0, 0), limits = c(0, 6))

p <- gg + bar + point1 + point2 + point3 + fill + colour + shape + lab + theme + axis.y #+ guides(guide_legend(override.aes = list(shape = NA)))
p

#export the plot
ggsave(filename = "C:/Users/user/Desktop/tst2_plot.png", width = 8.68, height = 5.89, dpi = 600)