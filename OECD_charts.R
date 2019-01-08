library(readxl)
library(ggplot2)
library(extrafont)
loadfonts(device = "win")

rm(list = ls())

xls <- "C:/Users/user/Desktop/¡¯SF_2_1_Fertility_rates_LUNG.xlsx"
df <- as.data.frame(read_xlsx(xls, sheet = 1, range = "L5:Q42", col_names = TRUE))
df <- fortify(df[order(df$"2016"), ])

#axis color condition
axiscolor <- ifelse(df$country == "Taiwan", "red", ifelse(df$country == "OECD average", "springgreen4", "black"))

#bar color conditoin
barcolor <- ifelse(df$country == "Taiwan", "red", ifelse(df$country == "OECD average", "springgreen3", "steelblue"))

p <- ggplot(data = df, aes(x = reorder(`country`, `2016`))) + 
        #barplot
        geom_bar(aes(y = `2016`, fill = barcolor, colour = "2016"), stat = "identity", 
             width = 0.5) + 
        #point 1995
        geom_point(aes(y = `1995`), fill = "white", colour = "snow4", shape = 23, size = 2.5) + 
        #point 1970
        geom_point(aes(y = `1970`), fill = "black", colour = "snow4", shape = 23, size = 2.5) +  
        #
        scale_colour_manual(values = c("snow4" = "snow4", "white" = "white", "black" = "black")) + 
        #
        geom_hline(yintercept = 2.1, size = 0.5, colour = "grey37", linetype = "longdash") +
    
    #labels
    labs(title = "Chart SF2.1.A. Total fertility rate, 1970, 1995 and 2016 or latest available", 
         subtitle = "Average number of children born per woman over a lifetime given current age-specific fertility rates and assuming no female mortality during reproductive years", 
         size = 12) +
    
    #customization
    theme(axis.text.x = element_text(family = "mono", angle = 45, hjust = 1, vjust = 1, colour = axiscolor), 
          plot.title = element_text(family = "sans", size = 14), #title
          plot.subtitle = element_text(family = "sans", size = 8), #subtitle
          axis.title = element_blank(), #axis title
          axis.line = element_line(colour = "snow4", size = 1, linetype = "solid"), 
          axis.ticks.y = element_line(colour = "snow4", size = 1, linetype = "solid"), 
          axis.ticks.length = unit(0.3, "cm"), 
          axis.ticks.x = element_blank(), 
          panel.grid.minor = element_line(size = 0.1, colour = "grey28"), 
          panel.background = element_blank(), 
          panel.grid.major= element_blank(), 
          legend.position = "top"
          ) + 
    
    #y_scale
    scale_y_continuous(expand = c(0, 0), limits = c(0, 6))
    
    #legend
    #scale_color_manual("", guide = "legend", values = c("steelblue", "white", "black"))

#export the plot
ggsave(filename = "C:/Users/user/Desktop/tst2_plot.png", width = 8.68, height = 5.89, dpi = 600)
