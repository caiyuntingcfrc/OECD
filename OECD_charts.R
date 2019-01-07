library(readxl)
library(ggplot2)
library(extrafont)
loadfonts(device = "win")

rm(list = ls())

xls <- "C:/Users/user/Desktop/¡¯SF_2_1_Fertility_rates_LUNG.xlsx"
df <- as.data.frame(read_xlsx(xls, sheet = 1, range = "L5:Q42", col_names = TRUE))
df <- fortify(df[order(df$"2016"), ])

axiscolor <- ifelse(df$country == "Taiwan", "peachpuff", ifelse(df$country == "OECD average", "lightgreen", "black"))
barcolor <- ifelse(df$country == "Taiwan", "peachpuff", ifelse(df$country == "OECD average", "lightgreen", "lightblue"))

p <- ggplot(data = df) + 
    
    geom_bar(aes(x = reorder(country, `2016`), y = `2016`), stat = "identity", fill = alpha(barcolor, 1), color = "grey28", 
             width = 0.5) + 
    
    #point 1995
    geom_point(aes(x = `country`, y = `1995`), shape = 23, size = 2.5, fill = alpha("white", 0.8), colour = "black") + 
    
    #point 1970
    geom_point(aes(x = `country`, y = `1970`), shape = 23, size = 2.5, fill = alpha("black", 0.8), colour = "black") + 
    
    labs(title = "Chart SF2.1.A. Total fertility rate, 1970, 1995 and 2016 or latest available", 
         subtitle = "Average number of children born per woman over a lifetime given current age-specific fertility rates and assuming no female mortality during reproductive years", 
         size = 12) + 
    
    guides() +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, colour = axiscolor), 
          plot.title = element_text(size = 14), 
          plot.subtitle = element_text(size = 8), 
          axis.title = element_blank(), 
          text = element_text(family = "serif", size = 14), 
          axis.line = element_line(color = "black", size = 1, linetype = "solid"), 
          panel.grid.minor = element_line(size = 0.1, colour = "grey28"), 
          panel.background = element_blank(), 
          panel.grid.major= element_blank() 
          
          ) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 6))
p

#export the plot
ggsave(filename = "C:/Users/user/Desktop/tst_plot.png", dpi = 600)

