library(readxl)
library(ggplot2)
library(extrafont)
library(tidyverse)
library(reshape2)
loadfonts(device = "win")

rm(list = ls())

#read and manipulate date
xls <- "C:/Users/user/Desktop/¡¯SF_2_1_Fertility_rates_LUNG.xlsx"
df <- read_xlsx(xls, sheet = 1, range = "L5:Q42", col_names = TRUE)
head(df)
colnames(df) <- c("country", "NA", "replacement rate", "1970", "1995", "2016")
colnames(df)

#new variable
df <- df %>% mutate(
    member = case_when(
        country == "Taiwan" ~ "Taiwan", 
        country == "OECD average" ~ "A", 
        TRUE ~ "OECD"
        )
)


#axis color condition
axiscolor <- ifelse(df$country == "Taiwan", "tan4", ifelse(df$country == "OECD average", "springgreen4", "black"))

#gg
gg <- ggplot(data = df, aes(x = reorder(`country`, `2016`)))

#barplot
bar <- geom_bar(aes(y = `2016`, fill = `member`), colour = "snow1", stat = "identity", width = 0.5)

#points
c <- melt(df[ , c("country", "1970", "1995", "replacement rate")], id.vars = "country")
point <- geom_point(data = c, aes(x = `country`, y = `value`, fill = `variable`, 
                                  shape = `variable`), colour = "snow1", size = 3)

#custom fill and color
fill <- scale_fill_manual("AAA",
                          labels = c("a", "b", "c"), 
                          #labels = c("f2" = "1995", "f3" = "1997", "f4" = "replacement rate"), 
                          values = c("Taiwan" = "tan1", "A" = "springgreen3", "OECD" = "turquoise2", 
                                     "1970" = "grey", "1995" = "green", "replacement rate" = "red"), 
                          breaks = c("1970", "1995", "replacement rate")
                          )

colour <- scale_colour_manual("AAA", 
                              labels = c("a", "b", "c") 
                              #labels = c("Taiwan" = "Taiwan", "A" = "OECD-average", "T" = "OECD", "c2" = "1995", "c3" = "1997", "c4" = "replacement rate"), 
                              )

shape <- scale_shape_manual("AAA", 
                            labels = c("a", "b", "c"), 
                            #labels = c("s2" = "1995", "s3" = "1997", "s4" = "replacement rate"), 
                            values = c(23, 23, 24)
                            )
    
#labels
lab <- labs(title = "Chart SF2.1.A. Total fertility rate, 1970, 1995 and 2016 or latest available", 
            subtitle = "Average number of children born per woman over a lifetime given current age-specific fertility rates and assuming no female mortality during reproductive years", 
            size = 12
            )
    
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

p <- gg + bar + point + shape + fill + lab + theme + axis.y

p

#export the plot
ggsave(filename = "C:/Users/user/Desktop/tst2_plot.png", width = 8.68, height = 5.89, dpi = 600)
