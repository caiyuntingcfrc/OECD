library(readxl)
library(ggplot2)
library(extrafont)
library(tidyverse)
library(reshape2)
loadfonts(device = "win")

rm(list = ls())

#####references#####
#aes specs
vignette("ggplot2-specs")
####################

#read and manipulate data
xls <- "C:/Users/user//Desktop/¡¯SF_2_1_Fertility_rates_LUNG.xlsx"
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

df$member <- factor(df$member)

#axis color condition
axiscolor <- ifelse(df$country == "Taiwan", "tan4", 
                    ifelse(df$country == "OECD average", "springgreen4", "black"))
#Plot
p <- ggplot() +
    #barplot
    geom_bar(data = df, aes(x = reorder(`country`, `2016`), y = `2016`, fill = `member`), 
             colour = "grey", stat = "identity", width = 0.7) + 
    #points
    geom_point(data = melt(df[ , c("country", "1970", "1995")], id.vars = "country"), 
               aes(x = `country`, y = `value`, colour = `variable`, fill = `variable`, 
                   shape = `variable`), size = 3, inherit.aes = FALSE) + 
    #geom_point(data = c, aes(x = `country`, y = `value`, shape = `variable`), inherit.aes = FALSE)
    #line
    geom_line(data = df, aes(x = `country`, y = 2.1, linetype = "solid"), 
              colour = "red", group = 1, size = 1) + 
    #custom fill and color
    scale_fill_manual("", labels = c("Taiwan" = "Taiwan 2016 ¡ù", "A" = "OECD average", 
                                          "OECD" = "OECD 2016 ¡ù"), 
                      #labels = c("f2" = "1995", "f3" = "1997", "f4" = "replacement rate"), 
                      values = c("Taiwan" = "tan1", "A" = "springgreen3", 
                                 "OECD" = "turquoise2", "1970" = "black", "1995" = "snow1"), 
                      breaks = c("Taiwan", "A", "OECD")
                      ) + 
    scale_colour_manual("child per woman", labels = c("1970" = "1970", "1995" = "1995"),
                        values = c("1970" = "black", "1995" = "black")
                        ) + 
    scale_shape_manual("child per woman", 
                       labels = c("1970" = "1970", "1995" = "1995"),
                       values = c("1970" = 23, "1995" = 23)
                       ) + 
    scale_linetype_manual("", 
                          labels = c("populationi replacement rate"), 
                          values = c("solid")) +
    guides(fill = guide_legend(override.aes = list(shape = NA)),
           colour = guide_legend(override.aes = list(fill = c("black", "snow1")))
           ) +
    #labels
    labs(title = "Chart SF2.1.A. Total fertility rate, 1970, 1995 and 2016 or latest available", 
         subtitle = "Average number of children born per woman over a lifetime given current age-specific fertility rates and assuming no female mortality during reproductive years", 
         caption = "source: "
         ) + 
    #customization
    theme(axis.text.x = element_text(family = "mono", angle = 45, hjust = 1, vjust = 1, colour = axiscolor), 
          plot.title = element_text(family = "sans", size = 14), #title
          plot.subtitle = element_text(family = "sans", size = 12), #subtitle
          plot.caption = element_text(family = "serif", size = 8, hjust = 0, vjust = -1), 
          axis.title = element_blank(), #axis title
          axis.line = element_line(colour = "snow4", size = 1, linetype = "solid"), 
          axis.ticks.y = element_line(colour = "snow4", size = 1, linetype = "solid"), 
          axis.ticks.length = unit(0.3, "cm"), 
          axis.ticks.x = element_blank(), 
          panel.grid.minor = element_line(size = 0.1, colour = alpha("grey28", 0.1)), 
          #panel.grid.major= element_blank(), 
          #panel.grid = element_blank(), 
          panel.background = element_blank(), 
          legend.position = "top",
          legend.background = element_blank()
          ) + 
    #axis y
    scale_y_continuous(expand = c(0, 0), limits = c(0, 6)) ; p
#export the plot
ggsave(filename = "C:/Users/user/Desktop/tst2_plot.png", width = 8.68, height = 5.89, dpi = 600)