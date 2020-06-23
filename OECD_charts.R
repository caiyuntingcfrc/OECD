
# prep and options --------------------------------------------------------

# rm and clear console
rm(list = ls()); cat("\14")
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
# setwd
setwd("d:/R_wd/")
# option: scipen
options(scipen = 999)
# library
ins.pack("readxl", "ggplot2", "tidyverse", 
         "extrafont", "reshape2", 
         "plotly")
# fonts
loadfonts(device = "pdf")

#####references#####
#aes specs
vignette("ggplot2-specs")


# # SF 1.1 Family size and Composition --------------------------------------
# 
# # read excel file
# xls <- "demo_dashboard/[2020_May_11]SF_1_1_Family_size_and_composition graph update.xlsx"
# df <- read_xlsx(xls, 
#                 # worksheet 1
#                 sheet = 1, 
#                 # range
#                 range = "L5:P50", 
#                 # column names
#                 col_names = c("country", 
#                               NA_character_, 
#                               "All households", 
#                               "Couple households with children", 
#                               "Single parent households with children")) %>% 
#     # remove empty column
#     select(-2)
# 
# # save file (rds format)
# saveRDS(df, file = "SF1.1.rds")

# read file ---------------------------------------------------------------

df <- read_rds("d:/R_wd/demo_dashboard/SF1.1/SF1.1.rds")

df[ , 2:4] <- sapply(df[ , 2:4], round, 2)

df$id <- NULL
df$country <- as.character(df$country)

lapply(df, class)
df <- df[rowSums(is.na(df)) != ncol(df), ]

df <- df %>% 
    select(-c(`Couple households with children`, `Single parent households with children`)) %>% 
    mutate(country = factor(country, levels = country), 
           member = case_when(country == "Taiwan" ~ "Taiwan", 
                              country == grep("^OECD", df$country, value = TRUE) ~ "OECD-average", 
                              row.names(df) %in% 39:46 ~ "non-OECD", 
                              TRUE ~ "OECD"), 
           group = case_when(member == "Taiwan" ~ 0, 
                             member == "OECD-average" ~ 0,
                             member == "OECD" ~ 0,
                             TRUE ~ 1
                             )
           )

# df$member <- as_factor(df$member)
# df$group <- as_factor(df$group)

saveRDS(df, "d:/R_wd/demo_dashboard/SF1.1/SF1.1.rds")

# read file: 1.1.2 --------------------------------------------------------

# read xlsx
xls <- "d:/R_wd/demo_dashboard/[2020_May_11]SF_1_1_Family_size_and_composition graph update.xlsx"
df <- readxl::read_xlsx(xls, sheet = 2, range = "A4:M50")

# remove empty columns
df <- df[colSums(!is.na(df)) > 0]
# names
names(df) <- c("country",
               "Total Couple households", 
               "Couple households with children", 
               "Couple households without children",
               "Total Single parent households", 
               "Single mother households", 
               "Single father households", 
               "Single person households", 
               "Other household types")
# special case of Israel
df[df$country == "Israel (e,k)", 9] <- df[df$country == "Israel (e,k)", 8]
df[df$country == "Israel (e,k)", 8] <- NA_character_
df <- sapply(df, na_if, "..") %>% 
    as_tibble() 
# as numeric and round
df[ , 2:9] <- sapply(df[ , 2:9], as.numeric) %>% round(2)

dPlot <- df %>% 
    select(1:2, 5, 8, 9)
dPlot <- dPlot %>% 
    .[order(-.[["Total Couple households"]]), ] %>%
    mutate(order = 1:n())

plot_ly(dPlot, 
        x = ~country, 
        y = ~`Total Couple households`, 
        type = "bar", 
        name = "Couple") %>% 
    add_trace(y = ~`Total Single parent households`, 
              name = "Single Parent") %>% 
    add_trace(y = ~`Single person households`, 
              name = "Single Person") %>% 
    add_trace(y = ~`Other household types`, 
              name = "Other") %>% 
    layout(barmode = "stack", 
           xaxis = list(categoryorder = "array",
                        categoryarray = dPlot$`country`))


# SF 2.1 Fertility Rate ---------------------------------------------------

#read and manipulate data
xls <- "c:/Users/user/Downloads/??SF_2_1_Fertility_rates_LUNG.xlsx"
df <- read_xlsx(xls, sheet = 1, 
                range = "L5:Q62", col_names = TRUE)
head(df)
colnames(df) <- c("country", "NA", "replacement rate", "1970", "1995", "2016")
colnames(df)

#remove empty columns
df <- df[rowSums(is.na(df)) != ncol(df), ]

#new variable `member`
df <- df %>% mutate(
    member = case_when(
        country == "Taiwan" ~ "Taiwan", 
        country == "OECD average" ~ "A", 
        row.names(df) %in% 38:56 ~ "non-OECD", 
        TRUE ~ "OECD"
        ), 
    group = case_when(
        member == "Taiwan" ~ 0, 
        member == "A" ~ 0,
        member == "OECD" ~ 0,
        TRUE ~ 1
    )
)

#factorize
df$country <- factor(df$country, levels = df$country)
#df$member <- factor(df$member)
df$group <- factor(df$group, levels = c(0, 1))
#sort df
#df <- df[with(df, order(`group`, `2016`)), ]

#axis color condition
axiscolor <- ifelse(df$country == "Taiwan", "tan4", 
                    ifelse(df$country == "OECD average", "springgreen4", "black"))
#Plot
p <- ggplot() +
    #barplot
    geom_bar(data = df, aes(x = `country`, y = `2016`, fill = `member`), 
             colour = "grey", stat = "identity", width = 0.7, position = position_dodge()) + 
    #points
    geom_point(data = melt(df[ , c("country", "1970", "1995")], id.vars = "country"), 
               aes(x = `country`, y = `value`, colour = `variable`, fill = `variable`, 
                   shape = `variable`), size = 3) + 
    #line(fertility replacement rate)
    geom_line(data = df, aes(x = `country`, y = 2.1, linetype = "solid"), 
              colour = "red", group = 1, size = 1) + 
    #custom fill and color
    scale_fill_manual("", labels = c("Taiwan" = "Taiwan 2016", "A" = "OECD average", 
                                     "OECD" = "OECD", "non-OECD" = "non-OECD"), 
                      
                      #labels = c("f2" = "1995", "f3" = "1997", "f4" = "replacement rate"), 
                      values = c("Taiwan" = "tan1", "A" = "springgreen3", 
                                 "OECD" = "turquoise2", "non-OECD" = "grey" 
                                 , "1970" = "black", "1995" = "snow1"), 
                      breaks = c("Taiwan", "A", "OECD", "non-OECD")
                      ) + 
    scale_colour_manual("child per woman", labels = c("1970" = "1970", "1995" = "1995"),
                        values = c("1970" = "black", "1995" = "black")
                        ) + 
    scale_shape_manual("child per woman", 
                       labels = c("1970" = "1970", "1995" = "1995"),
                       values = c("1970" = 23, "1995" = 23)
                       ) + 
    scale_linetype_manual("", 
                          labels = c("population replacement rate"), 
                          values = c("solid")) +
    guides(fill = guide_legend(override.aes = list(shape = NA), order = 2),
           colour = guide_legend(override.aes = list(fill = c("black", "snow1")), order = 1),
           shape = guide_legend(order = 1)
           ) +
    #labels
    labs(title = "Chart SF2.1.A. Total fertility rate, 1970, 1995 and 2016 or latest available", 
         subtitle = "Average number of children born per woman over a lifetime \ngiven current age-specific fertility rates and \nassuming no female mortality during reproductive years", 
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
          panel.grid.minor = element_line(size = 0.1, colour = alpha("grey28", 0.5)), 
          #panel.grid.major= element_blank(), 
          #panel.grid = element_blank(), 
          panel.background = element_blank(), 
          legend.position = "top",
          legend.background = element_blank()
          ) + 
    #axis y
    scale_y_continuous(expand = c(0, 0), limits = c(0, 6)) ; p
#export the plot
ggsave(filename = "C:/Users/user/Pictures/tst2_plot.png", width = 10, height = 6, dpi = 300)
