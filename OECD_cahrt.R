
# prep and options --------------------------------------------------------

# rm and clear console
rm(list = ls()); cat("\14")
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
# setwd
setwd("d:/R_wd/demo_dashboard/SF1.1/")
# option: scipen
options(scipen = 999)
# library
ins.pack("readxl", "tidyverse", "plotly")


# rank --------------------------------------------------------------------

x <- c(1, 10, 2, 5, 5, 8, 3)
dense_rank(x)
min_rank(x)


# region ------------------------------------------------------------------

df <- readRDS("SF1.1.B.rds") %>% expss::if_na(0)
region_list <- readxl::read_xlsx("region_rev.xlsx")

# which
w <- which(sapply(region_list$country, grepl, df$country), arr.ind = TRUE)

df$region[w[ , 1]] <- region_list$region[w[ , 2]]

saveRDS(df, "d:/R_wd/demo_dashboard/SF1.1/SF1.1.B.rds")



# read file: 1.1.2 --------------------------------------------------------

# read xlsx
xls <- "H:/20200619/SF1.1.xlsx"
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
# df[df$country == "Israel (e,k)", 9] <- df[df$country == "Israel (e,k)", 8]
# df[df$country == "Israel (e,k)", 8] <- NA_character_
# replace ".." with NA

df <- sapply(df, na_if, "..") %>% as_tibble() 

sapply(df, class)


# as numeric and round
df[ , 2:9] <- sapply(df[ , 2:9], as.numeric) %>% round(2)
# select columns
df <- df %>% 
    # select(1:2, 5, 8, 9) %>% 
    mutate(member = case_when(country == "Taiwan" ~ "Taiwan", 
                              country == grep("^OECD", df$country, value = TRUE) ~ "OECD-average", 
                              row.names(df) %in% 41:46 ~ "non-OECD", 
                              TRUE ~ "OECD")
           # group = case_when(member == "Taiwan" ~ 0, 
                             # member == "OECD-average" ~ 0,
                             # member == "OECD" ~ 0,
                             # TRUE ~ 1
           )

# saveRDS(dPlot, "d:/R_wd/demo_dashboard/SF1.1/SF1.1.B.rds")


# desc order: 'Total Couple households'
dPlot <- dPlot %>% 
    .[order(-.[["Total Single parent households"]]), ] %>%
    mutate(order = 1:n())
    # highlight_key(., ~country)

y <- "y"
switch(y, 
       single = "Total Single parent households")
print(y)

# plotly
plot_ly(dPlot) %>% 
    p1 %>% 
    # # couple
    add_bars(x = ~`country`,
             y = ~`Total Single parent households`,
             opacity = 1,
             marker = list(color = "#166273"),
             name = "Single parent" ) %>%
    # sinle parent
    add_bars(x = ~`country`, 
             y = ~`Total Couple households`, 
             opacity = 1, 
             marker = list(color = "#C1D9D0"), 
             name = "Couple") %>% 
    # single person
    add_bars(x = ~`country`, 
             y = ~`Single person households`, 
             opacity = 1, 
             marker = list(color = "#F23838"), 
             name = "Single Person") %>% 
    # other
    add_bars(x = ~`country`, 
             y = ~`Other household types`, 
             opacity = 1, 
             marker = list(color = "#F2B680"), 
             name = "Other") %>% 
    # layout options
    layout(barmode = "stack",
           xaxis = list(categoryorder = "array",
                        categoryarray = dPlot$`country`))
