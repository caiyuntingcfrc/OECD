
# prep and options --------------------------------------------------------

# rm and clear console
rm(list = ls()); cat("\14")
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
# setwd
# setwd("d:/R_wd/")
# option: scipen
options(scipen = 999)
# library
ins.pack("readxl", "tidyverse", "plotly")

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
dPlot <- df %>% 
    select(1:2, 5, 8, 9) %>% 
    mutate(member = case_when(country == "Taiwan" ~ "Taiwan", 
                              country == grep("^OECD", df$country, value = TRUE) ~ "OECD-average", 
                              row.names(df) %in% 41:46 ~ "non-OECD", 
                              TRUE ~ "OECD"), 
           group = case_when(member == "Taiwan" ~ 0, 
                             member == "OECD-average" ~ 0,
                             member == "OECD" ~ 0,
                             TRUE ~ 1
           ))

# desc order: 'Total Couple households'
dPlot <- dPlot %>% 
    .[order(-.[["Total Couple households"]]), ] %>%
    mutate(order = 1:n())

# plotly
plot_ly(dPlot, 
        x = ~country, 
        y = ~`Total Couple households`, 
        type = "bar", 
        name = "Couple") %>% 
    # sinle parent
    add_trace(y = ~`Total Single parent households`, 
              name = "Single Parent") %>% 
    # single person
    add_trace(y = ~`Single person households`, 
              name = "Single Person") %>% 
    # other
    add_trace(y = ~`Other household types`, 
              name = "Other") %>% 
    # layout options
    layout(barmode = "stack", 
           xaxis = list(categoryorder = "array",
                        categoryarray = dPlot$`country`))
