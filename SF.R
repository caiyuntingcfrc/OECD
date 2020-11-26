
# prep and options --------------------------------------------------------

# rm and clear console
rm(list = ls()); cat("\14")
source("~/Github_CFRC/misc/func_ins.pack.R")
# devtools::source_url("https://raw.githubus ercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
# setwd
setwd("d:/R_wd/")
# option: scipen
options(scipen = 999)
# load packages
ins.pack("tidyverse", 
         "feather", 
         "pastecs", 
         "expss", 
         "data.table")

# read data file ----------------------------------------------------------

df <- readRDS("tw_inc/R data files/df_inc108.rds")

# calc: number of people, children and elderly ----------------------------

# data.table approach
setDT(df)

# grep b1_: id
lb1 <- grep("^b1_", names(df), value = TRUE)

# number of people in the household
df[ , n.all := rowSums(!is.na(.SD)), .SDcols = lb1]

# grep b4_: age
lb4 <- grep("^b4_", names(df), value = TRUE)

# number of children (< 18)
df[ , n.children := rowSums(.SD < 18, na.rm = TRUE), .SDcols = lb4]

# number of the elderly (>= 65)
df[ , n.elderly := rowSums(.SD >= 65, na.rm = TRUE), .SDcols = lb4]

# # tidyverse approach
# # number of people in the household (tidyverse approach)
# df <- df %>% 
#         mutate(n.all = rowSums(!is.na(select(., matches("^b1_"))), na.rm = TRUE))
# 
# # number of children in the household (tidyverse approach)
# df <- df %>% 
#         mutate(n.children = rowSums((select(., matches("^b4_")) < 18), na.rm = TRUE))
# 
# # number of the elderly in the household (tidyverse approach)
# df <- df %>% 
#         mutate(n.elderly = rowSums((select(., matches("^b4_")) >= 65), na.rm = TRUE))


# check if weight is numeric ----------------------------------------------

# wight a20
weight <- "a20"
if(!is.numeric(df[[weight]])) {
        df[[weight]] <- as.numeric(df[[weight]])
        }

# All households ----------------------------------------------------------

# n.all and weight
all <- df$n.all
w <- df[[weight]]
# xtab
x <- round(xtabs(w ~ all), digits = 0)
n <- names(x)
# weigh
weighed <- mapply(rep, n, times = x)
# unlist and transform to numeric
l <- unlist(weighed, use.names = FALSE) %>% 
        as.numeric
# summary
stat.desc(l) %>% 
        round(2) %>% 
        knitr::kable(.)
# freq
epiDisplay::tab1(l, decimal = 2, graph = TRUE)

# couple households with children -----------------------------------------

# data.table approach
# d <- setDT(df)
# filter couple households with at least one dependent children
d <- df[a18 %in% c(421, 422, 431, 432) & n.children >= 1, ]
d <- df[a18 %in% c(421, 422, 431, 432), coupleWithChildren := ifelse(n.children >= 1, 1, 0)]

# tidyverse approach
# filter couple households with at least one dependent children
# d <- df %>% 
#         filter(a18 %in% c(421, 422, 431, 432)) %>% 
#         filter(n.children >= 1)

# n.all and weight
all <- d$n.all
w <- d[[weight]]
# xtab
x <- round(xtabs(w ~ all), digits = 0); x
n <- names(x); n
# weigh
weighed <- mapply(rep, n, times = x)
# unlist and transform to numeric
l <- unlist(weighed, use.names = FALSE) %>% 
        as.numeric()
# summary
stat.desc(l) %>% 
        round(2) %>% 
        knitr::kable(.)
# coupleWithChildren and weight
all <- d$coupleWithChildren
w <- d[[weight]]
# xtab
x <- round(xtabs(w ~ all), digits = 0); x
n <- names(x); n
# weigh
weighed <- mapply(rep, n, times = x)
# unlist and transform to numeric
l <- unlist(weighed, use.names = FALSE) %>% 
        as.numeric()
# summary
stat.desc(l) %>% 
        round(2) %>% 
        knitr::kable(.)
# freq
epiDisplay::tab1(l, decimal = 2, graph = TRUE)

# single parent households with children ----------------------------------

# data.table approach
d <- df %>% setDT()
# filter couple households with at least one dependent children
d <- d[a18 %in% c(321, 322, 331, 332) & n.children >= 1, ]

# tidyverse approach
# filter couple households with at least one dependent children
# d <- df %>% 
#         filter(a18 %in% c(321, 322, 331, 332)) %>% 
#         filter(n.children >= 1)

# n.all and weight
all <- d$n.all
w <- d[[weight]]
# xtab
x <- round(xtabs(w ~ all), digits = 0); x
n <- names(x); n
# weigh
weighed <- mapply(rep, n, times = x)
# unlist and transform to numeric
l <- unlist(weighed, use.names = FALSE) %>% 
        as.numeric()
# summary
stat.desc(l) %>% 
        round(2) %>% 
        knitr::kable(.)
# freq
epiDisplay::tab1(l, decimal = 2, graph = TRUE)

# types of household ------------------------------------------------------

# data.table approach
d <- df %>% setDT()
# one person
d[a18 %in% c(101, 102), sf := 1]
# married couple
# d[a18 %in% c(201, 202), sf := 2]
# single mother households
d[a18 %in% c(322, 332) & n.children >= 1, sf := 3.2]
# single father households
d[a18 %in% c(321, 331) & n.children >= 1, sf := 3.1]
# nuclear families (couple households with children)
d[a18 %in% c(421, 422, 431, 432) & n.children >= 1, sf := 4.1]
# nuclear families (couple households without children)
d[(a18 %in% c(201, 202)) | (a18 %in% c(421, 422, 431, 432) & n.children < 1), sf := 4.0]
# other types of households
d[a18 %in% c(511, 512, 531, 532, 
             611, 612, 621, 622, 631, 632, 
             701, 702), sf := 7]
d[, sf := ifelse(is.na(sf), 7, sf)]

# # tidyverse approach
# d <- df %>% 
#         mutate(sf = case_when(a18 %in% c(101, 102) ~ 1, 
#                               a18 %in% c(322, 332) & n.children >= 1 ~ 3.2, 
#                               a18 %in% c(321, 331) & n.children >= 1 ~ 3.1, 
#                               a18 %in% c(421, 422, 431, 432) & n.children >= 1 ~ 4.1, 
#                               (a18 %in% c(201, 202)) | (a18 %in% c(421, 422, 431, 432) & n.children < 1) ~ 4.0, 
#                               a18 %in% c(511, 512, 531, 532, 
#                                          611, 612, 621, 622, 631, 632, 
#                                          701, 702) ~ 7, 
#                               TRUE ~ 7
#                               )
#                )

# label
val_lab(d$sf) <- num_lab("
                         1 single person
                         3.2 single mother
                         3.1 single father
                         4.1 couple with children
                         4.0 couple without children
                         7 others
                         ")
# sf and weight
weight <- "a20"
sf <- d$sf
w <- d[[weight]]
# xtab
x <- round(xtabs(w ~ sf), digits = 0)
n <- names(x)
# weigh
weighed <- mapply(rep, n, times = x)
# unlist and transform to numeric
l <- unlist(weighed, use.names = FALSE)
# freq table
epiDisplay::tab1(l, decimal = 2, graph = TRUE)

# household with children -------------------------------------------------

# data.table approach
d <- df
d[n.children == 0 , with_children := 0]
d[n.children == 1 , with_children := 1]
d[n.children == 2 , with_children := 2]
d[n.children >= 3 , with_children := 3]

# recode with tidyverse approach
# d <- df %>% 
#         mutate(with_children = case_when(n.children == 0 ~ 0,
#                                          n.children == 1 ~ 1, 
#                                          n.children == 2 ~ 2, 
#                                          n.children >= 3 ~ 3))

# label
val_lab(d$with_children) <- num_lab("
                         0 0 children
                         1 1 child
                         2 2 children
                         3 3 or more children
                         ")

# with_children and weight
weight <- "a20"
with_children <- d$with_children
w <- d[[weight]]
# xtab
x <- round(xtabs(w ~ with_children), digits = 0)
n <- names(x)
# weigh
weighed <- mapply(rep, n, times = x)
# unlist and transform to numeric
l <- unlist(weighed, use.names = FALSE)
# freq table
epiDisplay::tab1(l, decimal = 2, graph = TRUE)

# household with childre under 6 ------------------------------------------

# data.table approach
d <- df
# grep
lb4 <- grep("^b4_", names(d), value = TRUE)
# number of children (< 6)
d[ , n.6 := rowSums(.SD < 6, na.rm = TRUE), .SDcols = lb4]
d[ , n.undersix := ifelse(n.6 >= 1, 1, 0)]

# # tidyverse approach
# d <- df %>% 
#         mutate(n.6 = rowSums((select(., matches("^b4_")) < 6), na.rm = TRUE)) %>% 
#         mutate(n.undersix = case_when(n.6 >= 1 ~ 1, 
#                                        TRUE ~ 0))
# n.undersix and weight
weight <- "a20"
n.undersix <- d$n.undersix
w <- d[[weight]]
# xtab
x <- round(xtabs(w ~ n.undersix), digits = 0)
n <- names(x)
# weigh
weighed <- mapply(rep, n, times = x)
# unlist and transform to numeric
l <- unlist(weighed, use.names = FALSE)
# freq table
epiDisplay::tab1(l, decimal = 2, graph = TRUE)

# living arrangements of children -----------------------------------------
# children > 1
d <- df[n.children > 1, ]
# living with one parent
d[a18 %in% c(321, 331, 322, 332), sf := 1]
# living with two parent unspecified
d[is.na(sf) & !(a18 %in% c(511, 512, 531, 532, 
                         101, 102, 
                         201, 202, 
                         701, 702)), sf := 2]
# others
d[is.na(sf), sf := 3]

# label
val_lab(d$sf) <- num_lab("
                         1 with one parent
                         2 with tow parent(unspecified)
                         3 others
                         ")

# weigh
weight <- "a20"
sf <- d$sf
w <- d[[weight]]
# xtab
x <- round(xtabs(w ~ sf), digits = 0)
n <- names(x)
# weigh
weighed <- mapply(rep, n, times = x)
# unlist and transform to numeric
l <- unlist(weighed, use.names = FALSE)
# freq table
epiDisplay::tab1(l, decimal = 2, graph = TRUE)

# living arrangements of children (ALL) -----------------------------------

# children > 1
d <- df[n.children > 1, ]
# single parent
d[a18 %in% c(321, 331, 322, 332), sf := 3]
# nuclear family
d[a18 %in% c(421, 422, 431, 432), sf := 4]
# grandparent
d[a18 %in% c(511, 512, 531, 532), sf := 5]
# stem
d[a18 %in% c(611, 612, 
             621, 622, 
             631, 632), sf := 6]
# others
d[is.na(sf), sf := 7]
# label
val_lab(d$sf) <- num_lab("
                         3 single parent
                         4 nuclear
                         5 grandparent
                         6 stem
                         7 others
                         ")
# weigh
weight <- "a20"
sf <- d$sf
w <- d[[weight]]
# xtab
x <- round(xtabs(w ~ sf), digits = 0)
n <- names(x)
# weigh
weighed <- mapply(rep, n, times = x)
# unlist and transform to numeric
l <- unlist(weighed, use.names = FALSE)
# freq table
epiDisplay::tab1(l, decimal = 2, graph = TRUE)
# one person
d[a18 %in% c(101, 102), sf := 1]
# living with one parent
d[a18 %in% c(321, 331, 322, 332), sf := 1]