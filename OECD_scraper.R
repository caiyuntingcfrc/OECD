# prep and options --------------------------------------------------------
# rm
rm(list = ls()); cat("\14")
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
# setwd
setwd("i:/R_wd/")
# option: scipen
options(scipen = 999)
# packages
ins.pack("rvest", "openssl", "progress", 
         "tidyverse", "curl", "httr")

# read html ---------------------------------------------------------------

# # read the html page
# w <- read_html("http://www.oecd.org/els/family/database.htm") %>% 
#         # nodes
#         html_nodes("li li") %>% 
#         html_text() %>% 
#         grep("CO|LMF|PF|SF", ., value = TRUE) %>% 
#         gsub("\n", "", .) %>% 
#         gsub("\\s", " ", .)
#                 
# lapply(w, strsplit, split = " ") %>% 
#         unlist() %>% 
#         grep("^CO|^SF|^LMF|^PF", ., value = TRUE)
        
# make list of pdf and excel files ----------------------------------------

w <- read_html("http://www.oecd.org/els/family/database.htm") %>% 
        # nodes
        html_nodes("li li a")

# list of pdf files
list.pdf <- html_attr(w, "href") %>% 
        grep(".pdf", ., value = T)

# names.pdf <- strsplit(list.pdf, "/") %>% unlist() %>% grep("*.pdf", ., value = T)
path.pdf <- paste("http://www.oecd.org", list.pdf[-38], sep = "") 
path.pdf[71] <- list.pdf[38]

# pdf file names
names.pdf <- strsplit(path.pdf, "/") %>% 
        unlist() %>% 
        grep("*.pdf", ., value = T)
# 32
names.pdf[32] <- "PF1_5_Child_Support.pdf"
# 53
names.pdf[53] <- "CO_1_5_Breastfeeding_Rates.pdf"
# encode URLs
path.pdf <- path.pdf %>% 
        sapply(URLencode, simplify = T, USE.NAMES = F)

# list of excel files
list.xls <- html_attr(w, "href") %>% 
        grep(".xls", ., value = T)

# names.xls <- strsplit(list.xls, "/") %>% unlist() %>% grep("*.xls", ., value = T)
path.xls <- paste("http://www.oecd.org", list.xls[-38], sep = "") 
path.xls[69] <- list.xls[38]

# xls file names
names.xls <- strsplit(path.xls, "/") %>% 
        unlist() %>% 
        grep("*.xls", ., value = T)
# 32
names.xls[32] <- "PF1_5_Child_Support.xls"
# 51
names.xls[51] <- "CO_1_5_Breastfeeding_Rates.xls"

# encode URLs
path.xls <- path.xls %>% 
        sapply(URLencode, simplify = T, USE.NAMES = F)

# add prefix :date
d <- date() %>% 
        strsplit(" ") %>% 
        .[[1]]
d <- paste(d[5], d[2], d[3], sep = "_") %>% 
        paste0("[", ., "]")
names.pdf <- paste(d, names.pdf, sep = " ")
names.xls <- paste(d, names.xls, sep = " ")

# download path
path.download.pdf <- paste("i:/R_wd/OECD/", names.pdf, sep = "")
path.download.xls <- paste("i:/R_wd/OECD/", names.xls, sep = "")
# pdf <- mapply(download.file, path.pdf, path.download)

# for loop with timeer
OECD <- function(path.pdf, path.xls, t = sample(5:30, 1), p){
        p <- length(path.pdf)
        # progressbar
        pb <- progress_bar$new(
                format = "processing [:bar]:percent | :eta | :elapsed",
                total = length(path.pdf), clear = F, width = 80, show_after = 0
                )
        # for loop
        for(i in 1:p){
                a <- try(curl_download(path.pdf[i], path.download.pdf[i], quiet = T))
                if(inherits(a, "try-error") == TRUE) next
                pb$tick()
                if(is.na(path.xls[i])){
                        message("finished")
                        break
                } else {
                        b <- try(curl_download(path.xls[i], path.download.xls[i], quiet = T))
                        if(inherits(b, "try-error") == TRUE) next
                        }
                Sys.sleep(t)
                }
}


# execute the scrapper ----------------------------------------------------

ptm <- proc.time()
OECD(path.pdf, path.xls, p = p)
proc.time() - ptm

# -------------------------------------------------------------------------



# file.rename("i:/R_wd/OECD/2020_May_11 43136964.pdf", "i:/R_wd/OECD/2020_may_11 CO_1_5_Breast_Feeding_Rates.pdf")

# md5, character vector, raw vector or connection objects
# pdf
a <- lapply(path.download.pdf, file) %>% lapply(md5)
b <- lapply(path.pdf, url) %>% lapply(md5)
if(length(intersect(a, b)) == length(a)) message("There is no update of PDF files")

x <- lapply(path.download.xls, file) %>% lapply(md5)
y <- lapply(path.xls, url) %>% lapply(md5)
if(length(intersect(x, y)) == length(x)) message("There is no update of xls files")
#
# a <- md5(file("d:/OECD/SF_1_1_Family_size_and_composition.pdf"))
# b <- md5(url(path.pdf[1]))