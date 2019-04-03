library(rvest)
library(openssl)
library(progress)
library(tidyverse)
library(curl)
library(httr)

rm(list = ls())

w <- read_html("http://www.oecd.org/els/family/database.htm") %>% html_nodes("li li a")

list.pdf <- html_attr(w, "href") %>% grep(".pdf", ., value = T)
# names.pdf <- strsplit(list.pdf, "/") %>% unlist() %>% grep("*.pdf", ., value = T)
path.pdf <- paste("http://www.oecd.org", list.pdf[-38], sep = "") 
path.pdf[71] <- list.pdf[38]
names.pdf <- strsplit(path.pdf, "/") %>% unlist() %>% grep("*.pdf", ., value = T)
path.pdf <- path.pdf %>% sapply(URLencode, simplify = T, USE.NAMES = F)

list.xls <- html_attr(w, "href") %>% grep(".xls", ., value = T)
# names.xls <- strsplit(list.xls, "/") %>% unlist() %>% grep("*.xls", ., value = T)
path.xls <- paste("http://www.oecd.org", list.xls[-38], sep = "") 
path.xls[69] <- list.xls[38]
names.xls <- strsplit(path.xls, "/") %>% unlist() %>% grep("*.xls", ., value = T)
path.xls <- path.xls %>% sapply(URLencode, simplify = T, USE.NAMES = F)
# download path
path.download.pdf <- paste("D:/OECD/", names.pdf, sep = "")
path.download.xls <- paste("D:/OECD/", names.xls, sep = "")
# pdf <- mapply(download.file, path.pdf, path.download)

# for loop with timeer
OECD <- function(path.pdf, path.xls, t, p){
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

ptm <- proc.time()
OECD(path.pdf, path.xls, t = 10, p = p)
proc.time() - ptm

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