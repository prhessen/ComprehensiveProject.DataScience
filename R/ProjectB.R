# Libraries

library(tidyverse)
library(httr)
library(rvest)

# Data Import and Cleaning

vals <- seq(from = 0, to = 230, by = 10)
empty_text <- vector()
title_fxn <- function (x) {
    schol_html <- read_html(paste0("https://scholar.google.com/scholar?start=", x, "&q=%22covid-19%22+source:psychology&hl=en&as_sdt=0,24"))
    Sys.sleep(5)
    title_nodes <- html_nodes(schol_html, css = ".gs_a")
    title_text <- c(empty_text, html_text(title_nodes))
}

other_fxn <- function (x) {
    schol_html <- read_html(paste0("https://scholar.google.com/scholar?start=", x, "&q=%22covid-19%22+source:psychology&hl=en&as_sdt=0,24"))
    Sys.sleep(5)
    other_nodes <- html_nodes(schol_html, css = ".gs_rt")
    other_text <- c(empty_text, html_text(other_nodes))
}

titles <- lapply(vals, title_fxn)
other <- lapply(vals, other_fxn)

