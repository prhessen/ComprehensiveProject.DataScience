# Libraries

library(tidyverse)
library(httr)
library(rvest)

# Data Import and Cleaning

## Function to pull necessary information from Google Scholar and convert into
## text
## Input is a numeric value (page number)
## Output is a list of lists containing th requested information
vals <- seq(from = 0, to = 270, by = 10)
empty_text <- vector()
schol_fxn <- function (x) {
    schol_html <- read_html(paste0("https://scholar.google.com/scholar?start=", x, "&q=%22covid-19%22+source:psychology&hl=en&as_sdt=0,24"))
    Sys.sleep(5)
    title_nodes <- html_nodes(schol_html, css = ".gs_a")
    title_text <- c(empty_text, html_text(title_nodes))
    other_nodes <- html_nodes(schol_html, css = ".gs_rt")
    other_text <- c(empty_text, html_text(other_nodes))
    mylist <- list(title_text, other_text)
    return(mylist)
}

## Applying the function to the range of values for page numbers
text <- lapply(vals, schol_fxn)

## Function to grab the title values from the list populated above
## Input is a numerical value indicating a list element
## Output is a list of lists of titles
len <- length(text)
new_vals <- seq(from = 1, to = len, by = 1)

empty_titles <- vector()
title_fxn <- function (x) {
    title <- text[[x]][[2]]
    titles_list <- c(empty_titles, title)
}
## Applying the function to the full list
all_titles <- lapply(new_vals, title_fxn)

## Turning the output of the function into a vector
titles <- unlist(all_titles)


## Function to grab the other information (author, year, journal) from the list 
## populated above
## Input is a numerical value indicating a list element
## Output is a list of lists of titles
empty_other <- vector()
other_fxn <- function (x) {
    other <- text[[x]][[1]]
    other_list <- c(empty_other, other)
}

## Applying the function to the full list
all_other <- lapply(new_vals, other_fxn)

## Turning the output of the function into a vector
other <- unlist(all_other)

## Create a tibble with the desired information
schol_tibble <- tibble(title = titles, other = other) %>%
    mutate(year = as.numeric(str_extract(other, pattern = "[0-9]{4}"))) %>%
    mutate(title = str_match(titles, pattern = "((\\[[A-Z]+\\])?(\\[[A-Z]+\\])?)\\s?(.+)")[,5]) %>%
    mutate(author = str_match(other, pattern = "(.*?\\s)-(\\s.*)")[,2]) %>%
    mutate(journal = str_match(other, pattern = "[\\s][-][\\s]…?\\:?([A-Za-z\\s\\.\\:]*)")[,2]) %>%
    mutate(link = str_match(other, pattern = "[\\s][-][\\s]([A-Za-z?\\.?\\s]*$)")[,2]) %>%
    select(-other)
    
# Visualization

chart_tbl <- schol_tibble %>%
    mutate_at("journal", str_trim) %>%
    mutate_at("journal", str_to_upper) %>%
    mutate_at(.vars = c("year","journal"), as.factor)

chart_tbl$journal <- fct_collapse(chart_tbl$journal, "PSYCHOLOGY OF SPORT AND EXERCISE" = c("PSYCHOLOGY OF SPORT AND EXERCISE","PSYCHOLOGY OF SPORT AND"))

journal_tab <- sort(table(chart_tbl$journal), decreasing = T)[2:11]
journals <- as.data.frame(journal_tab)[,1]


plot <- chart_tbl %>%
    filter(journal %in% journals) %>%
    filter(!is.na(year)) %>%
    ggplot(aes(x = year)) + 
    geom_bar(aes(fill = journal))

plot

