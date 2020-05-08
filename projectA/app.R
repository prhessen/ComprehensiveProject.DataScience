#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Libraries

library(shiny)
library(rtweet)
library(tidyverse)
library(tm)
library(qdap)
library(textstem)
library(RWeka)
library(wordcloud)
library(shiny)
library(rsconnect)

# Data Import and Cleaning

## API Access Info

api <- '7DPTSeLp9wqmBaoayNYpPJvLp'
apiSecret <- 'ftzB0U2ulWaYs32tNHn9lERkhbVUtbuAiP6o8Lec5XV7lNU7GM'
access <- '1012248144-7qLC5cfX6BVKS1a4lpnIHRndLSPOxiUA62RnEOw'
accessSecret <- 'Qbh5TneWdrQhEkmlDvzOGFKmHzkZVm1KcDzb0kOK13eBP'
app <- "Phoebe Hessen"
twitter_token <- create_token(
    app = app,
    consumer_key = api,
    consumer_secret = apiSecret,
    access_token = access,
    access_secret = accessSecret)

## Twitter API Calls
COVID <- search_tweets(q = "#COVID",
                       n = 500,
                       include_rts = F)

COVID19 <- search_tweets(q = "#COVID19",
                         n = 500,
                         include_rts = F)

COVID.19 <- search_tweets(q = "#COVID-19",
                          n = 500,
                          include_rts = F)

COVID_19 <- search_tweets(q = "#COVID_19",
                          n = 500,
                          include_rts = F)

## Tokenizer function
tokenizer <- function(x)
{NGramTokenizer(x, Weka_control(min = 1, max = 2))}

## Function for removing regex patterns from tweets
f <- content_transformer(function(x, pattern) gsub(pattern, "", x))

## For this function, input is a dataframe of tweets
## Output is a clean DTM (steps described in more detail below)
## Gets called in tibble_fxn
clean_fxn <- function(x) {
    full_tweets <- x$text %>% iconv("UTF-8", "ASCII", sub="")
    twitter_cp <- VCorpus(VectorSource(full_tweets))
    # Remove links
    twitter_cp <- tm_map(twitter_cp, f,"https?://t.co/[a-z,A-Z,0-9]*")
    # Remove hastags
    twitter_cp <- tm_map(twitter_cp, f,"#[a-z,A-Z,0-9]*")
    # Remove RT headers
    twitter_cp <- tm_map(twitter_cp, f,"RT @[a-z,A-Z,0-9]{8}")
    # Remove @ tags
    twitter_cp <- tm_map(twitter_cp, f,"@[a-z,A-Z,0-9]*")
    # Replace abbreviations, contractions, put in lowercase, remove numbers and
    # punctuation, remove stop words (english and spanish), and strip white space
    twitter_cp <- tm_map(twitter_cp, content_transformer(replace_abbreviation))
    twitter_cp <- tm_map(twitter_cp, content_transformer(replace_contraction))
    twitter_cp <- tm_map(twitter_cp, content_transformer(str_to_lower))
    twitter_cp <- tm_map(twitter_cp, removeNumbers)
    twitter_cp <- tm_map(twitter_cp, removePunctuation)
    twitter_cp <- tm_map(twitter_cp, removeWords, stopwords("en"))
    # I noticed some Spanish tweets so I'm removing the Spanish stopwords too
    twitter_cp <- tm_map(twitter_cp, removeWords, stopwords("es"))
    twitter_cp <- tm_map(twitter_cp, stripWhitespace)
    # Lemmatize words
    twitter_cp <- lemmatize_words(twitter_cp)
    twitter_dtm <- DocumentTermMatrix(
        twitter_cp, 
        control = list(tokenize = tokenizer)
    )
    # Remove sparse terms
    # The different hastags result in very different numbers of tweets across
    # different API calls, but this index almost always results in between
    # 50 and 100 terms per hastag
    twitter_dtm <- removeSparseTerms(twitter_dtm, .987)
    # Create token counts vector
    tokenCounts <- apply(twitter_dtm, 1, sum)
    # Remove cases with no tokens
    twitter_dtm <- twitter_dtm[tokenCounts > 0,]
}

## Input to this function is a dataset from a twitter API call
## Output is a tibble of lemmas
tibble_fxn <- function(x) {
    clean_corp <- clean_fxn(x)
    matrix <- as.matrix(clean_corp)
    tibble <- as_tibble(matrix)
}

## Creating tweet tibbles using tibble_fxn
COVID_tbl <- tibble_fxn(COVID)
COVID19_tbl <- tibble_fxn(COVID19)
COVID.19_tbl <- tibble_fxn(COVID.19)
COVID_19_tbl <- tibble_fxn(COVID_19)

# Visualization

## Input to the function is clean token tibble
## Output is 50 most common ngram wordcloud
cloud_fxn <- function(x) {
    wordCounts <- colSums(x)
    wordNames <- names(x)
    wordcloud(wordNames, wordCounts, 
              max.words=50, scale=c(2,1), 
              colors = brewer.pal(8, "Dark2"))
}

## The scale I used makes the wordclouds a little small, but this was the 
## largest I could get it to be while ensuring that all 50 words were plotted


## Input is a tibble of lemmas
## Output a count of different lemmas that appear >5 times
counts_fxn <- function(x) {
    wordCounts <- colSums(x)
    summary_tbl <- enframe(wordCounts) %>%
        filter(value > 5)
}

## Get the lemma counts for different hastags
COVID_counts <- counts_fxn(COVID_tbl)
COVID19_counts <- counts_fxn(COVID19_tbl)
COVID.19_counts <- counts_fxn(COVID.19_tbl)
COVID_19_counts <- counts_fxn(COVID_19_tbl)

## Join the lemma counts to get overall counts across all hastags in a tibble
all_four <- COVID_counts %>%
    full_join(COVID19_counts, by = "name") %>%
    full_join(COVID.19_counts, by = "name") %>%
    full_join(COVID_19_counts, by = "name") %>%
    filter(complete.cases(.)) %>%
    mutate(total = rowSums(.[,2:5]))

## Make a table with the counts of overlapping lemmas between hashtag pairs
table_vals <- c(length(intersect(COVID_counts$name, COVID19_counts$name)),
                length(intersect(COVID_counts$name, COVID.19_counts$name)),
                length(intersect(COVID_counts$name, COVID_19_counts$name)),
                length(intersect(COVID19_counts$name, COVID.19_counts$name)),
                length(intersect(COVID19_counts$name, COVID_19_counts$name)),
                length(intersect(COVID.19_counts$name, COVID_19_counts$name)))

## Add names to table
table_names <- c("COVID:COVID19","COVID:COVID.19","COVID:COVID_19",
                 "COVID19:COVID.19","COVID19:COVID_19","COVID.19:COVID_19")
disp_table <- tibble("Hashtag Comparisons" = table_names, 
                     "Number of Shared Tokens" = table_vals)

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("NLP Analysis of Tweets From Various COVID-19 Hashtags"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("hashtag", "Hashtag Selection: ",
                        choices = c("#COVID","#COVID19","#COVID-19","#COVID_19"), 
                        selected = "#COVID")
        ),
        mainPanel(
           plotOutput("cloud"),
           plotOutput("chart"),
           tableOutput("table")
        )
    )
)

# Define server logic
server <- function(input, output) {
    output$chart <- renderPlot({
        all_four %>% 
            arrange(desc(total)) %>%
            top_n(20) %>%
            ggplot(aes(x= reorder(name, total),y = total)) + 
            geom_col(fill = "darkorchid") + 
            coord_flip() +
            ylab("Total Number of Appearances") +
            xlab("Common Words")
    })
    output$cloud <- renderPlot({
        data <- switch(input$hashtag,
                       "#COVID" = COVID_tbl,
                       "#COVID19" = COVID19_tbl, 
                       "#COVID-19" = COVID.19_tbl,
                       "#COVID_19" = COVID_19_tbl)
        cloud_fxn(data)
    })
    output$table <- renderTable({
        disp_table
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



# Link to live app: https://phoebe-apps.shinyapps.io/projecta/