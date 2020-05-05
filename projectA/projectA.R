# Libraries
library(rtweet)
library(tidyverse)
library(tm)
library(qdap)
library(textstem)
library(RWeka)
library(wordcloud)
library(shiny)

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


# Tokenizer function
tokenizer <- function(x)
{NGramTokenizer(x, Weka_control(min = 1, max = 2))}

# Function for removing regex patterns from tweets
f <- content_transformer(function(x, pattern) gsub(pattern, "", x))


# For this function, x needs to be a dataframe of tweets
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
    twitter_dtm <- removeSparseTerms(twitter_dtm, .987)
    print(twitter_dtm)
    # Create token counts vector
    tokenCounts <- apply(twitter_dtm, 1, sum)
   
    # Remove cases with no tokens
    twitter_dtm <- twitter_dtm[tokenCounts > 0,]
}

tibble_fxn <- function(x) {
    clean_corp <- clean_fxn(x)
    matrix <- as.matrix(clean_corp)
    tibble <- as_tibble(matrix)
}


COVID_tbl <- tibble_fxn(COVID)
COVID19_tbl <- tibble_fxn(COVID19)
COVID.19_tbl <- tibble_fxn(COVID.19)
COVID_19_tbl <- tibble_fxn(COVID_19)



#### ABOVE HERE ALL IN APP ####



# Input to the function is clean token tibble
# Output is 50 most common ngram wordcloud
cloud_fxn <- function(x) {
    wordCounts <- colSums(x)
    wordNames <- names(x)
    wordcloud(wordNames, wordCounts, max.words=50, color = "springgreen4")
}

COVID_cloud <- cloud_fxn(COVID_tbl)
COVID19_cloud <- cloud_fxn(COVID19_tbl)
COVID.19_cloud <- cloud_fxn(COVID.19_tbl)
COVID_19_cloud <- cloud_fxn(COVID_19_tbl)



# identify tokens that appear more than 5 times
# count number of overlaps
# put in table

counts_fxn <- function(x) {
    wordCounts <- colSums(x)
    summary_tbl <- enframe(wordCounts) %>%
        filter(value > 5)
}

COVID_counts <- counts_fxn(COVID_tbl)
COVID19_counts <- counts_fxn(COVID19_tbl)
COVID.19_counts <- counts_fxn(COVID.19_tbl)
COVID_19_counts <- counts_fxn(COVID_19_tbl)

table_vals <- c(length(intersect(COVID_counts$name, COVID19_counts$name)),
                length(intersect(COVID_counts$name, COVID.19_counts$name)),
                length(intersect(COVID_counts$name, COVID_19_counts$name)),
                length(intersect(COVID19_counts$name, COVID.19_counts$name)),
                length(intersect(COVID19_counts$name, COVID_19_counts$name)),
                length(intersect(COVID.19_counts$name, COVID_19_counts$name)))

table_names <- c("COVID:COVID19","COVID:COVID.19","COVID:COVID_19","COVID19:COVID.19","COVID19:COVID_19","COVID.19:COVID_19")
disp_table <- tibble("Hastag Comparisons" = table_names, "Number of Shared Tokens" = table_vals)
disp_table

all_four <- COVID_counts %>%
    full_join(COVID19_counts, by = "name") %>%
    full_join(COVID.19_counts, by = "name") %>%
    full_join(COVID_19_counts, by = "name") %>%
    filter(complete.cases(.)) %>%
    mutate(total = rowSums(.[,2:5]))

chart <- all_four %>% 
    arrange(desc(total)) %>%
    top_n(20) %>%
    ggplot(aes(x= reorder(name, total),y = total)) + 
    geom_col(fill = "darkorchid") + 
    coord_flip()
chart









