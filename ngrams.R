library(dplyr)
library(tidytext)
library(tidyr)
library(igraph)
library(ggraph)
library(widyr)

load("..\\..\\Data\\en_US.twitterSample3.RData")
load("..\\..\\Data\\sampleDataWithRowTibble.RData")

## sampleDataWithRowTibble contains the row number

## bigrams

twitter_bigrams <- unnest_tokens(sampleData3, bigram, text, token = "ngrams", n = 2)
twitter_bigrams <- unnest_tokens(sampleDataWithRowTibble, bigram, text, token = "ngrams", n = 2)

twitter_bigrams

twitter_bigrams_filtered <- twitter_bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    unite(bigram, word1, word2, sep = " ") %>%
    count(textNumber, bigram, sort = TRUE)

twitter_bigrams_filtered

# tf_idf only makes sence when you have different documents
twitter_bigrams_tf_idf <- twitter_bigrams_filtered %>%
    #count(bigram) %>%
    bind_tf_idf(bigram, textNumber, n) %>%
    arrange(desc(bigram))

bigram_tf_idf <- bigrams_united %>%
    count(book, bigram) %>%
    bind_tf_idf(bigram, book, n) %>%
    arrange(desc(tf_idf))

bigram_tf_idf

## trigrams


twitter_trigrams <- unnest_tokens(sampleDataWithRowTibble, trigram, text, token = "ngrams", n = 3) %>%
    separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !word3 %in% stop_words$word) %>%
    unite(trigram, word1, word2, word3, sep = " ") %>%
    count(trigram, sort = TRUE)

twitter_trigrams

## igraph

twitter_bigrams_sep <- twitter_bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
twitter_bigrams_sep

twitter_bigrams_graph <- twitter_bigrams_sep %>%
    filter(n > 30) %>%
    graph_from_data_frame()

twitter_bigrams_graph

set.seed(2017)

ggraph(twitter_bigrams_graph, layout = "fr") +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1)


set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(twitter_bigrams_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()



## count and graph functions

count_bigrams <- function(dataset) {
    dataset %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        filter(!word1 %in% stop_words$word,
               !word2 %in% stop_words$word) %>%
        count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
    set.seed(2016)
    a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
    
    bigrams %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
        geom_node_point(color = "lightblue", size = 5) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
        theme_void()
}


## Counting and correlating

twitterWordsTweet <- sampleDataWithRowTibble %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word)

twitterWordsTweet

twitter_word_pairs <- twitterWordsTweet %>%
    pairwise_count(word, textNumber, sort = TRUE)

twitter_word_pairs

# correlation

twitter_word_cors <- twitterWordsTweet %>%
    group_by(word) %>%
    filter(n() >= 20) %>%
    pairwise_cor(word, textNumber, sort = TRUE)

twitter_word_cors


set.seed(2016)

twitter_word_cors %>%
    filter(correlation > .2) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()
