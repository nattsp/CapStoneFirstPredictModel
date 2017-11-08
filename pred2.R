## Pred 2
library(dplyr)
library(tidytext)
library(tidyr)
library(igraph)
library(ggraph)
library(widyr)
twitter_word_cors %>%
    filter(item1 == "bacon") %>%
    filter(item2 == "beer")

twitter_word_cors %>%
    filter(item1 == "bouquet")

## Read in text typed
text = "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"

typedText <- as.data.frame(text,
                           stringsAsFactors = FALSE)
typedWords <- unnest_tokens(typedText, word, text, token = "words") %>%
    unique() %>%
    anti_join(stop_words)


twitter_word_cors %>%
    filter(item1 %in% c("bacon", "beer"))

wordsCorr <- twitter_word_cors %>%
    filter(item1 %in% typedWords$word) %>%
    select(-item1) %>%
    group_by(item2) %>%
    summarise(sumCorr = sum(correlation)) %>%
    ungroup() %>%
    arrange(desc(sumCorr))

wordsCorr


