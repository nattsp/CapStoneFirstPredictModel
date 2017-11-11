## Pred 2
library(dplyr)
library(tidytext)
library(tidyr)
library(igraph)
library(ggraph)
library(widyr)
library(stringr)

## Get a feel for filtering
twitter_word_cors %>%
    filter(item1 == "bacon") %>%
    filter(item2 == "beer")

twitter_word_cors %>%
    filter(item1 == "bouquet")

twitter_word_cors %>%
    filter(item1 %in% c("bacon", "beer"))

## Read in text typed
text = "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"

typedText <- as.data.frame(text,
                           stringsAsFactors = FALSE)
typedWords <- unnest_tokens(typedText, word, text, token = "words") %>%
    unique() %>%
    anti_join(stop_words)




system.time(wordsCorr <- twitter_word_cors %>%
    filter(item1 %in% typedWords$word) %>%
    select(-item1) %>%
    group_by(item2) %>%
    summarise(sumCorr = sum(correlation)) %>%
    ungroup() %>%
    arrange(desc(sumCorr)))

#test to see which of the quiz options is available.

filter(wordsCorr, item2 %in% c("cheese","beer", "pretzels", "soda"))

findCorrWords <- function(text, pairsCorr){
    typedText <- as.data.frame(text,
                               stringsAsFactors = FALSE)
    typedWords <- unnest_tokens(typedText, word, text, token = "words") %>%
        unique() %>%
        anti_join(stop_words)
    pairsCorr %>%
        filter(item1 %in% typedWords$word) %>%
        select(-item1) %>%
        group_by(item2) %>%
        summarise(sumCorr = sum(correlation)) %>%
        ungroup() %>%
        arrange(desc(sumCorr))
}

wordsCorr <- findCorrWords(
    "The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
    twitter_word_cors
)


twitter_trigrams <- unnest_tokens(sampleDataWithRowTibble, trigram, text, token = "ngrams", n = 3) %>%
    count(trigram, sort = TRUE) %>%
    separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !word3 %in% stop_words$word)


typedText <- as.data.frame(text,
                           stringsAsFactors = FALSE)
typedWords <- unnest_tokens(typedText, word, text, token = "words") %>%
    tail(n = 2)

possTri <- twitter_trigrams %>%
    filter(word1 == typedWords[1,]
           ,word2 == typedWords[2,]
           )

## Case of beer does appear in the sample of text
sampleDataWithRowTibble %>%
    filter(str_detect(text, 'case of beer'))

## try trigrams leaving in the stop words
twitter_trigrams <- unnest_tokens(sampleDataWithRowTibble, trigram, text, token = "ngrams", n = 3) %>%
    count(trigram, sort = TRUE) %>%
    separate(trigram, c("word1", "word2", "word3"), sep = " ")

twitter_tri_logprob <- twitter_trigrams %>%
    mutate(word1Comb = word1, word2Comb = word2) %>%
    unite(bigrams, word1Comb, word2Comb, sep = " ") %>%
    group_by(bigrams) %>%
    mutate(countBigrams = sum(n)) %>%
    ungroup() %>%
    mutate(Prob3given2 = log(n/countBigrams)) %>%
    group_by(word1) %>%
    mutate(countUnigrams = sum(n)) %>%
    ungroup() %>%
    mutate(Prob2given1 = log(countBigrams/countUnigrams)) %>%
    mutate(ProbUnigram = log(countUnigrams/sum(n))) %>%
    mutate(LogProbTrigram = ProbUnigram + Prob2given1 + Prob3given2) %>%
    mutate(Perplexity = exp(LogProbTrigram)^-3) %>%
    mutate(Perplexity2 = (n/sum(n))^-3)
    

intersect(possTri$word3, wordsCorr$item2)

lastTwo <- function(text){
    typedText <- as.data.frame(text,
                               stringsAsFactors = FALSE)
    lastWords <- unnest_tokens(typedText, word, text, token = "words") %>%
        tail(n = 2)
    lastWords[, 1]
    
}

lastWords <- lastTwo("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")


possTri <- twitter_trigrams %>%
    filter(word1 == lastWords[1]
           ,word2 == lastWords[2]
    )


possTrigram <- function(text, trigrams){
    lastWords <- lastTwo(text)
    trigrams %>%
        filter(word1 == lastWords[1]
               ,word2 == lastWords[2]
        )
}

wordSuggestions <- function(text, pairsCorr, trigrams){
    wordsCorr <- findCorrWords(text, pairsCorr)
    possTri <- possTrigram(text,trigrams)
    intersect(wordsCorr$item2, possTri$word3)
}

text <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
text <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"
text <- "Hey sunshine, can you follow me and make me the"
text <- "Very early observations on the Bills game: Offense still struggling but the"
text <- "Go on a romantic date at the"
text <- "Well I'm pretty sure my granny has some old bagpipes in her garage 
        I'll dust them off and be on my"
text <- "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
text <- "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
text <- "Be grateful for the good times and keep the faith during the"
text <- "If this isn't the cutest thing you've ever seen, then you must be"

wordPredict <- wordSuggestions(text, twitter_word_cors, twitter_trigrams)

wordPredict

posTri <- possTrigram(text,twitter_trigrams)
posTri
wordsCorr <- findCorrWords(text, twitter_word_cors)
wordsCorr
filter(wordsCorr, item2 == "callous")

##Quiz 3
text <- "I'll be there for you, I'd live and I'd"
text <- "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
text <- "I'd give anything to see arctic monkeys this"
text <- "Talking to your mom has the same effect as a hug and helps reduce your"

wordPredict <- wordSuggestions(text, twitter_word_cors, twitter_trigrams)
wordPredict
wordsCorr <- findCorrWords(text, twitter_word_cors)
wordsCorr
posTri <- possTrigram(text,twitter_trigrams)
posTri
