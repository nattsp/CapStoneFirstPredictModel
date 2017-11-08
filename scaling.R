hist(log(twitter_trigrams$n+1))

mostFreq <- twitter_trigrams %>%
    mutate(logn=log(n+1)) %>%
    filter(logn > 1) %>%
    select(n) %>%
    count()

mostFreq/count(twitter_trigrams)

hist(10* log(twitter_word_cors$correlation+1))     
