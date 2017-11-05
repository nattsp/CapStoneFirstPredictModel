## Text prediction

library(tm)
library(quanteda)
library(data.table)
library(tidytext)
library(stringr)


## Read in data
load("..\\..\\Data\\sampleNewsT.RData")
sampleNewsDT <- as.data.table(sampleNewsT)
sampleNewsDF <- as.data.frame(sampleNewsT)
sampleNewsDF$textNumber <- as.character(sampleNewsDF$textNumber)
cast_dfm(sampleNewsT, text, textNumber)

rm(sampleNewsT)

## Tokenise the text
test <- tokens(sampleNewsDT)
test2 <- tokens(sampleNewsT)
class(sampleNewsT)
sampleNewsT

sampleNewsCorpus <- corpus(sampleNewsDF, docid_field = "textNumber", text_field = "text")
summary(sampleNewsCorpus)
class(sampleNewsT$text)

profanityEng <- readLines("..\\..\\Data\\obsceneEnglish.txt")
profanityEng

# Showing a hit and the words around it
options(width = 200)
kwic(sampleNewsCorpus, "terror")
kwic(sampleNewsCorpus, "acknowledges")
kwic(sampleNewsCorpus, "shit")

sampleNewsToken <- tokens(sampleNewsCorpus)
newsTrigrams <- tokens_ngrams(sampleNewsToken, n=3)
