# Functions to create the Bag of Words

library(tidyverse)
library(magrittr)
library(tm)
library(SnowballC)

#' This function create a bag of words with words + nGrams 
#' Mixing nGrams and words has been found to be the most powerful bag of word approach on the current corpus
#' It is based on the tm library
#'
#' @param df a data frame with a cleaned column of strings with one line per document
#' @param colum the column of the data frame containing the text corpus
#' @param min_words minimal frequency of of a word to be included in the vocabulary
#' @param min_nGrams minimal frequency of of a nGram to be included in the vocabulary
#' @param nGrams the number of words per nGram. 2 for bigrams (recommended for English), 3 for trigrams (recommended for French), ...
#' @param weighting A tm weight function like weightTf, weightTfIdf (best for long items), weightBin (best for short or single sentence items)...
#' @param language the 2 letters code of the language, by default "en"
#' 
#' @return a tm DocumentTermMatrix with the terms of the vocabulary created, in data frame format
#' 
#' @examples
#' bag_of_words(corpus,min_words=50,min_nGrams=1000,nGrams=2,weighting=weightTfIdf,language="en")
#' bag_of_words(c("My first sentence", "My second sentence", "My third sentence"),1,1)
#' 
bag_of_words = function(df,column,min_words=50,min_nGrams=1000,nGrams=2,weighting=weightTfIdf,language="en") {
  
  corpus = VCorpus(VectorSource(df[[column]]), readerControl = list(language = language))

  print("VCorpus:")
  print(corpus)
  
  # used for n-grams
  nGramsTokenizer <- function(x) unlist(lapply(ngrams(words(x), nGrams), paste, collapse = "xxx"), use.names = FALSE)
  
  # The control list for the words BoW
  control_list_words = list(
    tokenize = words,
    language=language,
    bounds = list(global = c(min_words, Inf)),
    weighting = weighting,
    tolower = TRUE,
    removePunctuation = TRUE,
    removeNumbers = TRUE,
    stopwords = TRUE,
    stemming = TRUE
  )
  
  # The control list for the nGrams BoW
  control_list_ngrams = list(
    tokenize = nGramsTokenizer,
    language=language,
    bounds = list(global = c(min_nGrams, Inf)),
    weighting = weighting,
    tolower = TRUE,
    removePunctuation = TRUE,
    removeNumbers = TRUE,
    # We don't remove stop-words for nGrams as structure like "are a" or "such a" are meaningful
    stopwords = FALSE,
    stemming = TRUE
  )
  
  
  
  # Create the Document Term Matrix from the corpus: 
  
  # For words
  dtm_words = DocumentTermMatrix(corpus, control=control_list_words)
  m_words = as.matrix(dtm_words)
  print("Dimensions of the words matrice:")
  print(dim(dtm_words))
  
  
  # For nGrams
  dtm_ngrams = DocumentTermMatrix(corpus, control=control_list_ngrams)
  m_ngrams = as.matrix(dtm_ngrams)
  print("Dimensions of the nGrams matrice:")
  print(dim(dtm_ngrams))
  
  
  # Transform into a matrix
  X_m = cbind(m_words,m_ngrams)
  print("Dimensions of the features matrice:")
  print(dim(X_m))
  
  return(as.data.frame(X_m))
  
}

