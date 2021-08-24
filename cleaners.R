library(tidyverse)
library(textshape)
library(lexicon)
library(textclean)
library(hunspell)
library(qdapRegex)

#' Ensure that a value is either O or 1
#' @param x A number.
#' @return TRUE if \code{x} is either O or 1 and FALSE instead
#' @examples
#' is.bin(1)
#' is.bin(5)
#' 
is.bin = function(x) x %in% c(0,1)



#' Detect and correct misspells in a string
#' @param x string.
#' @return corrected string
#' @examples
#' replace_misspells('I vrite anglish verry weell')
#' replace_misspells('Romeo, Romeo! Wherefore art thou Romeo?')
#' 
replace_misspells = function(x){
  
  sapply(1:length(x),function(y){
    if (is.na(y) || !is.character(y)) return(y)
    bad = hunspell(x[y])[[1]]
    good = unlist(lapply(hunspell_suggest(bad),`[[`,1))
    
    if (length(bad)){
      for (i in 1:length(bad)){
        x[y] <<- gsub(bad[i],good[i],x[y])
      }}})
  return(x)
}

#' Replace digits by characters
#' We use the ¤ character that is rare yet ignored by the rest of the pipeline
#' This function retain the number of digits. 3 is ¤ and 123 is ¤¤¤
#' We cut the result when there is more than 5 digits
replace_numbers_by_token = function(x, replacement="number") {
  #result = str_replace_all(x, "[:digit:]", "¤")
  rm_number(x, replacement = replacement)
}

#' Take a list of strings with text as "x" and return a cleaned version that is more suited for NLP
#' @param x list of strings
#' @return cleaned strings
#' @examples
#' replace_misspells(df$text)
#' replace_misspells(corpus)
#' 
clean_corpus = function(x){
  # Replace redundant white spaces and line jumps such as \n
  x = replace_white(x)
  # Replace or remove non ASCII characters
  x = replace_non_ascii(x)
  # Replace contractions such as "you're" by expanded such as "you are"
  x = replace_contraction(x)
  # Replace elongations. Ex: "heyyyyy" is replaced by "Hey"
  x = replace_word_elongation(x)
  # Replace emoji by plain text
  x = replace_emoji(x)
  # Same for emoticons
  x = replace_emoticon(x)
  # Get ride of HTML remaining in the text if any
  x = replace_html(x)
  # Normalize incomplete sentence replacement
  x = replace_incomplete(x, '.')
  # Replace internet slang by standard words
  x = replace_internet_slang(x)
  # Normalize spaces
  x = replace_kern(x)
  # Replace all amounts of money by a word
  x = replace_money(x, replacement = 'money')
  # Replace all names by a word
  x = replace_names(x, replacement = 'name')
  # Replace dates by a word
  x = replace_date(x, replacement = 'date')
  # Replace all times with a word
  x = replace_time(x, replacement = 'time')
  # Replace ordinals. For example 1st is transformed to first
  x = replace_ordinal(x)
  # Replace ratings such as "five stars" by more common adjectives
  x = replace_rating(x)
  # Replace all numbers by a word
  x = rm_number(x, replacement = "number")
  # Replace symbols used as abbreviations such as @ by at
  x = replace_symbol(x)
  # Strip remaining characters that are not useful
  x = strip(x, char.keep = c("?","!", ".",",",";",":","'"))
  # Replace misspelled words (disabled since the data is too large)
  #x = replace_misspells(x)
  # ok, done...
  return(x)
}

# Extract all the data frame columns that have a specific prefix
prefixed = function(df,prefix) {
  cols = names(df)[startsWith(names(df), prefix)]
  return(df[cols])
}

# Extract one of the data frame column according to its name
getColumn = function(df,col_name) {
  col = df[col_name]
  colnames(col) = col_name
  return(col)
}

# Convert back a column to a data frame
vec2df = function(vec,col_name) {
  df = as.data.frame(vec)
  colnames(df) = col_name
  return(df)
}
