#
# Preprocess and clean-up the data.
#
# Exported functions:
#
# preprocess.all() - preprocess text from all files.
#

library(fastmatch)
library(readr)
library(parallel)
library(stopwords)
library(stringr)
library(tokenizers)

source("include/cache.R")

#
# Remove URLs. The regular expression detects http(s) and ftp(s) protocols.
#
# @param x the text to remove URLs from.
#
# @return the text with removed URLs.
#
preprocess.removeUrl <- function(x) gsub("(ht|f)tp(s?)://\\S+", "", x)

#
# Remove e-mail addresses.
# The regular expression from Stack Overflow:
# https://stackoverflow.com/questions/201323/how-to-validate-an-email-address-using-a-regular-expression
#
# @param x the text to remove e-mail addresses from.
#
# @return the text with removed e-mail addresses.
#
preprocess.removeEmail <- function(x) gsub("(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])", "", x, perl = TRUE)

#
# Remove hash tags (the character # and the following word) and twitter handles
# (the character @ and the following word).
#
# @param x the text to remove hash tags from.
#
# @return the text with removed hash tags.
#
preprocess.removeTagsAndHandles <- function(x) gsub("[@#]\\S+", "", x)

#
# Remove all underscore characters ("_").
# There are different occurence or underscore in the corpora.
# Sometimes several underscores are used to indicate a place to insert
# a word. Sometimes underscores are used as an emphasis. Finally, sometimes
# underscores are used to hide profanity.
# We replace underscores with space characters to keep words separated
# in case when underscores were used as separators.
#
# @param x the text to remove underscores from.
#
# @return the text with removed underscores.
#
preprocess.removeUnderscores <- function(x) gsub("_", " ", x)

#
# Surround punctuation marks which does not appear inside a word with space
# characters. Without this step, fragments with a missing space are
# transformed to a single non-existing word when punctuation is removed.
# Example: corpus contains
# "I had the best day yesterday,it was like two kids in a candy store"
# Without this step, "yesterday,it" is transformed to a non-existing word
# "yesterdayit" when removing punctuation. This step transforms it to
# "yesterday, it"
#
# @param x the text to surround punctuation marks with space characters.
#
# @return the text with punctuation marks surrounded by space characters.
#
preprocess.addMissingSpace <- function(x) gsub("[,.!?()\":;”…]", " ", x)

#
# Split text on words. Keep the words uppercase.
#
# @param text the text to be split on words.
#
# @return the character vector consisting of words.
#
preprocess.tokenize <- function(text) {
    tokenizers::tokenize_words(text,
                               lowercase = FALSE,
                               simplify = TRUE,
                               strip_numeric = TRUE)
}

# Load table mapping short versions such as "I'm" to full versions such as
# "I am".
replacements.tokens <- readr::read_csv("replacements.txt",
                                       col_names = c("token", "replacement"),
                                       col_types = list(col_character(), col_character()))
                                       
# Extract tokens to a separate variable to be able to use fast match.
replacements.tokens.token <- replacements.tokens$token

#
# Replace words in a sentence with replacements available from the table.
# Keep words which are not in the replacement table "as is".
# As a side effect, removes punctuation and transforms to a lower case.
#
# @param tokens the character vector with tokens (words).
#
# @return the character vector with tokens where sort versions are replaced
# by full versions.
#
preprocess.replaceWords <- function(tokens) {
    # Replace character often used in the sample text instead of the
    # upper single quote in abbreviations.
    tokens <- gsub("’", "'", tokens)
    tokens <- gsub("\u0092", "'", tokens)
    
    # Attempt to replace each word.
    tokens <- sapply(tokens, function(x) {
        # Search if a replacement exist.
        replacement.index <- fmatch(x, replacements.tokens.token)
        replacement.index.tolower <- fmatch(tolower(x), replacements.tokens.token)
        
        ifelse(!is.na(replacement.index),
               # Replace the token.
               replacements.tokens$replacement[replacement.index],
               # Can't find a replacement, try lowercase.
               ifelse(!is.na(replacement.index.tolower),
                      # Found a replacement with lowercase. replace the token,
                      # changing the first letter to uppercase.
                      str_to_sentence(replacements.tokens$replacement[replacement.index.tolower]),
                      # Still can't find a replacement, fall back on the token.
                      x
               )
        )
    }, USE.NAMES = FALSE)
    
    tokens <- unlist(preprocess.tokenize(tokens))
}

# Prepare a vector with stop words.
# The vector is cached to be able to use fast-search (fmatch).
stopwords.tokens <- stopwords::stopwords()

#
# Removes stop words, that is less useful words which appears very often.
#
# @param tokens the character vector with tokens (words).
#
# @return the character vector with tokens where stop words have been removed.
#
preprocess.removeStopwords <- function(tokens) {
    # Step 1: look up lower-cased input in stopwords. The output is
    # a vector with the same length as input, with NA where no match found
    # and an index of a stopword where a stopword is found.
    # Step 2: transform to a boolean vector where NA (no match found)
    # are TRUE.
    # Step 3: select only elements with TRUE, that is which are not matched
    # as a stopword.
    words.keep.index <- is.na(fmatch(tolower(tokens), stopwords.tokens))
    tokens[words.keep.index]
}

#
# Remove tokens with characters not from extended Latin alphabet (that is,
# including accented characters).
#
# @param tokens the character vector with tokens (words).
#
# @return the character vector with tokens where tokens with non-latin
#       characters were removed.
#
preprocess.removeNonLatin <- function(tokens) {
    # Valid characters:
    # * A-Z, a-z
    # * Extended Latin (accented): \u00c0 - \u00ff
    # * Upper single quote
    tokens.valid <- grepl("^[A-Za-z'\u00c0-\u00ff]+$", tokens)
    tokens[tokens.valid]
}

#
# Add tokens representing start of a sentence. STOS = Start-of-Sentence.
#
# @param tokens the character vector with tokens (words).
#
# @return the character vector with the Start-of-Sentence token prepended.
#
preprocess.addSentenceTokens <- function(tokens) c("STOS", tokens)

#
# Keep the specified stem, if it is included in the given table with
# top-frequency stems. Otherwise, replaces the word with the special token "UNK"
# (Unknown).
#
# @param 
#
preprocess.top.stem.keep <- function(stem, stems.freq.top) {
    ifelse(is.na(fmatch(stem, stems.freq.top)), "UNK", stem)
}

# Stem the specified token, transform it to the lower case, and
# replace with the UNK token if it is not in the list of top stems.
preprocess.stem.word <- function(x, stems.freq.top) {
    ifelse(x == "STOS", x, 
           preprocess.top.stem.keep(SnowballC::wordStem(tolower(x), language = "en"),
                                    stems.freq.top))
}

#
# Pre-processes the specified text for further processing.
#
# @param x the text to be pre-processed. The text must be 1-element character
#       vector.
# @param removeStopwords TRUE if stop words to be removed. Defaults to FALSE.
#
# @return the pre-processed text as a 1-element character vector.
#
preprocess.text <- function(x, removeStopwords = FALSE) {
    text <- preprocess.removeUrl(x)
    text <- preprocess.removeEmail(text)
    text <- preprocess.removeTagsAndHandles(text)
    text <- preprocess.removeUnderscores(text)
    text <- preprocess.addMissingSpace(text)
    
    tokens <- preprocess.tokenize(text)
    tokens <- preprocess.replaceWords(tokens)
    if (removeStopwords) {
        tokens <- preprocess.removeStopwords(tokens)
    }
    tokens <- preprocess.removeNonLatin(tokens)
    tokens <- preprocess.addSentenceTokens(tokens)
    
    paste(tokens, collapse = " ")
}

#
# Pre-processes the specified text, caching the result into a file. The cache
# directory depends on whether stop words to be removed.
#
# If the cached file already exists, loads pre-processed text from the file.
#
# @param x the character vector with sentences to be pre-processed.
# @param name the base name of the output file.
# @param messageName the human-readable name for progress messages.
# @param removeStopwords TRUE if stop words to be removed. Defaults to FALSE.
#
# @return the character vector with pre-processed text.
#
preprocess.file <- function(x, name, messageName, removeStopwords = FALSE) {
    file.name <- file.path(cache.dir(removeStopwords), name)
    if (!file.exists(file.name)) {
        message("Pre-processing ", messageName, " to ", file.name)
        ts <- system.time(text.preprocessed <- unlist(mclapply(x, preprocess.text, removeStopwords)))
        print(ts)
        message("Removing empty lines from pre-processed ", messageName)            
        text.preprocessed.empty <- grepl("STOS\\s*$", text.preprocessed)
        text.preprocessed <- text.preprocessed[!text.preprocessed.empty]
        message("Saving pre-processed ", messageName)
        write_lines(text.preprocessed, file.name)
        message("Done pre-processing ", messageName)
    } else {
        message("Loading pre-processed ", messageName)
        text.preprocessed <- read_lines(file.name)
        message("Done loading pre-processed ", messageName)
    }
    
    return (text.preprocessed)
}

#
# Preprocess all 3 training corpora (blogs, news and Twitter), caching results
# into files.
#
# @param removeStopwords TRUE if stop words to be removed. Defaults to FALSE.
#
preprocess.all <- function(removeStopwords = FALSE) {
    blogs.text <- text.original.cache("blogs", "training")
    blogs.text <- unlist(tokenizers::tokenize_sentences(blogs.text))
    blogs.text.preprocessed <<- preprocess.file(blogs.text,
                                                "blogs.text.preprocessed.txt",
                                                "blogs", removeStopwords)

    news.text <- text.original.cache("news", "training")
    news.text <- unlist(tokenizers::tokenize_sentences(news.text))
    news.text.preprocessed <<- preprocess.file(news.text,
                                               "news.text.preprocessed.txt",
                                               "news", removeStopwords)

    twitter.text <- text.original.cache("twitter", "training")
    twitter.text <- unlist(tokenizers::tokenize_sentences(twitter.text))
    twitter.text.preprocessed <<- preprocess.file(twitter.text,
                                                  "twitter.text.preprocessed.txt",
                                                  "twitter", removeStopwords)
}
