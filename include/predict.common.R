#
# Common function for predicting text based on the model.
#
# if (!exists('predict.common')) {
    predict.common <- TRUE
    
    source("include/preprocess.R")

    # Loads frequency table for stems.
    stems.freq.load <- function() {
        stems.freq.file <- "cache/stems.freq.RDS"
        readRDS(stems.freq.file)
    }
    
    # Caches frequency table for stems.
    stems.freq <- stems.freq.load()
    
    # We will use 2 bytes to encode each stem, but we require 2 additional
    # special tokens: STOS for Start-Of-Sentence, and UNK for a Unknown
    # Word, that is a word which is not included in our encoding table.
    # The token STOS is already included in stems, so we must make place
    # only for the token UNK, that is we may encode (256 * 256 - 2) top
    # stems.
    stems.freq.top <- unlist(stems.freq$Terms[1:(256 * 256 - 1)])
    
    # Keep the word, if it is included in the table with top stems.
    # Otherwise, replaces the word with the specified token.
    preprocess.top.stem.keep <- function(stem) {
        ifelse(is.na(fmatch(stem, stems.freq.top)), "UNK", stem)
    }
    
    # Stem the specified token, transform it to the lower case, and
    # replace with the UNK token if it is not in the list of top stems.
    preprocess.stem.word <- function(x) {
        ifelse(x == "STOS",
               x,
               preprocess.top.stem.keep(SnowballC::wordStem(tolower(x), language = "en")))
    }
    
    # Pre-process text before predicting next word based on the text.
    # Returns character vector with words.
    preprocess.text <- function(data.text, removeStopwords = FALSE) {
        # Split the text on sentences and keep only the last one.
        data.sentences <- unlist(tokenizers::tokenize_sentences(data.text))
        data.sentences.length <- length(data.sentences)
        if (data.sentences.length < 1) {
            return (NA)
        }
        text <- data.sentences[data.sentences.length]
        
        # Preprocess the text.
        text <- preprocess.removeUrl(text)
        text <- preprocess.removeEmail(text)
        text <- preprocess.removeTagsAndHandles(text)
        text <- preprocess.removeUnderscores(text)
        text <- preprocess.addMissingSpace(text)
        
        tokens <- preprocess.tokenize(text)
        tokens <- preprocess.replaceWords(tokens)
        if (removeStopwords) {
            tokens <- preprocess.removeStopwords(tokens)
        }
        tokens <- preprocess.removeNonEnglish(tokens)
        tokens <- preprocess.stem.word(tokens)
        tokens <- preprocess.addSentenceTokens(tokens)
        
        tokens
    }
# }