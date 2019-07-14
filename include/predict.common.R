#
# Common function for predicting text based on the model.
#
# if (!exists('predict.common')) {
    predict.common <- TRUE
    
    library("rlang")
    source("include/preprocess.R")
    
    get.cache <- function (removeStopwords = FALSE) {
        var.name <- if (removeStopwords) "cache.nosw" else "cache.sw"
        if (exists(var.name)) {
            var <- get(var.name, inherits = TRUE)
        } else {
            var <- env()
            assign(var.name, var, inherits = TRUE)
        }
    }
    
    # Return name of the directory with cached files with or without stop words.
    cache.dir <- function(removeStopwords = FALSE) {
        if (removeStopwords) {
            "cache.without-stop-words"
        } else {
            "cache.with-stop-words"
        }
    }

    # Get cached variable.
    get.var.cache <- function(var.name, var.build, removeStopwords = FALSE) {
        cache <- get.cache(removeStopwords)
        if (exists(var.name, where = cache)) {
            get(var.name, envir = cache)
        } else {
            file.name <- paste0(cache.dir(removeStopwords), "/", var.name, ".RDS")
            if (file.exists(file.name)) {
                message("Loading ", var.name, " from ", file.name)
                var <- readRDS(file.name)
                assign(var.name, var, envir = cache)
                var
            } else {
                message("Building ", var.name)
                var <- var.build()
                assign(var.name, var, envir = cache)
                
                message("Saving ", var.name, " to ", file.name)
                saveRDS(var, var.name)
                var
            }
        }
    }

    # Loads frequency table for stems, cache.
    stems.freq.cache <- function(removeStopwords = FALSE) {
        get.var.cache("stems.freq", function() {}, removeStopwords)
    }
    
    # We will use 2 bytes to encode each stem, but we require 2 additional
    # special tokens: STOS for Start-Of-Sentence, and UNK for a Unknown
    # Word, that is a word which is not included in our encoding table.
    # The token STOS is already included in stems, so we must make place
    # only for the token UNK, that is we may encode (256 * 256 - 2) top
    # stems.
    stems.freq.top.cache <- function(removeStopwords = FALSE) {
        var.name <- "stems.freq.top"
        var.build <- function() {
            stems.freq <- stems.freq.cache(removeStopwords)
            var <- unlist(stems.freq$Terms[1:(256 * 256 - 1)])
            fmatch.hash("", var)
        }
        get.var.cache(var.name, var.build, removeStopwords)
    }

    # Keep the word, if it is included in the table with top stems.
    # Otherwise, replaces the word with the specified token.
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
        
        # Load the top-frequency stems to keep.
        message("removeStopwords.length=", length(removeStopwords))
        stems.freq.top <- stems.freq.top.cache(removeStopwords)
        
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
        tokens <- preprocess.stem.word(tokens, stems.freq.top)
        tokens <- preprocess.addSentenceTokens(tokens)
        
        tokens
    }
# }