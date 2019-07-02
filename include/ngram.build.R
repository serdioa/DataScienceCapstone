#
# Build n-grams and calculate their observed frequencies.
#
# Exported functions:
#
#
#
#
# if (!exists('ngram.build')) {
    ngram.build <- TRUE

    library(data.table)
    library(parallel)
    library(quanteda)
    library(readr)
    
    # Build a Feature Vector for 1-grams, that is words.
    # This is a separate function as some steps could be skipped.
    ngram.build.1 <- function(text, stem = FALSE, tolower = FALSE) {
        # Split text on 1-grams.
        message("Splitting text on tokens")
        text.tokens <- tokens(text)
        
        if (tolower) {
            # Transform to lower case, except of the special token
            # Start-of-Sentence.
            message("Transforming tokens to the lower case")
            text.tokens <- mclapply(text.tokens, function(x) ifelse(x == "STOS", x, tolower(x)))
        }
        
        if (stem) {
            # Stem words, if requested. We are using an explicit stemming to
            # make sure that it is fully compatible with another places later in
            # the code where we have to apply the stemming manually.
            message("Stemming tokens")
            text.tokens <- as.tokens(
                mclapply(text.tokens, function(x) SnowballC::wordStem(x, language = "en")))
        }

        # Calculate the Document Feature Matrix
        message("Calculating the Document Feature Matrix")
        text.dfm <- dfm(text.tokens, tolower = FALSE)

        # Transform to a Feature Vector
        message("Transforming the Document Feature Matrix to a Feature Vector")
        dfm2fv(text.dfm)
    }
    
    # Transform a Document Feature Matrix to a Frequency Vector.
    dfm2fv <- function(text.dfm) {
        # Sum over all documents.
        text.dfm.colSums <- colSums(text.dfm)

        # Transform a Frequency Vector to a table and sort by frequency descending.
        tbl <- data.table(Terms = names(text.dfm.colSums), Freq = text.dfm.colSums)
        tbl[order(-Freq)]
    }

    load.preprocessed <- function() {
        if (!exists("all.text.preprocessed")) {
            message("Loading pre-processed blogs")
            blogs.text.preprocessed <- read_lines("cache/blogs.text.preprocessed.txt")
            message("Done loading pre-processed blogs")
            
            message("Loading pre-processed news")
            news.text.preprocessed <- read_lines("cache/news.text.preprocessed.txt")
            message("Done loading pre-processed news")
            
            message("Loading pre-processed twitter")
            twitter.text.preprocessed <- read_lines("cache/twitter.text.preprocessed.txt")
            message("Done loading pre-processed twitter")
            
            all.text.preprocessed <<- c(blogs.text.preprocessed,
                                        news.text.preprocessed,
                                        twitter.text.preprocessed)
        }
        if (!exists("all.text.100")) {
            all.text.100 <<- head(all.text.preprocessed, 100)
        }
        if (!exists("all.text.10000")) {
            all.text.10000 <<- head(all.text.preprocessed, 10000)
        }
    }
    
    # Builds a Frequency Vector for words in the aggregated corpus.
    # Words are not stemmed or transformed to a lower case.
    words.freq.cache <- function() {
        words.freq.file <- "cache/words.freq.RDS"
        if (!file.exists(words.freq.file)) {
            # Load aggregated corpus in the global environment.
            load.preprocessed()
            
            # Calculate the Frequency Vector
            message("Building 1-grams")
            words.freq <- ngram.build.1(all.text.preprocessed)
            message("Done building 1-grams")
            
            # Cache the Frequency Vector.
            message("Saving 1-grams")
            saveRDS(words.freq, words.freq.file)
            message("Done saving 1-grams")
        } else {
            # Load the cached Frequency Vector.
            message("Loading 1-grams")
            words.freq <- readRDS(words.freq.file)
            message("Done loading 1-grams")
        }
        
        words.freq
    }
    
    # Builds a Frequency Vector for stems in the aggregated corpus.
    # Words are stemmed and transformed to a lower case.
    stems.freq.cache <- function() {
        stems.freq.file <- "cache/stems.freq.RDS"
        if (!file.exists(stems.freq.file)) {
            # Load aggregated corpus in the global environment.
            load.preprocessed()
            
            # Calculate the Frequency Vector
            message("Building 1-grams")
            stems.freq <- ngram.build.1(all.text.preprocessed, stem = TRUE,
                                        tolower = TRUE)
            message("Done building 1-grams")
            
            # Cache the Frequency Vector.
            message("Saving 1-grams")
            saveRDS(stems.freq, stems.freq.file)
            message("Done saving 1-grams")
        } else {
            # Load the cached Frequency Vector.
            message("Loading 1-grams")
            stems.freq <- readRDS(stems.freq.file)
            message("Done loading 1-grams")
        }
        
        stems.freq
    }    
# }