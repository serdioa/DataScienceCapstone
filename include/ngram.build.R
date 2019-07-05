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
    library(fastmatch)
    library(parallel)
    library(pbmcapply)
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
    
    ngram.build.n.1 <- function(text, n) {
        if (n < 2) {
            stop("invalid n for building n-grams:", n)
        }
        
        # Split text on 1-grams.
        message("Splitting text on tokens")
        text.tokens <- tokens(text)
        
        # Build n-grams.
        message("Building ", n, "-grams")
        text.tokens <- unlist(tokens_ngrams(text.tokens, n = n, concatenator = " "),
                              use.names = FALSE)
        
        # Normalize n-grams.
        message("Normalizing ", n, "-grams")
        text.tokens <- mclapply(text.tokens, function(x) {
            # Split on words again.
            x.words <- unlist(tokens(x))

            # Transform the first n-1 words to stems and to the lower case.
            x.words[1:n-1] <- ifelse(x.words[1:n-1] == "STOS",
                                     x.words[1:n-1],
                                     SnowballC::wordStem(tolower(x.words[1:n-1]), language = "en"))

            # Concatenate words back to a single token.            
            tmp <- paste0(x.words, collapse = " ")
        })
        
        # Calculate the Document Feature Matrix
        message("Calculating the Document Feature Matrix")
        text.dfm <- dfm(as.tokens(text.tokens), tolower = FALSE)
        
        # Transform to a Feature Vector
        message("Transforming the Document Feature Matrix to a Feature Vector")
        dfm2fv(text.dfm)
    }
    
    ngram.build.n <- function(text, n) {
        if (n < 2) {
            stop("invalid n for building n-grams:", n)
        }
        
        # Define help functions
        
        # Load top stems to keep.
        stems.top <- stems.top.cache()$Terms
        
        # Keep the word, if it is included in the table with top stems.
        # Otherwise, replaces the word with the specified token.
        top.stem.keep <- function(stem) {
            ifelse(is.na(fmatch(stem, stems.top)), "UNK", stem)
        }
        
        # Stem the specified token, transform it to the lower case, and
        # replace with the UNK token if it is not in the list of top stems.
        stem.word <- function(x) {
            ifelse(x == "STOS",
                   x,
                   top.stem.keep(SnowballC::wordStem(tolower(x), language = "en")))
        }

        # Split text on 1-grams.
        message("Splitting text on tokens")
        text.tokens <- tokens(text)

        message("Stem tokens and transform them to the lower case")
        text.stems <- mclapply(text.tokens, stem.word)

        # Normalize n-grams.
        message("Normalizing ", n, "-grams")
        text.tokens <- mclapply(1:length(text.tokens), function(x) {
            item.tokens = text.tokens[[x]]
            item.stems = text.stems[[x]]
            
            if (length(item.tokens) < n) {
                c()
            } else {
                sapply(1:(length(item.tokens) - n + 1), function(i) {
                    paste0(c(item.stems[i:(i + n - 2)], item.tokens[i + n - 1]),
                           collapse = " ")
                })
            }
        })
        
        # Clean up the memory.
        rm(text.stems)
        
        # Calculate the Document Feature Matrix
        message("Calculating the Document Feature Matrix")
        text.dfm <- dfm(as.tokens(text.tokens), tolower = FALSE)
        
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
    
    stems.top.cache <- function() {
        # Load all stems.
        stems.freq <- stems.freq.cache()
        
        # We will use 2 bytes to encode each stem, but we require 2 additional
        # special tokens: STOS for Start-Of-Sentence, and UNK for a Unknown
        # Word, that is a word which is not included in our encoding table.
        # The token STOS is already included in stems, so we must make place
        # only for the token UNK, that is we may encode (256 * 256 - 2) top
        # stems.
        data.table(Terms = stems.freq$Terms[1:(256 * 256 - 1)])
    }
    
    all.ngram.freq.cache <- function(n) {
        all.ngram.freq.file <- paste0("cache/all.", n, "gram.freq.RDS")
        if (!file.exists(all.ngram.freq.file)) {
            # Load aggregated corpus in the global environment.
            load.preprocessed()
            
            # Calculate the Frequency Vector
            message("Building ", n, "-grams")
            all.ngram.freq <- ngram.build.n(all.text.preprocessed, n)
            message("Done building ", n, "-grams")
            
            # Cache the Frequency Vector.
            message("Saving ", n, "-grams")
            saveRDS(all.ngram.freq, all.ngram.freq.file)
            message("Done saving ", n, "-grams")
        } else {
            # Load the cached Frequency Vector.
            message("Loading ", n, "-grams")
            all.ngram.freq <- readRDS(all.ngram.freq.file)
            message("Done loading ", n, "-grams")
        }
        
        all.ngram.freq
    }

    options(mc.cores = 2)
    
    # all.ngram.freq.cache(2)
    # all.ngram.freq.cache(3)
    # all.ngram.freq.cache(4)
    # all.ngram.freq.cache(5)
    # all.ngram.freq.cache(6)
    
# }