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
    library(quanteda)
    library(readr)
    library(dplyr)
    
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
        
        # Construct n-grams from the specified stem and word tokens.
        # The first (n-1) elements for each n-gram are taken from stems,
        # the last from words.
        ngram.combine.chunk <- function(stems, words) {
            stems.length <- length(stems)

            lapply(1:stems.length, function(x) {
                item.words <- words[[x]]
                item.stems <- stems[[x]]

                if (length(item.words) < n) {
                    c()
                } else {
                    sapply(1:(length(item.words) - n + 1), function(i) {
                        paste0(c(item.stems[i:(i + n - 2)], item.words[i + n - 1]),
                               collapse = " ")
                    })
                }
            })
        }

        # Split text on 1-grams.
        message("Splitting text on tokens")
        text.tokens <- tokens(text)

        message("Stem tokens and transform them to the lower case")
        text.stems <- mclapply(text.tokens, stem.word)

        # Normalize n-grams.
        message("Normalizing ", n, "-grams")
        startTs <- Sys.time()
        message("Started at ", startTs)
        text.tokens.processed <- c()
        step = 1000
        indices <- seq(from = 1, to = length(text.tokens), by = step)
        for (startIndex in indices) {
            endIndex <- min(startIndex + step - 1, length(text.tokens))
            message("Processing ", startIndex, " to ", endIndex, " of ",
                    length(text.tokens))
            
            text.tokens.batch <- text.tokens[startIndex:endIndex]
            text.stems.batch <- text.stems[startIndex:endIndex]

            text.tokens.batch.processed <- ngram.combine.chunk(text.stems.batch,
                                                               text.tokens.batch)
            text.tokens.processed <- c(text.tokens.processed, text.tokens.batch.processed)

            processedPct <- endIndex / length(text.tokens)
            currTs <- Sys.time()
            diffTs <- currTs - startTs
            expectedTs <- currTs + diffTs * (1 - processedPct) / processedPct
            
            message("Processed ", (100 * processedPct), 
                    "%, expected to finish at ", expectedTs)
        }
        currTs <- Sys.time()
        message("Finished normalizing ", n, "-grams at ", currTs)
        message("Duration: ", (currTs - startTs))
        
        export.text.tokens.processed <- text.tokens.processed
        
        text.tokens <- text.tokens.processed
        message("Final total processed size: ", length(text.tokens))
        
        # Clean up the memory.
        rm(text.stems, text.tokens.processed)
        
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
    
    # Enrich the n-gram frequency table.
    # Split n-gram on prefix (first n-1 elements) and suffix (last element)
    # and store them in separate columns "Prefix" and "Suffix".
    ngram.enrich.n <- function(ngram.freq) {
        message("Splitting n-grams on prefix and postfix")
        splitted <- strsplit(ngram.freq$Terms, "\\s+(?=[^\\s]+$)", perl = TRUE)
        
        message("Merging splitted n-grams to the source data")
        ngram.freq %>%
            mutate(Prefix = unlist(lapply(splitted, "[[", 1)),
                   Suffix = unlist(lapply(splitted, "[[", 2)))
    }
    
    # Collapse n-gram frequency table by keeping for each prefix only a row
    # with the most frequently encountered suffix. If for a particular prefix
    # several suffixes have the same maximum frequency, use any one of them.
    # The returned data frame is sorted by the prefix ascending, so that one
    # may use it for a binary search.
    ngram.collapse.n <- function(ngram.freq.enriched) {
        ngram.freq.enriched %>%
            select(Prefix, Suffix, Freq) %>%
            group_by(Prefix) %>%
            top_n(1, Freq) %>%
            slice(1) %>%
            arrange(Prefix)
    }
    
    all.ngram.collapsed.cache <- function(n) {
        all.ngram.collapsed.file <- paste0("cache/all.", n, "gram.collapsed.RDS")
        if (!file.exists(all.ngram.collapsed.file)) {
            # Load the frequency table.
            all.ngram.freq <- all.ngram.freq.cache(n)
            
            # Enrich the frequency table.
            message("Enriching the frequency table for ", n, "-grams")
            all.ngram.freq <- ngram.enrich.n(all.ngram.freq)
            
            # Collapse the frequency table.
            message("Collapsing the frequency table for ", n, "-grams")
            all.ngram.collapsed <- ngram.collapse.n(all.ngram.freq)

            # Cache the frequency table.
            message("Saving collapsed ", n, "-grams")
            saveRDS(all.ngram.collapsed, all.ngram.collapsed.file)
            message("Done saving collapsed ", n, "-grams")
        } else {
            # Load the cached Collapsed Frequency Vector.
            message("Loading collapsed ", n, "-grams")
            all.ngram.collapsed <- readRDS(all.ngram.collapsed.file)
            message("Done loading collapsed ", n, "-grams")
        }
        
        all.ngram.collapsed
    }

    options(mc.cores = 2)
    
    # all.ngram.freq.cache(2)
    # all.ngram.freq.cache(3)
    # all.ngram.freq.cache(4)
    # all.ngram.freq.cache(5)
    # all.ngram.freq.cache(6)
    
    # all.ngram.collapsed.cache(2)
    # all.ngram.collapsed.cache(3)
    # all.ngram.collapsed.cache(4)
    # all.ngram.collapsed.cache(5)
    # all.ngram.collapsed.cache(6)

# }