#
# Build n-grams and calculate their observed frequencies.
#
# Exported functions:
#
# ngram.freq.cache() - build frequency table for n-grams.
# ngram.enriched.cache() - build table for n-grams prepared for the prediction
#       algorithm.
#
library(data.table)
library(fastmatch)
library(parallel)
library(quanteda)
library(readr)
library(dplyr)

source("include/cache.R")

#
# Build a frequency table for 1-grams, that is words.
# This is a separate function as some steps could be skipped.
#
# @param text the character vector where each element contains a sentence.
# @param stem TRUE to stem words. Defaults to FALSE.
# @param tolower TRUE to transform words to lower case. Defaults to FALSE.
#
# @return a frequency table with columns "Terms" (words) and "Freq" (count of
#       the word in the corpus), ordered by "Freq" in descending order.
# 
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
    
    # Calculate the Document Feature Matrix.
    message("Calculating the Document Feature Matrix")
    text.dfm <- dfm(text.tokens, tolower = FALSE)
    
    # Transform to a frequency table.
    message("Transforming the Document Feature Matrix to a frequency table")
    dfm2ft(text.dfm)
}

#
# Builds a frequency table for n-grams for the given n.
#
# All words except the last are normalized, that is stemmed and transformed to
# the lower case. If a word is not in the list of top-frequency stems as
# returned by the function stems.top.cache(), the word is replaced by a special
# token "UNK" (Unknown). The last word of each n-gram remains "as is".
#
# @param text the character vector where each element contains a sentence.
# @param n the parameter n to build n-grams for.
#
# @return a frequency table with columns "Terms" (n-grams) and "Freq" (count of
#       the n-gram in the corpus), ordered by "Freq" in descending order.
#
ngram.build.n <- function(text, n) {
    if (n < 2) {
        stop("invalid n for building n-grams:", n)
    }
    
    # Define help functions
    
    # Load top stems to keep.
    stems.top <- stems.top.cache()
    
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
    
    # Calculate the Document Feature Matrix.
    message("Calculating the Document Feature Matrix")
    text.dfm <- dfm(as.tokens(text.tokens), tolower = FALSE)
    
    # Transform to a frequency table.
    message("Transforming the Document Feature Matrix to a frequency table")
    dfm2ft(text.dfm)
}    

#
# Transform a Document Feature Matrix to a frequency table. The frequency table
# contains columns "Terms" (n-gram) and "Freq" (count of the n-gram in the
# corpus), ordered by "Freq" in descending order.
#
# @param text.dfm Document Future Matrix as calculated by the function
#       quanteda::dfm().
#
# @return the frequency table.
#
dfm2ft <- function(text.dfm) {
    # Sum over all documents.
    text.dfm.colSums <- colSums(text.dfm)
    
    # Transform a Frequency Vector to a table and sort by frequency descending.
    tbl <- data.table(Terms = names(text.dfm.colSums), Freq = text.dfm.colSums)
    tbl[order(-Freq)]
}

#
# Builds a frequency table for stems in the aggregated corpora.
# Words are stemmed and transformed to a lower case.
#
# @param removeStopwords TRUE to build a frequency table for stems with stop
#       words removed. Defaults to FALSE.
#
# @return the frequency table for stems in the aggregated corpora
#
stems.freq.build <- function(removeStopwords = FALSE) {
    all.preprocessed <- text.preprocessed.cache("all", removeStopwords)
    ngram.build.1(all.preprocessed, stem = TRUE, tolower = TRUE)
}

#
# Returns a cached frequency table for stems in the aggregated corpora.
# The table is either returned from the cache, or is build and cached for the
# future usage. Words are stemmed and transformed to a lower case.
#
# @param removeStopwords TRUE to return a frequency table for stems with stop
#       words removed. Defaults to FALSE.
#
# @return the frequency table for stems in the aggregated corpora
#
#
stems.freq.cache <- function(removeStopwords = FALSE) {
    var.name <- "stems.freq"
    var.build <- function() stems.freq.build(removeStopwords)
    
    get.var.cache(var.name, var.build, removeStopwords)
}

#
# Builds a frequency table for top-frequency stems in the aggregated corpora.
# The table contains top 2^16 - 2 stems, making place for 2 special tokens
# UNK (Unknown) and STOS (Start-of-Sentence).
#
# @param removeStopwords TRUE to build a frequency table for top-frequency stems
#       with stop words removed. Defaults to FALSE.
#
# @return the frequency table for (2^16 - 2) top-frequency stems in the
#       aggregated corpora.
#
stems.top.build <- function(removeStopwords = FALSE) {
    stems.freq <- stems.freq.cache(removeStopwords)
    
    # We will use 2 bytes to encode each stem, but we require 2 additional
    # special tokens: STOS for Start-Of-Sentence, and UNK for a Unknown
    # Word, that is a word which is not included in our encoding table.
    # The token STOS is already included in stems, so we must make place
    # only for the token UNK, that is we may encode (256 * 256 - 2) top
    # stems.
    stems.freq$Terms[1:(256 * 256 - 1)]
}

#
# Returns a cached frequency table for (2^16 - 2) top-frequency  stems in the
# aggregated corpora. The table is either returned from the cache, or is build
# and cached for the future usage. Words are stemmed and transformed to a lower
# case.
#
# @param removeStopwords TRUE to return a frequency table for top-frequency
#       stems with stop words removed. Defaults to FALSE.
#
# @return the frequency table for (2^16 - 2) top-frequency stems in the
#       aggregated corpora.
#
stems.top.cache <- function(removeStopwords = FALSE) {
    var.name = "stems.top"
    var.build <- function() stems.top.build(removeStopwords)
    
    get.var.cache(var.name, var.build, removeStopwords)
}

#
# Returns a cached frequency table for n-grams with the specified n.
#
# @param n the parameter n to build n-grams for.
# @param removeStopWords TRUE to return a frequency table with stop words
#       removed. Defaults to FALSE.
#
# @return a frequency table with columns "Terms" (n-grams) and "Freq" (count of
#       the n-gram in the corpus), ordered by "Freq" in descending order.
#
ngram.freq.cache <- function(n, removeStopwords = FALSE) {
    var.name = paste0("ngram.", n, ".freq")
    var.build <- function() {
        all.preprocessed <- text.preprocessed.cache("all", removeStopwords)
        
        message("Building ", n, "-gram frequency table ",
                ifelse(removeStopwords, "without stop words", "with stop words"))
        if (n == 1) {
            ngram.build.1(all.preprocessed)
        } else {
            ngram.build.n(all.preprocessed, n)
        }
    }
    
    get.var.cache(var.name, var.build, removeStopwords)
}

#
# Builds an extended n-gram frequency table by adding columns "Prefix", "Suffix"
# and "Prob". The "Prefix" and "Suffix" are obtained by splitting the input
# column "Term" (n-gram) on the first (n-1) words ("Prefix") and the last word
# ("Suffix"). For 1-grams the "Prefix" is always NA. The "Prob" is a maximum
# likehood estimate of the probability of the "Prefix" given "Suffix".
#
# @param ngram.freq thre frequency table with columns "Terms" and "Freq".
#
# @return the extended n-gram frequency table with columns "Prefix", "Suffix"
#       and "Prob".
#
ngram.extended.full.build <- function(ngram.freq) {
    message("Splitting n-grams on prefix and suffix")
    splitted <- strsplit(ngram.freq$Terms, "\\s+(?=[^\\s]+$)", perl = TRUE)
    
    message("Merging splitted n-grams to the source data")
    if (length(splitted[[1]]) > 1) {
        ngram.freq <- ngram.freq %>%
            mutate(Prefix = unlist(lapply(splitted, "[[", 1)),
                   Suffix = unlist(lapply(splitted, "[[", 2)),
                   Freq = ngram.freq$Freq)
    } else {
        # Attempted to split 1-grams. No prefixes are available.
        ngram.freq <- ngram.freq %>%
            mutate(Prefix = as.character(NA),
                   Suffix = ngram.freq$Terms,
                   Freq = ngram.freq$Freq)
    }
    
    rm (splitted)
    
    message("Calculating maximum likehood estimate")
    ngram.freq %>%
        group_by(Prefix) %>%
        mutate(Prob = Freq / sum(Freq)) %>%
        ungroup()
}

#
# Caches an extended n-gram frequency table by adding columns "Prefix", "Suffix"
# and "Prob". The "Prefix" and "Suffix" are obtained by splitting the input
# column "Term" (n-gram) on the first (n-1) words ("Prefix") and the last word
# ("Suffix"). For 1-grams the "Prefix" is always NA. The "Prob" is a maximum
# likehood estimate of the probability of the "Prefix" given "Suffix".
#
# @param n the n parameter for n-grams.
# @param removeStopwords TRUE to build the frequency table without stop words.
#       Defaults to FALSE.
#
# @return the extended n-gram frequency table with columns "Prefix", "Suffix"
#       and "Prob".
#
ngram.extended.full.cache <- function(n, removeStopwords = FALSE) {
    var.name <- paste0("ngram.", n, ".extended.full")
    var.build <- function() {
        ngram.freq <- ngram.freq.cache(n, removeStopwords)
        
        message("Building full extended ", n, "-gram frequency table ",
                ifelse(removeStopwords, "without stop words", "with stop words"))
        
        ngram.extended.full.build(ngram.freq)
    }
    
    get.var.cache(var.name, var.build, removeStopwords)
}

#
# Builds a normalized extended n-gram frequency table with columns "Prefix",
# "Suffix" and "Prob". The "Prefix" and "Suffix" are obtained by splitting the
# input column "Term" (n-gram) on the first (n-1) words ("Prefix") and the last
# word ("Suffix"). For 1-grams the "Prefix" is always NA. The "Prob" is a
# maximum likehood estimate of the probability of the "Prefix" given "Suffix".
#
# @param ngram.extended.full.freq the full extended frequency table.
#
# @return the normalized extended n-gram frequency table with columns "Prefix",
#       "Suffix" and "Prob".
#
ngram.extended.build <- function(ngram.extended.full.freq) {
    ngram.extended.full.freq %>%
        select(Prefix, Suffix, Prob)
}

#
# Caches a normalized extended n-gram frequency table by splitting each n-gram
# on a prefix (first n-1 elements) and suffix (last element), and storing them
# in separate columns "Prefix" and "Suffix". For 1-grams the prefix is NA,
# the suffix is the n-gram itself.
#
# @param n the n parameter for n-grams.
# @param removeStopwords TRUE to build the frequency table without stop words.
#       Defaults to FALSE.
#
# @return the normalized extended n-gram frequency table with columns "Prefix"
#       and "Suffix".
#
ngram.extended.cache <- function(n, removeStopwords = FALSE) {
    var.name <- paste0("ngram.", n, ".extended")
    var.build <- function() {
        ngram.extended.full <- ngram.extended.full.cache(n, removeStopwords)

        message("Building normalized extended ", n, "-gram frequency table ",
                ifelse(removeStopwords, "without stop words", "with stop words"))

        ngram.extended.build(ngram.extended.full)
    }
    
    get.var.cache(var.name, var.build, removeStopwords)
}
