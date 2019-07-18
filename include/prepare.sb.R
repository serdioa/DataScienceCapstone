#
# Prediction using Stupid Backoff estimation.
#
# if (!exists('predict.sb')) {
    predict.sb <- TRUE

    library(data.table)
    library(dplyr)
    library(rlang)
    source("include/predict.common.R")
    source("include/ngram.encode.R")
    
    # Return name of the directory with cached files with or without stop words.
    cache.dir <- function(removeStopwords = FALSE) {
        if (removeStopwords) {
            "cache.without-stop-words"
        } else {
            "cache.with-stop-words"
        }
    }
    
    # Enrich n-grams: add condition probability Suffix|Prefix, add prefix code.
    table.enr.build.tbl <- function(table.ngram, n, removeStopwords = FALSE) {
        message("Enriching ", n, "-gram frequency table ",
                ifelse(removeStopwords, "without", "with"), " stop words ",
                "for Stupid Backoff")
        
        ngram.dict <- ngram.dict.cache(n - 1, removeStopwords)
        ngram.code <- ngram.code.cache(n - 1, removeStopwords)
        
        table.ngram <- 
            table.ngram %>%
            group_by(Prefix) %>%
            mutate(SuffixProb = Freq / sum(Freq),
                   SuffixProbLog = log(Freq) - log(sum(Freq)),
                   PrefixCode = ngram.code[fmatch(Prefix, ngram.dict)])

        table.ngram
    }
    
    # Enrich 1-grams: add condition probability Suffix|Prefix. Do not add
    # prefix code since 1-grams do not have a suffix. 
    table.enr.build.1 <- function(table.ngram, removeStopwords = FALSE) {
        message("Enriching 1-gram frequency table ",
                ifelse(removeStopwords, "without", "with"), " stop words ",
                "for Stupid Backoff")
        
        table.ngram <- 
            table.ngram %>%
            mutate(SuffixProb = Freq / sum(Freq),
                   SuffixProbLog = log(Freq) - log(sum(Freq)))
        
        table.ngram        
    }
    
    # Enrich n-grams: calculate probability of a suffix given prefix.
    table.enr.build <- function(n, removeStopwords = FALSE) {
        table.ngram <- table.ngram.enriched.cache(n, removeStopwords)
        if (n == 1) {
            table.enr.build.1(table.ngram, removeStopwords)
        } else {
            table.enr.build.n(table.ngram, n, removeStopwords)
        }
    }
    
    # Enrich and cache n-grams.
    table.enr.cache <- function(n, removeStopwords = FALSE) {
        var.name <- paste0("table.", n, ".enr")
        var.build <- function() {
            table.enr.build(n, removeStopwords)
        }
        
        get.var.cache(var.name, var.build, removeStopwords)
    }
    
    # Pre-calculate enriched tables for all N.
    table.enr.all <- function() {
        for (n in 1:5) {
            table.enr.cache(n)
            table.enr.cache(n, removeStopwords = TRUE)
        }
    }
    
    # Optimize n-grams for Stupid Backoff algorithm.
    # threshold - the maximum frequency of n-grams to discard.
    table.optimize.sb.build.n <- function(table.ngram, threshold, removeStopwords = FALSE) {
        # Keep only relevant columns.
        # Sort by probability descending.
        table.ngram <- table.ngram %>%
            ungroup() %>%
            filter(Freq > threshold) %>%
            transmute(PrefixCode = PrefixCode,
                      Suffix = Suffix,
                      Prob = as.integer(SuffixProbLog * 1000)) %>%
            arrange(desc(Prob))
        
        # Transform to data.table.
        data.table(table.ngram, key = c("PrefixCode"))
    }
    
    table.optimize.sb.build.1 <- function(table.ngram, threshold, removeStopwords = FALSE) {
        # Keep only relevant columns.
        table.ngram %>%
            filter(Freq > threshold) %>%
            transmute(Suffix = Suffix,
                      Prob = as.integer(SuffixProbLog * 1000)) %>%
            arrange(desc(Prob))
    }
    
    table.optimize.sb.bulid <- function(n, threshold = 5, removeStopwords = FALSE) {
        table.ngram <- table.enr.cache(n, removeStopwords)
        if (n == 1) {
            table.optimize.sb.build.1(table.ngram, threshold, removeStopwords)
        } else {
            table.optimize.sb.build.n(table.ngram, threshold, removeStopwords)
        }
    }
    
    table.optimize.sb.cache <- function(n, threshold = 5, removeStopwords = FALSE) {
        var.name <- paste0("table.sb.", n, ".", threshold)
        var.build <- function() {
            table.optimize.sb.bulid(n, threshold, removeStopwords)
        }
        
        get.var.cache(var.name, var.build, removeStopwords)
    }
# }