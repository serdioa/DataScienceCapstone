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

    # Prefix is a character vector with tokens.    
    predict.candidates <- function(prefix, n = 5, removeStopwords = FALSE, threshold = 5) {
        prefix.length <- length(prefix)
        
        candidates <- data.table(Prefix = character(),
                                 Suffix = character(),
                                 Freq = integer(),
                                 SuffixProb = numeric(),
                                 N = integer())
        
        # Lookup in 5-grams.
        for (i in 5:1) {
            if (prefix.length >= (i - 1)) {
                prefix.n <- ifelse(i > 1, paste0(tail(prefix, i - 1), collapse = " "), NA)
                
                table.ngram.n <- table.ngram.enrich.cache.n(i, removeStopwords)
                
                # Order by is not required: the table is already pre-sorted by
                # conditional probability.
                candidates.n <- table.ngram.n[.(prefix.n), .(Prefix, Suffix, Freq, SuffixProb), nomatch = NULL]
                candidates.n <- candidates.n[Freq >= threshold]
                candidates.n <- candidates.n[!(Suffix %in% candidates$Suffix)]
                candidates.n <- head(candidates.n, n)
                candidates.n.length <- nrow(candidates.n)
                candidates.n[, ("N") := rep(i, candidates.n.length)]
                #message("Candidates n=", i, ":")
                #print(candidates.n)
                
                candidates <- rbind(candidates, candidates.n)
                
                n <- n - candidates.n.length
                #message("Remaining candidates to find: ", n)
                if (n <= 0) {
                    break
                }
            }
        }
        
        message("Prefix: ", paste(prefix, collapse = " "))
        print(candidates)

        candidates
    }
    
    test.sample <- list(
        "The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
        "You're the reason why I smile everyday. Can you follow me please? It would mean the",
        "Hey sunshine, can you follow me and make me the",
        "Very early observations on the Bills game: Offense still struggling but the",
        "Go on a romantic date at the",
        "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",
        "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",
        "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",
        "Be grateful for the good times and keep the faith during the",
        "If this isn't the cutest thing you've ever seen, then you must be",
        "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd",
        "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his",
        "I'd give anything to see arctic monkeys this",
        "Talking to your mom has the same effect as a hug and helps reduce your",
        "When you were in Holland you were like 1 inch away from me but you hadn't time to take a",
        "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the",
        "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each",
        "Every inch of you is perfect from the bottom to the",
        "I’m thankful my childhood was filled with imagination and bruises from playing",
        "I like how the same people are in almost all of Adam Sandler's"
    )
# }