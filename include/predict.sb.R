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
    
    table.enr.prefix.code <- function(Prefix, n, removeStopwords = FALSE) {
        message("Encoding Prefix in ", n, "-gram frequency table ",
                ifelse(removeStopwords, "without", "with"), " stop words ",
                "for Stupid Backoff")

        ngram.dict <- ngram.dict.cache(n - 1)
        ngram.code <- ngram.code.cache(n - 1)
        
        Prefix.length <- length(Prefix)
        if (n < 3) {
            Prefix.Code <- integer(Prefix.length)
        } else {
            Prefix.Code <- numeric(Prefix.length)
        }
        
        pb <- pbmcapply::progressBar(min = 0,
                                     max = Prefix.length,
                                     initial = 0)
        for (i in 1:Prefix.length) {
            Prefix.Code[i] <- ngram.code[fmatch(Prefix[i], ngram.dict)]
            
            if (i %% 100 == 0) setTxtProgressBar(pb, i)
        }
        close(pb)
        
        Prefix.Code
    }
    
    table.enr.build.tbl <- function(table.ngram, n, removeStopwords = FALSE) {
        message("Enriching ", n, "-gram frequency table ",
                ifelse(removeStopwords, "without", "with"), " stop words ",
                "for Stupid Backoff")

        table.ngram <- 
            table.ngram %>%
            group_by(Prefix) %>%
            mutate(SuffixProb = Freq / sum(Freq),
                   SuffixProbLog = log(Freq) - log(sum(Freq)))
        table.ngram$PrefixCode <- table.enr.prefix.code(table.ngram$Prefix, n,
                                                        removeStopwords)

        table.ngram
    }
    
    # Enrich n-grams: calculate probability of a suffix given prefix.
    table.enr.build <- function(n, removeStopwords = FALSE) {
        table.ngram <- table.ngram.enriched.cache(n, removeStopwords)
        table.enr.build.tbl(table.ngram, n, removeStopwords)
        
        # message("Enriching ", n, "-grams ",
        #         ifelse(removeStopwords, "without", "with"), " stop words ",
        #         "with conditional probabilities")
        # table.ngram %>% group_by(Prefix) %>% mutate(SuffixProb = Freq / sum(Freq))
        # 
        # # Transforming to data.table and setting the key for fast lookup.
        # table.ngram <- data.table(table.ngram)
        # setkey(table.ngram, "Prefix")
        # table.ngram
    }
    
    # Enrich and cache n-grams.
    table.ngram.enrich.cache.n <- function(n, removeStopwords = FALSE) {
        # # If the variable already exist, just return it.
        # var.name <- paste0("table.", n, ".enr.", ifelse(removeStopwords, "nosv", "sv"))
        # cache <- get.cache()
        # if (exists(var.name, envir = cache)) {
        #     var <- get(var.name, envir = cache)
        # } else {
        #     # if the file already exist, read from it.
        #     var.file <- paste0(cache.dir(removeStopwords), "/", var.name, ".RDS")
        #     if (file.exists(var.file)) {
        #         message("Loading ", n, "-grams ",
        #                 ifelse(removeStopwords, "without", "with"), " stop words ",
        #                 "with conditional probabilities")
        #         var <- readRDS(var.file)
        #     } else {
        #         # ... otherwise calculate and save in the file.
        #         var <- table.ngram.enrich.n(n, removeStopwords)
        #         
        #         message("Saving ", n, "-grams ",
        #                 ifelse(removeStopwords, "without", "with"), " stop words ",
        #                 "with conditional probabilities")
        #         saveRDS(var, var.file)
        #     }
        #     
        #     # Put in the memory cache.
        #     assign(var.name, var, envir = cache)
        # }
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
    
    #test.sample.tokens <- lapply(test.sample, preprocess.text)
    
    # test.sample.candidates <- lapply(test.sample.tokens, predict.candidates, n = 5)

    #table.ngram.enrich.cache.n(1)
    #table.ngram.enrich.cache.n(2)
    #table.ngram.enrich.cache.n(3)
    #table.ngram.enrich.cache.n(4)
    #table.ngram.enrich.cache.n(5)
    
    #table.ngram.enrich.cache.n(1, TRUE)
    #table.ngram.enrich.cache.n(2, TRUE)
    #table.ngram.enrich.cache.n(3, TRUE)
    #table.ngram.enrich.cache.n(4, TRUE)
    #table.ngram.enrich.cache.n(5, TRUE)
# }