#
# Prediction using Stupid Backoff estimation.
#
# if (!exists('prepare.sb')) {
    prepare.sb <- TRUE

    source("include/prepare.sb.R")
    
    # Transorms the probability from the filtered table from log to standard.
    sb.probability.word.prob <- function(table.found) {
        if (nrow(table.found) > 0) {
            # Transform from Log to normal probability.
            exp(table.found$Prob / 1000000)
        } else {
            # Now data found, return NA.
            NA
        }        
    }
    
    sb.probability.word.1 <- function(suffix, removeStopwords = FALSE) {
        # Load n-grams.
        table.sb <- table.optimize.sb.cache(1, 5, removeStopwords)
        
        # Search by suffix.
        table.found <- table.sb %>% filter(Suffix == suffix)
        sb.probability.word.prob(table.found)
    }
        
    sb.probability.word.2 <- function(prefix.tokens, suffix, removeStopwords = FALSE) {
        # Load n-grams.
        table.sb <- table.optimize.sb.cache(2, 5, removeStopwords)
        
        # Encode prefix.
        dict.hash <- dict.hash.cache(removeStopwords)
        prefix.code <- tokens.encode.1(tail(prefix.tokens, 1), dict.hash)

        # Search by prefix, filter by suffix.
        table.found <- table.sb[.(prefix.code)][Suffix == suffix]
        sb.probability.word.prob(table.found)
    }
    
    sb.probability.word.3 <- function(prefix.tokens, suffix, removeStopwords = FALSE) {
        # Load n-grams.
        table.sb <- table.optimize.sb.cache(3, 5, removeStopwords)
        
        # Encode prefix.
        dict.hash <- dict.hash.cache(removeStopwords)
        prefix.code <- tokens.encode.2(tail(prefix.tokens, 2), dict.hash)

        # Search by prefix, filter by suffix.
        table.found <- table.sb[.(prefix.code)][Suffix == suffix]
        sb.probability.word.prob(table.found)
    }
    
    sb.probability.word.4 <- function(prefix.tokens, suffix, removeStopwords = FALSE) {
        # Load n-grams.
        table.sb <- table.optimize.sb.cache(4, 5, removeStopwords)
        
        # Encode prefix.
        dict.hash <- dict.hash.cache(removeStopwords)
        prefix.code <- tokens.encode.3(tail(prefix.tokens, 3), dict.hash)
        
        # Search by prefix, filter by suffix.
        table.found <- table.sb[.(prefix.code)][Suffix == suffix]
        sb.probability.word.prob(table.found)
    }
    
    sb.probability.word.5 <- function(prefix.tokens, suffix, removeStopwords = FALSE) {
        # Load n-grams.
        table.sb <- table.optimize.sb.cache(5, 5, removeStopwords)
        
        # Encode prefix.
        dict.hash <- dict.hash.cache(removeStopwords)
        prefix.code <- tokens.encode.4(tail(prefix.tokens, 4), dict.hash)

        # Search by prefix, filter by suffix.
        table.found <- table.sb[.(prefix.code)][Suffix == suffix]
        sb.probability.word.prob(table.found)
    }
    
    # Suffix must be a single word.
    sb.probability.word <- function(prefix, suffix, removeStopwords = FALSE) {
        prefix.length = length(prefix)
        prob = NA
        
        if (prefix.length >= 4) {
            prob <- sb.probability.word.5(prefix, suffix, removeStopwords)
            if (!is.na(prob)) {
                found.length = 4
            }
        }
        if (is.na(prob) && prefix.length >= 3) {
            prob <- sb.probability.word.4(prefix, suffix, removeStopwords)
            if (!is.na(prob)) {
                found.length = 3
            }
        }
        if (is.na(prob) && prefix.length >= 2) {
            prob <- sb.probability.word.3(prefix, suffix, removeStopwords)
            if (!is.na(prob)) {
                found.length = 2
            }
        }
        if (is.na(prob) && prefix.length >= 1) {
            prob <- sb.probability.word.2(prefix, suffix, removeStopwords)
            if (!is.na(prob)) {
                found.length = 1
            }
        }
        if (is.na(prob)) {
            prob <- sb.probability.word.1(suffix, removeStopwords)
            if (!is.na(prob)) {
                found.length = 0
            }
        }
        if (is.na(prob)) {
            # Not found - return 0
            0
        } else {
            prob * (0.4 ^ (min(4, prefix.length) - found.length))
        }
    }
    
    # Suffix may be a vector of words.
    sb.probability <- function(prefix, suffix, removeStopwords = FALSE) {
        suffix.length <- length(suffix)
        prob <- numeric(suffix.length)
        names(prob) <- suffix
        
        for (i in 1:suffix.length) {
            prob[i] <- sb.probability.word(prefix, suffix[i], removeStopwords)
        }
        
        prob
    }
    
    sb.probability.text <- function(text, suffix, removeStopwords = FALSE) {
        prefix.tokens <- preprocess.text(text, removeStopwords = removeStopwords)
        sb.probability(prefix.tokens, suffix, removeStopwords = removeStopwords)
    }
    
    # Predicts the next word following the specified tokens.
    # 
    # prefix - character vector with tokens.
    # n - number of candidates to return.
    sb.predict <- function(prefix, n = 5, threshold = 5, removeStopwords = FALSE) {
        prefix.length <- length(prefix)

        # Load the table for encoding the prefixes.        
        dict.hash <- dict.hash.cache(removeStopwords)

        # Create a table to hold results.
        candidates <- data.frame(Prefix = character(),
                                 Suffix = character(),
                                 N = integer(),
                                 SuffixProb = numeric(),
                                 Score = numeric())
        
        # Choose candidates from 5- to 2-grms.
        # 1-grams (context-free prediction which completely ignores the prefix)
        # are used only as the last resort if not enough candidates are found.
        for (i in 4:1) {
            if (prefix.length >= i) {
                # Choose the right encoding function.
                tokens.encode <- tokens.encode.n(i)
                
                # Encode the (i-1) part of the prefix.
                prefix.n <- tail(prefix, i)
                prefix.n.code <- tokens.encode(prefix.n, dict.hash)

                # Choose candidates starting with our prefix from optimized
                # table with n-grams.
                table.ngram.n <- table.optimize.sb.cache(i + 1,
                                                         threshold = threshold,
                                                         removeStopwords = removeStopwords)
                table.candidates.n <- table.ngram.n[.(prefix.n.code), nomatch = NULL][!(Suffix %in% candidates$Suffix)]

                if (nrow(table.candidates.n) > 0) {
                    table.candidates.n <- table.candidates.n %>%
                        transmute(Prefix = paste0(prefix.n, collapse = " "),
                                  Suffix = Suffix,
                                  N = i,
                                  SuffixProb = Prob,
                                  Score = exp(Prob / 1000000 + log(0.4) * (min(4, prefix.length) - i)))

                    candidates <- rbind(candidates, table.candidates.n)
                }
            }
        }
        
        # If we still have not enough candidates, choose from 1-grams
        # context-free, that is without taking the prefix in the consideration.
        candidates.found <- nrow(candidates)
        if (candidates.found < n) {
            table.ngram.1 <- table.optimize.sb.cache(1, removeStopwords = removeStopwords)
            table.candidates.1 <- table.ngram.1 %>%
                filter(!(Suffix %in% candidates$Suffix)) %>%
                transmute(Prefix = "",
                          Suffix = Suffix,
                          N = 0,
                          SuffixProb = Prob,
                          Score = exp(Prob / 1000000 + 4 * log(0.4))) %>%
                head(n - candidates.found)
            
            candidates <- rbind(candidates, table.candidates.1)
        }
        
        candidates %>%
            arrange(desc(Score)) %>%
            head(n) %>%
            mutate_if(is.factor, as.character)
    }
    
    sb.predict.text <- function(text, n = 5, threshold = 5, removeStopwords = FALSE) {
        prefix.tokens <- preprocess.text(text, removeStopwords = removeStopwords)
        sb.predict(prefix.tokens, n = n, threshold = threshold, removeStopwords = removeStopwords)
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
    
    test.candidates <- list(
        c("soda", "beer", "cheese", "pretzels"),
        c("world", "best", "most", "universe"),
        c("happiest", "saddest", "smelliest", "bluest"),
        c("referees", "crowd", "defense", "players"),
        c("beach", "mall", "grocery", "movies"),
        c("way", "motorcycle", "horse", "phone"),
        c("weeks", "time", "thing", "years"),
        c("fingers", "toes", "ears", "eyes"),
        c("sad", "bad", "hard", "worse"),
        c("insensitive", "callous", "insane", "asleep"),
        c("sleep", "give", "die", "eat"),
        c("horticultural", "financial", "marital", "spiritual"),
        c("morning", "decade", "month", "weekend"),
        c("sleepiness", "happiness", "stress", "hunger"),
        c("minute", "look", "walk", "picture"),
        c("account", "matter", "incident", "case"),
        c("finger", "arm", "hand", "toe"),
        c("side", "top", "center", "middle"),
        c("weekly", "inside", "daily", "outside"),
        c("pictures", "movies", "novels", "stories")
    )
    
    sb.probability.test <- function(removeStopwords = FALSE) {
        for (i in 1 : length(test.sample)) {
            prefix <- test.sample[[i]]
            prefix.tokens <- preprocess.text(prefix, removeStopwords = removeStopwords)
            suffix.candidates <- test.candidates[[i]]
            suffix.prob <- sb.probability(prefix.tokens, suffix.candidates, removeStopwords = removeStopwords)
            
            suffix.tbl <- data.frame(Suffix = suffix.candidates,
                                     Score = suffix.prob) %>%
                arrange(desc(Score))
            
            print(paste0("Prefix: ", prefix))
            print(suffix.tbl)
            print("")
        }
    }
    
    sb.predict.test <- function(removeStopwords = FALSE) {
        for (i in 1 : length(test.sample)) {
            prefix <- test.sample[[i]]
            suffix.candidates <- sb.predict.text(prefix, n = 5, removeStopwords = removeStopwords)
            
            message("Prefix: ", prefix)
            print(suffix.candidates)
            print("")
        }
    }
# }