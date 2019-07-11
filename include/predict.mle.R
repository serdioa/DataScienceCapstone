#
# Prediction using maximum likehood estimation.
#
# if (!exists('predict.mle')) {
    predict.mle <- TRUE
    
    library(data.table)
    source("include/predict.common.R")
    
    # Return name of the directory with cached files with or without stop words.
    cache.dir <- function(removeStopwords = FALSE) {
        if (removeStopwords) {
            "cache.without-stop-words"
        } else {
            "cache.with-stop-words"
        }
    }
    
    # Load n-grams with or without stop words.
    table.ngram.load <- function(n, removeStopwords = FALSE) {
        message("Loading ", n, "-grams ",
                ifelse(removeStopwords, "without", "with"), " stop words")
        
        # Load the cached table.
        table.ngram.file <- paste0(cache.dir(removeStopwords), "/all.", n, "gram.enriched.RDS")
        table.ngram <- readRDS(table.ngram.file)
        
        # Transform to data.table with only required columns (prefix, suffix,
        # frequency) indexed by prefix.
        message("Preparing ", n, "-grams ",
                ifelse(removeStopwords, "without", "with"), " stop words")
        data.table.ngram <- data.table(Prefix = table.ngram$Prefix,
                                       Suffix = table.ngram$Suffix,
                                       Freq = table.ngram$Freq)
        setkey(data.table.ngram, "Prefix")
        data.table.ngram
    }
    
    # Cache n-grams with or without stop words.
    table.ngram.cache <- function(n, removeStopwords = FALSE) {
        table.ngram.file <- paste0(cache.dir(removeStopwords), "/table.", n, "gram.RDS")
        if (!file.exists(table.ngram.file)) {
            table.ngram <- table.ngram.load(n, removeStopwords)
            
            message("Saving prepared ", n, "-grams ",
                    ifelse(removeStopwords, "without", "with"), " stop words")
            saveRDS(table.ngram, table.ngram.file)
        } else {
            message("Loading prepared ", n, "-grams ",
                    ifelse(removeStopwords, "without", "with"), " stop words")
            table.ngram <- readRDS(table.ngram.file)
        }
        
        table.ngram
    }
    
    # Load n-grams for all available n with or without stop words.
    all.ngram.load <- function(removeStopwords = FALSE) {
        list(table.ngram.cache(1, removeStopwords),
             table.ngram.cache(2, removeStopwords),
             table.ngram.cache(3, removeStopwords),
             table.ngram.cache(4, removeStopwords),
             table.ngram.cache(5, removeStopwords),
             table.ngram.cache(6, removeStopwords))
    }
    
    all.ngram.cache <- function(removeStopwords = FALSE) {
        var.name <- paste0("all.ngram.", ifelse(removeStopwords, "withoutsw", "withsw"))
        if (exists(var.name)) {
            get(var.name, inherits = TRUE)
        } else {
            all.ngram.lst <- all.ngram.load(removeStopwords)
            assign(var.name, all.ngram.lst, inherits = TRUE)
        }
    }
    
    predict.freq.n <- function(x, tokens, n, removeStopwords = FALSE) {
        # If we predict based on n-grams, there should be at least n-1 tokens.
        if (length(tokens) < n-1) {
            message("Can't use ", n, "-grams: only ", length(tokens), " are available")
            return (NA)
        }
        
        # Build the prefix.
        if (n > 1) {
            prefix <- paste(tail(tokens, n-1), collapse = " ")
        } else {
            prefix = NA
        }

        # Get the right table.
        all.ngram <- all.ngram.cache(removeStopwords)
        table.ngram <- all.ngram[[n]]
        table.ngram[.(prefix)][Suffix == x]$Freq[1]
    }

    predict.freq <- function(x, text, removeStopwords = FALSE) {
        print(paste("Predicting for text: ", text))

        # Split text on tokens.
        tokens <- preprocess.text(text, removeStopwords)

        # Prepare table for the result.
        result <- data.table(Token = x)
    
        for (i in 1 : 6) {
            freq.n <- sapply(x, function(x) {
                predict.freq.n(x, tokens, i, removeStopwords = removeStopwords)
            })
            freq.n.table <- data.table(freq.n)
            colnames(freq.n.table) <- paste0("Freq.", i)
            
            result <- cbind(result, freq.n.table)
        }
        
        result
    }

    test <- function(removeStopwords = FALSE) {
        freq <- predict.freq(c("soda", "beer", "cheese", "pretzels"),
                     "The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
                     removeStopwords)
        print(freq)

        freq <- predict.freq(c("world", "best", "most", "universe"),
                             "You're the reason why I smile everyday. Can you follow me please? It would mean the",
                             removeStopwords)
        print(freq)

        freq <- predict.freq(c("happiest", "saddest", "smelliest", "bluest"),
                             "Hey sunshine, can you follow me and make me the",
                             removeStopwords)
        print(freq)

        freq <- predict.freq(c("referees", "crowd", "defense", "players"),
                             "Very early observations on the Bills game: Offense still struggling but the",
                             removeStopwords)
        print(freq)

        freq <- predict.freq(c("beach", "mall", "grocery", "movies"),
                             "Go on a romantic date at the",
                             removeStopwords)
        print(freq)

        freq <- predict.freq(c("way", "motorcycle", "horse", "phone"),
                             "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",
                             removeStopwords)
        print(freq)

        freq <- predict.freq(c("weeks", "time", "thing", "years"),
                             "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",
                             removeStopwords)
        print(freq)

        freq <- predict.freq(c("fingers", "toes", "ears", "eyes"),
                             "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",
                             removeStopwords)
        print(freq)

        freq <- predict.freq(c("sad", "bad", "hard", "worse"),
                             "Be grateful for the good times and keep the faith during the",
                             removeStopwords)
        print(freq)

        freq <- predict.freq(c("insensitive", "callous", "insane", "asleep"),
                             "If this isn't the cutest thing you've ever seen, then you must be",
                             removeStopwords)
        print(freq)
    }
    
    test2 <- function(removeStopwords = FALSE) {
        freq <- predict.freq(c("sleep", "give", "die", "eat"),
                             "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd",
                             removeStopwords)
        print(freq)

        freq <- predict.freq(c("horticultural", "financial", "marital", "spiritual"),
                             "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his",
                             removeStopwords)
        print(freq)

        freq <- predict.freq(c("morning", "decade", "month", "weekend"),
                             "I'd give anything to see arctic monkeys this",
                             removeStopwords)
        print(freq)

        freq <- predict.freq(c("sleepiness", "happiness", "stress", "hunger"),
                             "Talking to your mom has the same effect as a hug and helps reduce your",
                             removeStopwords)
        print(freq)

        freq <- predict.freq(c("minute", "look", "walk", "picture"),
                             "When you were in Holland you were like 1 inch away from me but you hadn't time to take a",
                             removeStopwords)
        print(freq)

        freq <- predict.freq(c("account", "matter", "incident", "case"),
                             "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the",
                             removeStopwords)
        print(freq)

        freq <- predict.freq(c("finger", "arm", "hand", "toe"),
                             "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each",
                             removeStopwords)
        print(freq)

        freq <- predict.freq(c("side", "top", "center", "middle"),
                             "Every inch of you is perfect from the bottom to the",
                             removeStopwords)
        print(freq)

        freq <- predict.freq(c("weekly", "inside", "daily", "outside"),
                             "I’m thankful my childhood was filled with imagination and bruises from playing",
                             removeStopwords)
        print(freq)

        freq <- predict.freq(c("pictures", "movies", "novels", "stories"),
                             "I like how the same people are in almost all of Adam Sandler's",
                             removeStopwords)
        print(freq)
    }
# }