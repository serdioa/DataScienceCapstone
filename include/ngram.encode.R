#
# Encode words and n-grams to reduce memory usage.
#
# if (!exists('ngram.encode')) {
    ngram.encode <- TRUE

    source("include/predict.common.R")
    library(hashmap)
    library(pbapply)
    library(progress)
    library(tokenizers)

    # Int (0 : 256*256-1) to raw and back
    int2raw <- function(value) {
        low = value %% 256
        high = value %/% 256
        as.raw(c(high, low))
    }
    
    raw2int <- function(value) {
        as.integer(value[1]) * 256 + as.integer(value[2])
    }
    
    # Num (8 bytes) to raw and back.
    num2raw.empty <- raw()
    num2raw <- function(value) {
        writeBin(value, num2raw.empty)
    }
    
    raw2num <- function(value) {
        readBin(value, what = "numeric")
    }
    
    # Load top frequency stems.
    # stems.freq.top.cache(removeStopwords = FALSE)
    
    dict.stems.cache <- function(removeStopwords = FALSE) {
        var.name <- "dict.stems"
        var.build <- function() {
            stems.freq.top <- stems.freq.top.cache(removeStopwords)
            c(stems.freq.top, "UNK")
        }
        
        get.var.cache(var.name, var.build, removeStopwords)
    }
    
    dict.hash.cache <- function(removeStopwords = FALSE) {
        # hashmap does not support RDS persistence (native C structure).
        
        var.name <- "dict.hash"
        var.build <- function() {
            dict.stems <- dict.stems.cache(removeStopwords)
            hashmap(keys = dict.stems, values = 0 : (256*256-1))
        }
        
        get.var.cache(var.name, var.build, removeStopwords, var.persist = FALSE)
    }

    tokens.encode.1 <- function(tokens, dict.hash) {
        # Encoding a single token: just return a code from the table.
        dict.hash[[tokens[1]]]
    }
    
    tokens.encode.2 <- function(tokens, dict.hash) {
        # Encoding 2 tokens: combine raw codes and transform them to a single
        # integer code (4 bytes).
        code.raw <- c(int2raw(dict.hash[[tokens[1]]]),
                      int2raw(dict.hash[[tokens[2]]]))

        readBin(code.raw, what = "integer")
    }
    
    tokens.encode.3 <- function(tokens, dict.hash) {
        # Encoding 3 tokens: combine raw codes and transform them to a single
        # numeric code (8 bytes). Actually we require just 6 bytes, but there
        # is no 6-byte data type in R.
        code.raw <- as.raw(c(int2raw(dict.hash[[tokens[1]]]),
                             int2raw(dict.hash[[tokens[2]]]),
                             int2raw(dict.hash[[tokens[3]]]),
                             0x0, 0x0))
        readBin(code.raw, what = "numeric")
    }
    
    tokens.encode.4 <- function(tokens, dict.hash) {
        # Encoding 4 tokens: combine raw codes and transform them to a single
        # numeric code (8 bytes).
        code.raw <- c(int2raw(dict.hash[[tokens[1]]]),
                      int2raw(dict.hash[[tokens[2]]]),
                      int2raw(dict.hash[[tokens[3]]]),
                      int2raw(dict.hash[[tokens[4]]]))
        readBin(code.raw, what = "numeric")
    }
    
    tokens.encode.n <- function(n) {
        if (n == 1) {
            tokens.encode.1
        } else if (n == 2) {
            tokens.encode.2
        } else if (n == 3) {
            tokens.encode.3
        } else if (n == 4) {
            tokens.encode.4
        }
    }

    tokens.decode.1 <- function(code, dict.stems) {
        # Just use the code to return a token from the table.
        dict.stems[code + 1]
    }
    
    tokens.decode.2 <- function(code, dict.stems) {
        # Decoding 2 tokens: transform the integer code into 4 bytes,
        # split them on 2 x 2 bytes, transform each pair into the index.
        code.raw = writeBin(as.integer(code), num2raw.empty)
        dict.stems[c(raw2int(code.raw[1:2]) + 1,
                     raw2int(code.raw[3:4]) + 1)]
    }
    
    tokens.decode.3 <- function(code, dict.stems) {
        # Decoding 3 tokens: transform the numeric code into 8 bytes,
        # split them on 4x2 bytes, transform the first 3 pairs into indices
        # ignoring the last pair.
        code.raw = writeBin(code, num2raw.empty)
        dict.stems[c(raw2int(code.raw[1:2]) + 1,
                     raw2int(code.raw[3:4]) + 1,
                     raw2int(code.raw[5:6]) + 1)]
    }

    tokens.decode.4 <- function(code, dict.stems) {
        # Decoding 4 tokens: transform the numeric code into 8 bytes,
        # split them on 4x2 bytes, transform each pair into the index.
        code.raw = writeBin(code, num2raw.empty)
        dict.stems[c(raw2int(code.raw[1:2]) + 1,
                     raw2int(code.raw[3:4]) + 1,
                     raw2int(code.raw[5:6]) + 1,
                     raw2int(code.raw[7:8]) + 1)]
    }
    
    tokens.decode.n <- function(n) {
        if (n == 1) {
            tokens.decode.1
        } else if (n == 2) {
            tokens.decode.2
        } else if (n == 3) {
            tokens.decode.3
        } else if (n == 4) {
            tokens.decode.4
        }
    }

    # Encode characters vector of 1-grams.
    ngram.encode.vec.1 <- function(text, dict.hash) {
        message("Encoding 1-grams")
        pbsapply(text, function(x) tokens.encode.1(x, dict.hash),
                 USE.NAMES = FALSE,
                 simplify = TRUE)
    }
    
    # Encode character vector of n-grams, n = 2, 3, 4
    ngram.encode.vec.234 <- function(text, n, dict.hash) {
        message("Encoding ", n, "-grams")
        
        text.length <- length(text)
        if (n < 3) {
            text.code <- integer(text.length)
        } else {
            text.code <- numeric(text.length)
        }
        
        tokens.encode <- tokens.encode.n(n)
        
        pb <- txtProgressBar(min = 0,
                             max = text.length,
                             initial = 0,
                             style = 3)
        
        for (i in 1 : text.length) {
            text.tokens <- tokenizers::tokenize_regex(text[i], simplify = TRUE)
            text.code[i] <- tokens.encode(text.tokens, dict.hash)
            
            setTxtProgressBar(pb, i)
        }
        close(pb)
        
        text.code
    }
    
    # Encode character vector of n-grms, n = 1...4
    ngram.encode.vec.n <- function(text, n, dict.hash) {
        if (n == 1) {
            ngram.encode.vec.1(text, dict.hash)
        } else {
            ngram.encode.vec.234(text, n, dict.hash)
        }
    }

    # Decode provided code to n-grams, n = 1...4
    ngram.decode.vec.n <- function(code, n, dict.stems) {
        message("Decoding ", n, "-grams")
        
        tokens.decode <- tokens.decode.n(n)
        pbsapply(code, function(x) {paste(tokens.decode(x, dict.stems), collapse=" ")})
    }
    
    ngram.dict.build.tbl <- function(ngram.table, n, removeStopwords = FALSE) {
        # Load top-frequency words required for stemming.
        stems.freq.top <- stems.freq.top.cache(removeStopwords)

        # Preprocess n-gram for a dictionary.
        # N-grams are already heavily pre-processed in tables. Only the last
        # word is not yet lowercased and stemmed.
        ngram.dict.build.tbl.preprocess <- function(text) {
            # Split on tokens.
            tokens <- tokenizers::tokenize_regex(text, simplify = TRUE)

            # Lowercase and stem the last word.
            tokens.length <- length(tokens)
            tokens[tokens.length] <- preprocess.stem.word(tokens[tokens.length], stems.freq.top)

            # Concatenate to n-gram again.
            paste0(tokens, collapse = " ")
        }

        # Keep just terms.
        #ngram.index <- sample(1:nrow(ngram.table), 10000)
        #ngram.terms <- ngram.table$Terms[ngram.index]
        ngram.terms <- ngram.table$Terms
        
        # Stem words.
        message("Stemming ", n, "-grams ",
                ifelse(removeStopwords, "without", "with"), " stop words")
        ngram.terms <- pbsapply(ngram.terms, ngram.dict.build.tbl.preprocess,
                                USE.NAMES = FALSE, simplify = TRUE)
        
        # Collect unique ngrams.
        message("Collecting unique ", n, "-grams ",
                ifelse(removeStopwords, "without", "with"), " stop words")
        unique(ngram.terms)
    }
    
    ngram.dict.build <- function(n, removeStopwords = FALSE) {
        # Load n-grams.
        ngram.table <- table.ngram.enriched.cache(n, removeStopwords)
        
        ngram.dict.build.tbl(ngram.table, n, removeStopwords)
    }
    
    ngram.code.build <- function(ngram.dict, n, removeStopwords = FALSE) {
        # Load the dictionary.
        dict.hash <- dict.hash.cache(removeStopwords)
        
        # Encode n-grams.
        message("Encoding unique ", n, "-grams ",
                ifelse(removeStopwords, "without", "with"), " stop words")
        ngram.encode.vec.n(ngram.dict, n, dict.hash)
    }
    
    ngram.dict.cache <- function(n, removeStopwords = FALSE) {
        var.name <- paste0("dict.", n)
        var.build <- function() {
            ngram.dict.build(n, removeStopwords)
        }
        
        get.var.cache(var.name, var.build, removeStopwords)
    }
    
    ngram.code.cache <- function(n, removeStopwords = FALSE) {
        var.name <- paste0("code.", n)
        var.build <- function() {
            ngram.dict <- ngram.dict.cache(n, removeStopwords)
            ngram.code.build(ngram.dict, n, removeStopwords)
        }
        
        get.var.cache(var.name, var.build, removeStopwords)
    }
#}