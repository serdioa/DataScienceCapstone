#
# Encode words and n-grams to reduce memory usage.
#
source("include/cache.R")
source("include/ngram.build.R")

library(hashmap)
library(pbapply)
library(progress)
library(tokenizers)

#
# Encode an integer value in a range of 0 to (2^16 - 1) to the raw data type.
#
# @param value the integer value in the range of 0 to (2^16 - 1).
#
# @return the raw representation.
#
int2raw <- function(value) {
    low = value %% 256
    high = value %/% 256
    as.raw(c(high, low))
}

#
# Decode a raw representation of an integer in a range of 0 to (2^16 - 1).
#
# @param the raw representation.
#
# @return the decoded integer.
#
raw2int <- function(value) {
    as.integer(value[1]) * 256 + as.integer(value[2])
}

#
# Cache an empty raw object to not create the new one every time.
#
num2raw.empty <- raw()

#
# Encode a numeric value 
#
#num2raw <- function(value) {
#    writeBin(value, num2raw.empty)
#}

#raw2num <- function(value) {
#    readBin(value, what = "numeric")
#}

#
# Returns a cached character vector with top-frequency stems supported by the
# encoder/decoder. The character vector contains (2^16 - 2) stems and the
# special token "STOS" (Start-of-Sentence) returned by the method
# stems.top.cache(), we well as the special token "UNK" (Unknown), in total
# 2^16 elements.
#
# @param removeStopwords TRUE to return the top-frequency stems with stop words
#       removed. Defaults to FALSE.
#
# @return the cached character vector with top-frequency stems supported by the
#       encoder/decoder, with 2^16 elements.
#
dict.stems.cache <- function(removeStopwords = FALSE) {
    var.name <- "dict.stems"
    var.build <- function() {
        stems.top <- stems.top.cache(removeStopwords)
        c("UNK", stems.top$Terms)
    }
    
    get.var.cache(var.name, var.build, removeStopwords)
}

#
# Returns a cached hash table mapping 2^16 top-frequency stems as returned
# by the fuction dict.stems.cache() to their codes. The codes are in the range
# 0 to 2^16 - 1. The hash table is used for the fast lookup.
#
# @param removeStopwords TRUE to return a hash table with stop words removed.
#       Defaults to FALSE.
#
# @return the cached hash table mapping top-frequency stems to their codes.
#
dict.hash.cache <- function(removeStopwords = FALSE) {
    # hashmap does not support RDS persistence (native C structure).
    
    var.name <- "dict.hash"
    var.build <- function() {
        dict.stems <- dict.stems.cache(removeStopwords)
        hashmap(keys = dict.stems, values = 0 : (256*256-1))
    }
    
    get.var.cache(var.name, var.build, removeStopwords, var.persist = FALSE)
}

#
# Encodes 1-gram prefix of the provided tokens using the given encoding
# dictionary. The dictionary may be obtained by using the function
# dict.hash.cache().
#
# @param tokens the character vector.
# @param dict.hash the encoding dictionary.
#
# @return the encoded 1-gram.
#
tokens.encode.1 <- function(tokens, dict.hash) {
    # Encoding a single token: just return a code from the table.
    dict.hash[[tokens[1]]]
}

#
# Encodes 2-gram prefix of the provided tokens using the given encoding
# dictionary. The dictionary may be obtained by using the function
# dict.hash.cache().
#
# @param tokens the character vector.
# @param dict.hash the encoding dictionary.
#
# @return the encoded 2-gram.
#
tokens.encode.2 <- function(tokens, dict.hash) {
    # Encoding 2 tokens: combine raw codes and transform them to a single
    # integer code (4 bytes).
    code.raw <- c(int2raw(dict.hash[[tokens[1]]]),
                  int2raw(dict.hash[[tokens[2]]]))
    
    readBin(code.raw, what = "integer", endian = "little")
}

#
# Encodes 3-gram prefix of the provided tokens using the given encoding
# dictionary. The dictionary may be obtained by using the function
# dict.hash.cache().
#
# @param tokens the character vector.
# @param dict.hash the encoding dictionary.
#
# @return the encoded 3-gram.
#
tokens.encode.3 <- function(tokens, dict.hash) {
    # Encoding 3 tokens: combine raw codes and transform them to a single
    # numeric code (8 bytes). Actually we require just 6 bytes, but there
    # is no 6-byte data type in R.
    code.raw <- as.raw(c(int2raw(dict.hash[[tokens[1]]]),
                         int2raw(dict.hash[[tokens[2]]]),
                         int2raw(dict.hash[[tokens[3]]]),
                         0x0, 0x0))
    readBin(code.raw, what = "numeric", endian = "little")
}

#
# Encodes 4-gram prefix of the provided tokens using the given encoding
# dictionary. The dictionary may be obtained by using the function
# dict.hash.cache().
#
# @param tokens the character vector.
# @param dict.hash the encoding dictionary.
#
# @return the encoded 4-gram.
#
tokens.encode.4 <- function(tokens, dict.hash) {
    # Encoding 4 tokens: combine raw codes and transform them to a single
    # numeric code (8 bytes).
    code.raw <- c(int2raw(dict.hash[[tokens[1]]]),
                  int2raw(dict.hash[[tokens[2]]]),
                  int2raw(dict.hash[[tokens[3]]]),
                  int2raw(dict.hash[[tokens[4]]]))
    readBin(code.raw, what = "numeric", endian = "little")
}

#
# Returns the encoding function for the specified n. The returned function may
# be used to encode n-grams, n from 1 to 4.
#
# @param n the parameter for choosing the encoding function, 1 to 4.
#
# @return the encoding function for n-grams.
#
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

#
# Decodes a 1-gram from the specified integer code using the given decoding
# dictionary. The dictionary may be obtained by using the function
# dict.stems.cache().
#
# @param code the integer code.
# @param dict.stems the decoding dictionary.
#
# @return the decoded 1-gram.
#
tokens.decode.1 <- function(code, dict.stems) {
    # Just use the code to return a token from the table.
    dict.stems[code + 1]
}

#
# Decodes a 2-gram from the specified integer code using the given decoding
# dictionary. The dictionary may be obtained by using the function
# dict.stems.cache().
#
# @param code the integer code.
# @param dict.stems the decoding dictionary.
#
# @return the decoded 2-gram.
#
tokens.decode.2 <- function(code, dict.stems) {
    # Decoding 2 tokens: transform the integer code into 4 bytes,
    # split them on 2 x 2 bytes, transform each pair into the index.
    code.raw = writeBin(as.integer(code), num2raw.empty, endian = "little")
    dict.stems[c(raw2int(code.raw[1:2]) + 1,
                 raw2int(code.raw[3:4]) + 1)]
}

#
# Decodes a 3-gram from the specified numeric code using the given decoding
# dictionary. The dictionary may be obtained by using the function
# dict.stems.cache().
#
# @param code the numeric code.
# @param dict.stems the decoding dictionary.
#
# @return the decoded 3-gram.
#
tokens.decode.3 <- function(code, dict.stems) {
    # Decoding 3 tokens: transform the numeric code into 8 bytes,
    # split them on 4x2 bytes, transform the first 3 pairs into indices
    # ignoring the last pair.
    code.raw = writeBin(code, num2raw.empty, endian = "little")
    dict.stems[c(raw2int(code.raw[1:2]) + 1,
                 raw2int(code.raw[3:4]) + 1,
                 raw2int(code.raw[5:6]) + 1)]
}

#
# Decodes a 4-gram from the specified numeric code using the given decoding
# dictionary. The dictionary may be obtained by using the function
# dict.stems.cache().
#
# @param code the numeric code.
# @param dict.stems the decoding dictionary.
#
# @return the decoded 4-gram.
#
tokens.decode.4 <- function(code, dict.stems) {
    # Decoding 4 tokens: transform the numeric code into 8 bytes,
    # split them on 4x2 bytes, transform each pair into the index.
    code.raw = writeBin(code, num2raw.empty, endian = "little")
    dict.stems[c(raw2int(code.raw[1:2]) + 1,
                 raw2int(code.raw[3:4]) + 1,
                 raw2int(code.raw[5:6]) + 1,
                 raw2int(code.raw[7:8]) + 1)]
}

#
# Returns the decoding function for the specified n. The returned function may
# be used to decode n-grams, n from 1 to 4.
#
# @param n the parameter for choosing the decoding function, 1 to 4.
#
# @return the decoding function for n-grams.
#
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

#
# Encodes character vector of 1-grams.
#
# @param text the character vector where each element is 1-gram. The 1-grams
#       are expected to contain only stems available in the encoding dictionary.
# @param dict.hash the encoding dictionary.
#
# @return the integer vector with encoded 1-grams.
#
ngram.encode.vec.1 <- function(text, dict.hash) {
    message("Encoding 1-grams")
    pbsapply(text, function(x) tokens.encode.1(x, dict.hash),
             USE.NAMES = FALSE,
             simplify = TRUE)
}

#
# Encodes character vector of n-grams for n = 2, 3, 4.
#
# @param text the character vector where each element is n-gram with stems
#       separated by spaces. The n-grms are expected to contain only stems
#       available in the encoding dictionary.
# @param dict.hash the encoding dictionary.
#
# @return the integer vector (for 2-grams) or the numeric vector (for 3- and
#       4-grams) with encoded n-grams.
#
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

#
# Encodes character vector of n-garms, n = 1 to 4. 
#
# @param text the character vector where each element is n-gram with stems
#       separated by spaces. The n-grms are expected to contain only stems
#       available in the encoding dictionary.
# @param n the parameter n for n-grams.
# @param dict.hash the encoding dictionary.
#
# @return the integer vector (for 1- and 2-grams) or the numeric vector
#       (for 3- and 4-grams) with encoded n-grams.
#
ngram.encode.vec.n <- function(text, n, dict.hash) {
    if (n == 1) {
        ngram.encode.vec.1(text, dict.hash)
    } else {
        ngram.encode.vec.234(text, n, dict.hash)
    }
}

#
# Decodes provided code to n-grams, n = 1 to 4.
#
# @param code the integer vector (for 1- and 2-grams) or the numeric vector
#       (for 3- and 4-grams) with encoded n-grams.
# @param n the parameter n for n-grams.
# @param dict.stems the decoding dictionary.
#
# @return the character vector where each element is a decoded n-gram with
#       stems separated by space characters.
#
ngram.decode.vec.n <- function(code, n, dict.stems) {
    message("Decoding ", n, "-grams")
    
    tokens.decode <- tokens.decode.n(n)
    pbsapply(code, function(x) {paste(tokens.decode(x, dict.stems), collapse=" ")})
}

# ngram.dict.build.unique <- function(ngram.table, n, removeStopwords = FALSE) {
#     # Load top-frequency words required for stemming.
#     stems.top <- stems.top.cache(removeStopwords)$Terms
#     
#     # Preprocess n-gram for a dictionary.
#     # N-grams are already heavily pre-processed in tables. Only the last
#     # word is not yet lowercased and stemmed.
#     ngram.dict.build.unique.preprocess <- function(text) {
#         # Split on tokens.
#         tokens <- tokenizers::tokenize_regex(text, simplify = TRUE)
#         
#         # Lowercase and stem the last word.
#         tokens.length <- length(tokens)
#         tokens[tokens.length] <- preprocess.stem.word(tokens[tokens.length], stems.top)
#         
#         # Concatenate to n-gram again.
#         paste0(tokens, collapse = " ")
#     }
#     
#     # Keep just terms.
#     ngram.terms <- ngram.table$Terms
#     
#     # Stem words.
#     message("Stemming ", n, "-grams ",
#             ifelse(removeStopwords, "without", "with"), " stop words")
#     ngram.terms <- pbsapply(ngram.terms, ngram.dict.build.unique.preprocess,
#                             USE.NAMES = FALSE, simplify = TRUE)
#     
#     # Collect unique ngrams.
#     message("Collecting unique ", n, "-grams ",
#             ifelse(removeStopwords, "without", "with"), " stop words")
#     unique(ngram.terms)
# }

#
# Builds and returns a character vector with unique n-grams.
#
# @param n the parameter n for n-grams.
# @param removeStopwords TRUE if n-grams with removed stop words to be used.
#       Defaults to FALSE.
#
# @return the 
#
# ngram.prefix.unique.n.build <- function(n, removeStopwords = FALSE) {
#     # Load n-grams.
#     ngram.table <- table.ngram.enriched.cache(n, removeStopwords)
#     
#     ngram.dict.build.tbl(ngram.table, n, removeStopwords)
# }
# 
# ngram.code.build <- function(ngram.dict, n, removeStopwords = FALSE) {
#     # Load the dictionary.
#     dict.hash <- dict.hash.cache(removeStopwords)
#     
#     # Encode n-grams.
#     message("Encoding unique ", n, "-grams ",
#             ifelse(removeStopwords, "without", "with"), " stop words")
#     ngram.encode.vec.n(ngram.dict, n, dict.hash)
# }
# 
# ngram.dict.cache <- function(n, removeStopwords = FALSE) {
#     var.name <- paste0("dict.", n)
#     var.build <- function() {
#         ngram.dict.build(n, removeStopwords)
#     }
#     
#     get.var.cache(var.name, var.build, removeStopwords)
# }
# 
# ngram.code.cache <- function(n, removeStopwords = FALSE) {
#     var.name <- paste0("code.", n)
#     var.build <- function() {
#         ngram.dict <- ngram.dict.cache(n, removeStopwords)
#         ngram.code.build(ngram.dict, n, removeStopwords)
#     }
#     
#     get.var.cache(var.name, var.build, removeStopwords)
# }
