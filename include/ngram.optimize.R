#
# Optimize n-gram tables.
#
library(fastmatch)

library(data.table)

source("include/cache.R")
source("include/ngram.build.R")
source("include/ngram.encode.R")

#
# Returns cached unique prefixes of n-grams.
#
# @param n the parameter for n-grams.
# @param removeStopwords TRUE to return cached unique prefixes of n-grams
#       without stop words. Defaults to FALSE.
#
# @return the character vector with unique prefixes of n-grams.
#
ngram.optimize.prefix.unique.cache <- function(n, removeStopwords = FALSE) {
    var.name = paste0("ngram.", n, ".prefix.unique")
    var.build <- function() {
        ngram.extended <- ngram.extended.cache(n, removeStopwords)
        
        message("Building unique prefixes for ", n, "-grams ",
                ifelse(removeStopwords, "without", "with"), " stop words")
        unique(ngram.extended$Prefix)
    }
    
    get.var.cache(var.name, var.build, removeStopwords)
}

#
# Returns cached code of unique prefixes of n-grams. The returned codes
# corresponds to n-grams returned by the function
# ngram.optimize.prefix.unique.n.cache().
#
# @param n the parameter for n-grams.
# @param removeStopwords TRUE to return cached codes of prefixes of n-grams
#       without stop words. Defaults to FALSE.
#
# @return the integer (for 2- and 3-grams) or numeric (for 4- or 5-grams)
#       code of prefixes of n-grams. Note the the prefix has 1 word less than
#       the n-gram itself.
#
ngram.optimize.prefix.code.cache <- function(n, removeStopwords = FALSE) {
    var.name = paste0("ngram.", n, ".prefix.code")
    var.build <- function() {
        ngram.prefix.unique <- ngram.optimize.prefix.unique.cache(n, removeStopwords)
        dict.hash <- dict.hash.cache(removeStopwords)
        ngram.encode.vec.n(ngram.prefix.unique, n - 1, dict.hash)
    }
    
    get.var.cache(var.name, var.build, removeStopwords)
}

#
# Builds an n-gram table where the column "Prefix" contains the code of the
# prefix.
#
# @param n the parameter for n-grams.
# @param removeStopwords TRUE to build a table for n-grams without stop words.
#       Defaults to FALSE.
#
# @return the table with n-grams where the column "Prefix" contains the code
#       of the prefix.
#
ngram.optimize.prefix.build <- function(n, removeStopwords = FALSE) {
    ngram.extended <- ngram.extended.cache(n, removeStopwords)
    
    if (n == 1) {
        # For 1-grams, just remove the prefix. It is NA for all 1-grams anyway.
        ngram.extended %>% select(Suffix, Prob)
    } else {
        # For n > 1 encode the prefix.
        
        ngram.prefix.unique <- ngram.optimize.prefix.unique.cache(n, removeStopwords)
        ngram.prefix.code <- ngram.optimize.prefix.code.cache(n, removeStopwords)
        
        message("Encoding prefixes for ", n, "-grams ",
                ifelse(removeStopwords, "without", "with"), " stop words")
        
        ngram.extended %>%
            mutate(Prefix = ngram.prefix.code[fmatch(Prefix, ngram.prefix.unique)])
    }
}

#
# Returns a cached n-gram table where the column "Prefix" contains the code of the
# prefix.
#
# @param n the parameter for n-grams.
# @param removeStopwords TRUE to build a table for n-grams without stop words.
#       Defaults to FALSE.
#
# @return the table with n-grams where the column "Prefix" contains the code
#       of the prefix.
#
ngram.optimize.prefix.cache <- function(n, removeStopwords = FALSE) {
    var.name <- paste0("ngram.", n, ".opt.prefix")
    var.build <- function() ngram.optimize.prefix.build(n, removeStopwords)
    
    get.var.cache(var.name, var.build, removeStopwords)
}

#
# Builds an n-gram table with optimized "Prob" column, where the probability
# is subject to an integer log transformation.
#
# @param n the parameter for n-grams.
# @param removeStopwords TRUE to build a table for n-grams without stop words.
#       Defaults to FALSE.
#
# @return the table with optimized probability column.
#
ngram.optimize.prob.build <- function(n, removeStopwords = FALSE) {
    ngram.extended <- ngram.optimize.prefix.cache(n, removeStopwords)
    
    ngram.extended %>%
        mutate(Prob = as.integer(log(Prob) * 1000000))
}

#
# Returns a cached n-gram table with optimized "Prob" column, where the
# probability is subject to an integer log transformation.
#
# @param n the parameter for n-grams.
# @param removeStopwords TRUE to build a table for n-grams without stop words.
#       Defaults to FALSE.
#
# @return the table with optimized probability column.
#
ngram.optimize.prob.cache <- function(n, removeStopwords = FALSE) {
    var.name <- paste0("ngram.", n, ".opt.prob")
    var.build <- function() ngram.optimize.prob.build(n, removeStopwords)
    
    get.var.cache(var.name, var.build, removeStopwords)
}

#
# Builds an n-gram table with optimized "Prefix" and "Prob" columns,
# represented as a data.table with "Prefix as a search key.
#
# @param n the parameter for n-grams.
# @param threshold the minimum number of n-gram occurencies to keep it.
# @param removeStopwords TRUE to build a table for n-grams without stop words.
#       Defaults to FALSE.
#
# @return the data.table with optimized "Prefix" and "Prob" columns,
#       represented as a data.table with "Prefix as a search key.
#
ngram.optimize.build <- function(n, threshold = 0, removeStopwords = FALSE) {
    ngram.extended.full <- ngram.extended.full.cache(n, removeStopwords)
    
    # Filter according to the threshold.
    ngram.opt <- ngram.extended.full %>%
        filter(Freq >= threshold)
    
    if (n == 1) {
        # For 1-grams, just remove the prefix. It is NA for all 1-grams anyway.
        ngram.opt <- ngram.opt %>%
            transmute(Suffix = Suffix,
                      Prob = as.integer(log(Prob) * 1000000))
    } else {
        # For n > 1 encode the prefix.
        ngram.prefix.unique <- ngram.optimize.prefix.unique.cache(n, removeStopwords)
        ngram.prefix.code <- ngram.optimize.prefix.code.cache(n, removeStopwords)
        
        message("Encoding prefixes for ", n, "-grams ",
                ifelse(removeStopwords, "without", "with"), " stop words")
        
        ngram.opt <- ngram.opt %>%
            transmute(Prefix = ngram.prefix.code[fmatch(Prefix, ngram.prefix.unique)],
                      Suffix = Suffix,
                      Prob = as.integer(log(Prob) * 1000000))
    }
    
    if (n == 1) {
        # Transform to data.fram if n == 1. 
        data.frame(ngram.opt)
    } else {
        # Transform to data.table if n > 1.
        data.table(ngram.opt, key = c("Prefix"))
    }
}

#
# Returns a cached n-gram table with optimized "Prefix" and "Prob" columns,
# represented as a data.table with "Prefix as a search key.
#
# @param n the parameter for n-grams.
# @param threshold the minimum number of n-gram occurencies to keep it.
# @param removeStopwords TRUE to build a table for n-grams without stop words.
#       Defaults to FALSE.
#
# @return the data.table with optimized "Prefix" and "Prob" columns,
#       represented as a data.table with "Prefix as a search key.
#
ngram.optimize.cache <- function(n, threshold = 0, removeStopwords = FALSE) {
    var.name <- paste0("ngram.", n, ".", threshold, ".opt")
    var.build <- function() ngram.optimize.build(n, threshold, removeStopwords)
    
    get.var.cache(var.name, var.build, removeStopwords)
}
