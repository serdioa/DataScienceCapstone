#
# Executed when the appliation starts.
#
library(hashmap)
library(readr)

#
# Load standard replacement tokens.
#
replacements.tokens <- readr::read_csv(file.path("data", "replacements.txt"),
                                       col_names = c("token", "replacement"),
                                       col_types = list(col_character(), col_character()))
# Extract tokens to a separate variable to be able to use fast match.
replacements.tokens.token <- replacements.tokens$token

#
# Load stems used in n-grams.
#
stems.freq.top <- readRDS(file.path("data", "stems.freq.top.RDS"))
dict.stems <- c("UNK", stems.freq.top)
dict.hash <- hashmap(keys = dict.stems, values = 0 : (256*256-1))


#
# Load optimized n-grams.
#
ngram.1 <- readRDS(file.path("data", "ngram.1.RDS"))
ngram.2 <- readRDS(file.path("data", "ngram.2.RDS"))
ngram.3 <- readRDS(file.path("data", "ngram.3.RDS"))
ngram.4 <- readRDS(file.path("data", "ngram.4.RDS"))
ngram.5 <- readRDS(file.path("data", "ngram.5.RDS"))

# Returns n-gram table for the specified n.
# 
# @param n - the n for which to return the n-grams table.
ngram.n <- function(n) {
    if (n == 1) {
        ngram.1
    } else if (n == 2) {
        ngram.2
    } else if (n == 3) {
        ngram.3
    } else if (n == 4) {
        ngram.4
    } else if (n == 5) {
        ngram.5
    }
}
