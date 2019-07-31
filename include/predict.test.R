#
# Test quality of prediction.
#

library(dplyr)
library(progress)
library(readr)
library(tokenizers)

source("include/ngram.predict.R")

#
# Load testing data.
#
# @param source the source of the testing data, shall be one of "blogs", "news",
#       "twitter".
# @param type the type of the testing data, shall be one of "training",
#       "testing", "validation".
#
# @return the character vector with the testing data.
#
predict.test.text.load <- function(source, type) {
    source.valid <- c("blogs", "news", "twitter")
    if (!(source %in% source.valid)) {
        stop("Invalid source: ", source)
    }
    
    type.valid <- c("training", "testing", "validation")
    if (!(type %in% type.valid)) {
        stop("Invalid type: ", type)
    }
    
    file.name <- file.path("cache", paste0("en_US.", source, ".", type, ".txt"))
    message("Loading data from ", file.name)
    
    read_lines(file.name)
}

#
# Builds samples to test prediction from the specified text.
# Returns data frame with the columns "Prefix" and "Suffix" where in the
# provided text the Suffix follows the Prefix. The Prefix contains at least
# prefix.min.tokens tokens, the suffix contains 1 token, where tokens are parts
# of the provided text separated by whitespace characters. In addition, any
# punctuation characters directly before or after the suffix are removed.
#
# @param text the character vector to build samples from.
# @param n the number of samples to build.
# @param prefix.min.tokens the minimum number of tokens in the prefix.
#
# @return the data frame with prepared samples.
#
predict.test.build.samples.intern <- function(text, n, prefix.min.tokens = 5) {
    text.length <- length(text)
    
    message("Tokenizing text")
    text.tokens <- tokenize_regex(text)
    
    message("Building samples")
    
    sample.prefix <- character(n)
    sample.suffix <- character(n)

    pb <- txtProgressBar(min = 0,
                         max = n,
                         initial = 0,
                         style = 3)
    
    i = 1
    while (i <= n) {
        # Select a sentence.
        sentence.index <- sample(1 : text.length, 1)
        sentence.tokens <- text.tokens[[sentence.index]]
        sentence.length <- length(sentence.tokens)
        
        if (sentence.length > prefix.min.tokens) {
            split.index <- sample((prefix.min.tokens + 1) : sentence.length, 1)
            sample.suffix.i <- tail(sentence.tokens, -(split.index - 1))

            sample.suffix.i <- predict.test.preprocess.suffix(sample.suffix.i)
            
            # If there is no suffix left, skip this sample.
            if (is.na(sample.suffix.i)) {
                next
            }

            sample.suffix[i] <- sample.suffix.i
            sample.prefix[i] <- paste0(head(sentence.tokens, split.index - 1),
                                       collapse = " ")
            
            i = i + 1
            if (i %% 100 == 0) {
                setTxtProgressBar(pb, i)
            }
        }
    }
    setTxtProgressBar(pb, i)
    close(pb)
    
    data.frame(Prefix = as.character(sample.prefix),
               Suffix = as.character(sample.suffix),
               stringsAsFactors = FALSE)
}

#
# Pre-processes the suffix of a test sample.
#
# We keep the prefix "as is", with all the troublesome characters,
# but we pre-process the suffix (the word to be predicted). For example, we
# do not attempt to predict the punctuation. If the next token is produced
# simply by splitting the text by space characters, punctuation characters such
# as a point or a comma are not separated from the previous word, and we got
# a token which consists of a word and the following point or a comma. This
# function removes such punctuation characters and other similar cases.
#
# @param suffix the suffix to pre-process.
#
# @return the pre-processed suffix, or NA if after all the pre-processing
#       the suffix is empty (for example, if the provided suffix was just a
#       punctuation character).
#
predict.test.preprocess.suffix <- function(suffix, verbose = FALSE) {
    suffix <- preprocess.addMissingSpace(suffix)
    if (verbose) {
        message("preprocess(1)")
        print(suffix)
    }

    # After adding space characters instead of the punctuation, our suffix
    # may have been transformed to multiple words. We split it on tokens again,
    # and continue simplifying.
    suffix.empty <- grepl("^\\s*$", suffix)
    suffix <- suffix[!suffix.empty]
    
    tokens <- unlist(tokenize_regex(suffix))

    # Remove tokens which contains only punctuation marks.
    tokens.punctuation.idx <- grepl("^[,.!?()\":;”…-]+$", tokens)
    tokens <- tokens[!tokens.punctuation.idx]

    # After all the transformations some tokens may become empty strings.
    # Remove them.
    tokens.empty <- grepl("^\\s*$", tokens)
    tokens <- tokens[!tokens.empty]

    ifelse(length(tokens) > 0, tokens[1], NA)
}

#
# Builds samples to test prediction from the specified text.
# See predict.test.build.samples.intern() for the detailed description.
#
# @param source the source of the testing data, shall be one of "blogs", "news",
#       "twitter".
# @param type the type of the testing data, shall be one of "training",
#       "testing", "validation".
# @param n the number of samples to build.
# @param prefix.min.tokens the minimum number of tokens in the prefix.
#
# @return the data frame with prepared samples.
#
predict.test.build.samples.source <- function(source, type, n,
                                              prefix.min.tokens = 5) {
    text <- predict.test.text.load(source, type)
    samples <- predict.test.build.samples.intern(text, n = n,
                                                 prefix.min.tokens = prefix.min.tokens) %>%
        mutate(Source = source) %>%
        mutate_if(is.factor, as.character)
    
    samples
}

#
# Builds samples to test prediction from the specified text, or from all texts
# See predict.test.build.samples.intern() for the detailed description.
#
# @param source the source of the testing data, shall be one of "blogs", "news",
#       "twitter", or "all".
# @param type the type of the testing data, shall be one of "training",
#       "testing", "validation".
# @param n the number of samples to build.
# @param prefix.min.tokens the minimum number of tokens in the prefix.
#
# @return the data frame with prepared samples.
#
predict.test.build.samples <- function(source, type, n, prefix.min.tokens = 5) {
    if (source == "all") {
        # R do not have out-of-the-box function to split an integer on
        # approximately equal parts, so we have to use a simple ad-hoc solution
        # for just 3 groups (blogs, news, twitter).
        n.blogs <- floor(n / 3)
        n.news <- floor(n / 3)
        n.twitter <- n - n.blogs - n.news
        
        # Get the required number of samples.
        samples.blogs <- predict.test.build.samples.source("blogs", type, n.blogs,
                                                           prefix.min.tokens = prefix.min.tokens)
        samples.news <- predict.test.build.samples.source("news", type, n.news,
                                                           prefix.min.tokens = prefix.min.tokens)
        samples.twitter <- predict.test.build.samples.source("twitter", type, n.twitter,
                                                           prefix.min.tokens = prefix.min.tokens)
        samples <- rbind(samples.blogs, samples.news, samples.twitter)
        
        # Shuffle the samples.
        sample_n(samples, nrow(samples))
    } else {
        predict.test.build.samples.source(source, type, n,
                                          prefix.min.tokens = prefix.min.tokens)
    }
}

#
# Tests the given prediction algorithm on the specified samples.
# The samples must be a data frame with the columns "Prefix" and "Suffix".
# The algorithm must be a function which accepts a single character parameter - 
# the prefix, and returns the table with predicted suffixes.
#
# Returns a data frame created from samples by appending columns "Predicted.1"
# to "Predicted.10", "Score.1" to "Score.10" and "Match.Index".
#
# The columns "Predicted.N" and "Score.N" contain predicted next words and their
# score as returned by the algorithm. The column "Match.Index" contains the
# index of the correct prediction, or NA if none of predictions returned by the
# algorithm matches the prefix of the sample.
#
# @param samples the data frame with samples.
# @param algo the algorithm to test.
#
# @return the data frame with results.
#
predict.test.algo <- function(samples, algo) {
    samples.length <- nrow(samples)
    
    # Prepare storage for top 10 predicted candidates and their probabilities.
    predicted <- data.frame(Predicted.1 = character(samples.length),
                            Score.1 = numeric(samples.length),
                            Predicted.2 = character(samples.length),
                            Score.2 = numeric(samples.length),
                            Predicted.3 = character(samples.length),
                            Score.3 = numeric(samples.length),
                            Predicted.4 = character(samples.length),
                            Score.4 = numeric(samples.length),
                            Predicted.5 = character(samples.length),
                            Score.5 = numeric(samples.length),
                            Predicted.6 = character(samples.length),
                            Score.6 = numeric(samples.length),
                            Predicted.7 = character(samples.length),
                            Score.7 = numeric(samples.length),
                            Predicted.8 = character(samples.length),
                            Score.8 = numeric(samples.length),
                            Predicted.9 = character(samples.length),
                            Score.9 = numeric(samples.length),
                            Predicted.10 = character(samples.length),
                            Score.10 = numeric(samples.length),
                            Match.Index = as.integer(rep(NA, samples.length)),
                            stringsAsFactors = FALSE)

    message("Running prediction algorithm on ", samples.length, " samples")
    
    pb <- txtProgressBar(min = 0,
                         max = samples.length,
                         initial = 0,
                         style = 3)
    
    for (i in 1 : samples.length) {
        sample.prefix <- samples$Prefix[i]
        sample.predicted <- algo(sample.prefix)

        for (j in 1 : 10) {
            if (nrow(sample.predicted) >= j) {
                suffix.col <- paste0("Predicted.", j)
                score.col <- paste0("Score.", j)
                
                sample.predicted.suffix <- sample.predicted[j, "Suffix"]
                sample.predicted.score <- sample.predicted[j, "Score"]

                predicted[i, suffix.col] <- sample.predicted.suffix
                predicted[i, score.col] <- sample.predicted.score
                
                if (samples$Suffix[i] == sample.predicted.suffix) {
                    predicted[i, "Match.Index"] <- j
                }
            }
        }
        
        if (i %% 10 == 0) {
            setTxtProgressBar(pb, i)
        }
    }
    
    setTxtProgressBar(pb, samples.length)
    close(pb)

    cbind(samples, predicted)
}

#
# Tests the Stupid Backoff algorithm.
#
# @param source the source of the testing data, shall be one of "blogs", "news",
#       "twitter", or "all".
# @param type the type of the testing data, shall be one of "training",
#       "testing", "validation".
# @param n.samples the number of test samples.
# @param threshold the minimum number of n-gram occurencies to use for
#       prediction. Defaults to 5.
# @param removeStopwords TRUE to use n-grams with removed stop words.
#       Defaults to FALSE.
#
# @return the data frame with results.
#
predict.test.sb.build <- function(source, type, n.samples = 100,
                            threshold = 5, removeStopwords = FALSE) {
    samples <- predict.test.build.samples(source, type, n.samples)

    algo <- function(x) sb.predict.text(x, n = 10, threshold = threshold,
                                        removeStopwords = removeStopwords)
    
    predict.test.algo(samples, algo)
}

#
# Caches results of testing the Stupid Backoff algorithm.
#
# @param source the source of the testing data, shall be one of "blogs", "news",
#       "twitter", or "all".
# @param type the type of the testing data, shall be one of "training",
#       "testing", "validation".
# @param n.samples the number of test samples.
# @param threshold the minimum number of n-gram occurencies to use for
#       prediction. Defaults to 5.
# @param removeStopwords TRUE to use n-grams with removed stop words.
#       Defaults to FALSE.
#
# @return the data frame with results.
#
predict.test.sb.cache <- function(source, type, n.samples = 100,
                                  threshold = 5, removeStopwords = FALSE) {
    
    threshold.collapsed <- paste0(threshold, collapse = "")
    var.name <- paste0("predict.test.sb.", source, ".", type, ".", n.samples,
                       ".thr-", threshold.collapsed)
    var.build <- function() predict.test.sb.build(source, type, n.samples,
                                     threshold, removeStopwords)
    
    get.var.cache(var.name, var.build, removeStopwords)
}
