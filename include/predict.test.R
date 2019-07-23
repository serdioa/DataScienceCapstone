#
# Test quality of prediction.
#

library(dplyr)
library(progress)
library(readr)
library(tokenizers)

source("include/predict.sb.R")

#
# Load testing data.
# The source could be "blogs", "news", "twitter".
# The type could be "testing" or "validation".
#
predict.test.text.load <- function(source, type) {
    source.valid <- c("blogs", "news", "twitter")
    if (!(source %in% source.valid)) {
        stop("Invalid source: ", source)
    }
    
    type.valid <- c("testing", "validation")
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
predict.test.build.samples.intern <- function(text, n, prefix.min.tokens = 5,
                                              preprocess.suffix = FALSE) {
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

            sample.suffix.i <- predict.test.preprocess.suffix(sample.suffix.i,
                                                              heavy = preprocess.suffix)
            
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
               Suffix = as.character(sample.suffix))
}

# We keep the prefix "as is", with all the troublesome characters,
# but we may pre-process the suffix (the word to be predicted)
# if requested, because we even do not attempt to predict some
# tokens. Precision of prediction with and without pre-processing
# is useful: the first shows how good our algorithm is when running
# on a real-life text, the second shows how good our algorithm is
# in performing on words which it is expected to predict at all.
#
# If the algorithm demonstrates bad performance on pre-processed suffixes,
# that is on words it was programmed to predict, than the algorithm is bad
# in what it was intended to do. On the other hand, if the algorithm is good
# on pre-processed suffixes, but bad on a real-life text, that means that our
# assumptions on what the algorithm should attempt to predict are wrong.
#
# We distinguish between a mandatory pre-processing of the suffix, and the
# heavy pre-processing. The mandatory pre-processing is applied always, and
# it prevents down-grading the algoritihm if, for example, it does not predict
# punctuation marks.
predict.test.preprocess.suffix <- function(suffix, heavy = FALSE, verbose = FALSE) {
    if (heavy) {
        suffix <- preprocess.removeUrl(suffix)
        suffix <- preprocess.removeEmail(suffix)
        suffix <- preprocess.removeTagsAndHandles(suffix)
        suffix <- preprocess.removeUnderscores(suffix)
    }
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
    
    if (heavy) {
        tokens <- unlist(preprocess.tokenize(suffix))
    } else {
        tokens <- unlist(tokenize_regex(suffix))
    }
    if (verbose) {
        message("preprocess(2)")
        print(tokens)
    }
    
    if (heavy) {
        tokens <- preprocess.replaceWords(tokens)
        tokens <- preprocess.removeNonEnglish(tokens)
    }
    
    # Remove tokens which contains only punctuation marks.
    tokens.punctuation.idx <- grepl("^[,.!?()\":;”…-]+$", tokens)
    tokens <- tokens[!tokens.punctuation.idx]

    # After all the transformations some tokens may become empty strings.
    # Remove them.
    tokens.empty <- grepl("^\\s*$", tokens)
    tokens <- tokens[!tokens.empty]
    if (verbose) {
        message("preprocess(3)")
        print(tokens)
    }
    
    ifelse(length(tokens) > 0, tokens[1], NA)
}

# Builds samples to test prediction from the specified text.
predict.test.build.samples.source <- function(source, type, n,
                                              prefix.min.tokens = 5,
                                              preprocess.suffix = FALSE) {
        text <- predict.test.text.load(source, type)
        samples <- predict.test.build.samples.intern(text, n = n,
                                                     prefix.min.tokens = prefix.min.tokens,
                                                     preprocess.suffix = preprocess.suffix) %>%
            mutate(Source = source) %>%
            mutate_if(is.factor, as.character)
        
        samples
    }

# Builds samples to test prediction from the specified text, or from all texts
# if the source = "all".
predict.test.build.samples <- function(source, type, n, prefix.min.tokens = 5,
                                       preprocess.suffix = FALSE) {
    if (source == "all") {
        # R do not have out-of-the-box function to split an integer on
        # approximately equal parts, so we have to use a simple ad-hoc solution
        # for just 3 groups (blogs, news, twitter).
        n.blogs <- floor(n / 3)
        n.news <- floor(n / 3)
        n.twitter <- n - n.blogs - n.news
        
        # Get the required number of samples.
        samples.blogs <- predict.test.build.samples.source("blogs", type, n.blogs,
                                                           prefix.min.tokens = prefix.min.tokens,
                                                           preprocess.suffix = preprocess.suffix)
        samples.news <- predict.test.build.samples.source("news", type, n.news,
                                                           prefix.min.tokens = prefix.min.tokens,
                                                           preprocess.suffix = preprocess.suffix)
        samples.twitter <- predict.test.build.samples.source("twitter", type, n.twitter,
                                                           prefix.min.tokens = prefix.min.tokens,
                                                           preprocess.suffix = preprocess.suffix)
        samples <- rbind(samples.blogs, samples.news, samples.twitter)
        
        # Shuffle the samples.
        sample_n(samples, nrow(samples))
    } else {
        predict.test.build.samples.source(source, type, n,
                                          prefix.min.tokens = prefix.min.tokens,
                                          preprocess.suffix = preprocess.suffix)
    }
}

predict.test.cache.samples <- function(source, type, n, prefix.min.tokens = 5,
                                       preprocess.suffix = FALSE) {
    samples.file.name <- file.path("cache",
                                   paste0("samples.", source, ".", type, ".",
                                          preprocess.suffix, ".RDS"))
    if (file.exists(samples.file.name)) {
        message("Loading samples (", source, ", ", type, ") from ", samples.file.name)
        readRDS(samples.file.name)
    } else {
        samples <- predict.test.build.samples(source, type, n, prefix.min.tokens = prefix.min.tokens,
                                              preprocess.suffix = preprocess.suffix)
        
        # message("Saving samples (", source, ", ", type, ") into ", samples.file.name)
        # saveRDS(samples, samples.file.name)
        
        samples
    }
}

predict.test.cache.samples.all <- function() {
    predict.test.cache.samples("blogs", "testing", 1000000)
    predict.test.cache.samples("news", "testing", 1000000)
    predict.test.cache.samples("twitter", "testing", 1000000)
    
    predict.test.cache.samples("blogs", "validation", 1000000)
    predict.test.cache.samples("news", "validation", 1000000)
    predict.test.cache.samples("twitter", "validation", 1000000)
}

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
                            Match.Index = as.integer(rep(NA, samples.length))) %>%
        mutate_if(is.factor, as.character)

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

predict.test.sb <- function(source, type, n.samples = 100,
                            preprocess.suffix = FALSE,
                            threshold = 5, removeStopwords = FALSE) {
    samples <- predict.test.cache.samples(source, type, n.samples,
                                          preprocess.suffix = preprocess.suffix)
    
    algo <- function(x) sb.predict.text(x, n = 10, threshold = threshold,
                                        removeStopwords = removeStopwords)
    
    predict.test.algo(samples, algo)
}

predict.test.sb.cache <- function(source, type, n.samples = 100,
                                  preprocess.suffix = FALSE,
                                  threshold = 5, removeStopwords = FALSE) {
    threshold.collapsed <- paste0(threshold, collapse = "")
    file.name <- file.path("cache", paste0("predicted.", source, ".", type, ".", n.samples,
                       ".prep-", preprocess.suffix, ".thr-", threshold.collapsed,
                       ".remsw-", removeStopwords, ".RDS"))
    if (file.exists(file.name)) {
        message("Loading predicted results from ", file.name)
        # readRDS(file.name)
    } else {
        message("Predicting results for ", file.name)
        predicted <- predict.test.sb(source, type, n.samples, preprocess.suffix,
                                     threshold, removeStopwords)
        message("Saving predicted results to ", file.name)
        saveRDS(predicted, file.name)
        #    predicted
    }
}
    