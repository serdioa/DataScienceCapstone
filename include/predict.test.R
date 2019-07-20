#
# Test quality of prediction.
#

library(dplyr)
library(pbapply)
library(readr)
library(tokenizers)

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
predict.test.build.samples <- function(text, prefix.min.tokens = 5) {
    message("Tokenizing text")
    text.tokens <- tokenize_regex(text)
    
    text.samples <- data.frame(Prefix = character(),
                               Suffix = character())
    
    message("Building samples")
    pblapply(text.tokens, function(x) {
        x.length <- length(x)
        if (x.length > prefix.min.tokens) {
            text.samples.count <- x.length - prefix.min.tokens
            text.sample.prefix.tokens <- sapply(prefix.min.tokens:(x.length - 1),
                                                function(n) head(x, n))
            text.samples.prefix = sapply(text.sample.prefix.tokens,
                                         function(x) paste0(x, collapse = " "),
                                         simplify = TRUE)
            text.samples.suffix = x[(prefix.min.tokens + 1):x.length]
            text.samples.chunk <- data.frame(Prefix = as.character(text.samples.prefix),
                                             Suffix = as.character(text.samples.suffix))
            text.samples <<- rbind(text.samples, text.samples.chunk)
        }
    })
    
    text.samples
}

predict.test.cache.samples <- function(source, type, prefix.min.tokens = 5) {
    samples.file.name <- file.path("cache", paste0("samples.", source, ".", type, ".RDS"))
    if (file.exists(samples.file.name)) {
        readRDS(samples.file.name)
    } else {
        text <- predict.test.text.load(source, type)
        samples <- predict.test.build.samples(text, prefix.min.tokens = prefix.min.tokens)
        saveRDS(samples, samples.file.name)
        
        samples
    }
}

#
# Choose the specified number of samples. The provided text samples expected
# to be a data frame.
#
predict.test.select.samples <- function(text.samples, n) {
    message("Selecting samples")
    
    text.samples.length <- nrow(text.samples)
    text.samples.idx <- sample(1:text.samples.length, n)
    text.samples.selected <- text.samples[text.samples.idx, ]
    rownames(text.samples.selected) <- NULL
    
    text.samples.selected
}

#
# Creates the specified number of samples from the given data source.
# The data source is specified in the same way as in the function
# predict.test.text.load().
# The samples are enriched by an additional column "source" populated by the
# name of the source.
#
predict.test.file.build.samples <- function(source, type, n, prefix.min.tokens = 5) {
    text <- predict.test.text.load(source, type)
    samples <- predict.test.build.samples(text, prefix.min.tokens = prefix.min.tokens)
    predict.test.select.samples(samples, n) %>% mutate(Source = source)
}


