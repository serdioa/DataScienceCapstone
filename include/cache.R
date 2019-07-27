#
# Common functions for caching data when pre-processing the text corpus,
# bulding n-grams and the prediction algorithm.
#
# Many steps in the data cleanup, pre-processing, building n-grams and
# fine-tuning the prediction algorithm require processing of very large data
# and thus are very slow (some of the steps may take hours). Caching allows to
# execute each step just once and later re-use results.
#

library("rlang")

#
# Returns the cache environment.
#
# @param removeStopwords determines which cache environment to return: the one
#       where we process corpora after removing stop words (TRUE), or the one
#       where we keep stop words (FALSE). Defaults to FALSE.
#
# @return the R environment used as in-memory cache.
#
get.cache <- function (removeStopwords = FALSE) {
    var.name <- if (removeStopwords) "cache.nosw" else "cache.sw"
    if (exists(var.name)) {
        var <- get(var.name, inherits = TRUE)
    } else {
        var <- env()
        assign(var.name, var, inherits = TRUE)
    }
}

#
# Return name of the directory with cached files with or without stop words,
# and creates that directory if it does not exists.
#
# @param removeStopwords TRUE to return the cache directory for files with
#       removed stop words. Defaults to FALSE.
#
# @return the name of the directory with cached files.
#
cache.dir <- function(removeStopwords = FALSE) {
    if (removeStopwords) {
        cache.dir.name <- "cache.without-stop-words"
    } else {
        cache.dir.name <- "cache.with-stop-words"
    }
    
    if (!dir.exists(cache.dir.name)) {
        dir.create(cache.dir.name)
    }
    
    cache.dir.name
}

#
# Get a named variable from the cache, or build the variable if the value is not
# cached yet. This function supports 2-level caching: in memory and on
# persistent storage.
#
# @param var.name the name of the variable.
# @param var.build the function which build the value of the variable.
#       The function should not expect any parameters.
# @param removeStopwords TRUE if the variable should be cached in an environment
#       where stop words are removed. Defaults to FALSE.
# @param var.persist TRUE if the variable value should be persisted on the disk.
#       Defaults to TRUE.
#
# @return the value of the requested variable.
#
get.var.cache <- function(var.name, var.build, removeStopwords = FALSE, var.persist = TRUE) {
    cache <- get.cache(removeStopwords)
    if (exists(var.name, where = cache)) {
        get(var.name, envir = cache)
    } else {
        file.name <- paste0(cache.dir(removeStopwords), "/", var.name, ".RDS")
        if (var.persist & file.exists(file.name)) {
            message("Loading ", var.name, " from ", file.name)
            var <- readRDS(file.name)
            assign(var.name, var, envir = cache)
            var
        } else {
            message("Building ", var.name)
            var <- var.build()
            assign(var.name, var, envir = cache)
            
            if (var.persist) {
                message("Saving ", var.name, " to ", file.name)
                saveRDS(var, file.name)
            }
            var
        }
    }
}

#
# Loads the text corpus of the specified type from the specified source.
#
# The type must be one of "blogs", "news" or "twitter". The source must be
# one of "training", "testing" or "validation".
#
# @param source the source of the corpus.
# @param type the type of the corpus.
#
# @return the character vector with the required text corpus.
#
text.original.cache <- function(source, type) {
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
# Loads the pre-processed text corpus from the specified source. The source must
# be one of "blogs", "news" or "twitter".
#
# @param source the source of the corpus.
# @param removeStopwords TRUE to load the pre-processed corpus with removed
#       stop words. Defaults to FALSE.
#
# @return the character vector with pre-processed text corpus.
#
text.preprocessed.load <- function(source, removeStopwords = FALSE) {
    source.valid <- c("blogs", "news", "twitter")
    if (!(source %in% source.valid)) {
        stop("Invalid source: ", source)
    }

    file.name <- file.path(cache.dir(removeStopwords),
                           paste0(source, ".text.preprocessed.txt"))
    message("Loading pre-processed ", source, " corpus")
    
    read_lines(file.name)
}

#
# Returns cached pre-processed text corpus from the specified source. The source
# must be one of "blogs", "news", "twitter" or "all". If the source is "all",
# returns concatencated pre-processed text corpora from all 3 base sources.
#
# @param source the source of the corpus.
# @param removeStopwords TRUE to load the pre-processed corpus with removed
#       stop words. Defaults to FALSE.
#
# @return the character vector with pre-processed text corpus.
#
text.preprocessed.cache <- function(source, removeStopwords = FALSE) {
    source.valid <- c("all", "blogs", "news", "twitter")
    if (!(source %in% source.valid)) {
        stop("Invalid source: ", source)
    }
    
    if (source == "all") {
        blogs.text <- text.preprocessed.cache("blogs", removeStopwords)
        news.text <- text.preprocessed.cache("news", removeStopwords)
        twitter.text <- text.preprocessed.cache("twitter", removeStopwords)
        
        c(blogs.text, news.text, twitter.text)
    } else {
        var.name <- paste0(source, ".text.preprocessed")
        
        cache <- get.cache(removeStopwords)
        if (exists(var.name, where = cache)) {
            get(var.name, envir = cache)
        } else {
            if (source == "all") {
                text.blogs <- text.preprocessed.load("blogs", removeStopwords)
                text.news <- text.preprocessed.load("news", removeStopwords)
                text.twitter <- text.preprocessed.load("twitter", removeStopwords)
                text <- c(text.blogs, text.news, text.twitter)
            } else {
                text <- text.preprocessed.load(source, removeStopwords)
            }
            
            assign(var.name, var, envir = cache)
            var
        }
    }
}
