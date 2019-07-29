#
# Collect statistics about the n-grams and Stupid Backoff prediction algorithm.
#

library(dplyr)
library(ggplot2)
library(tools)

source("include/cache.R")
source("include/ngram.build.R")
source("include/ngram.optimize.R")

#
# Collect size (rows and bytes) of the specified data frame.
# Returns the data frame with columns "Rows" (contains the number of rows) and
# "Size" (contains the size in Mb).
#
# @param df the data frame to collect statistics for.
#
# @return the collected statistics on the specified data frame.
#
stat.ngram.build <- function(df) {
    df.rows = nrow(df)
    df.size = as.numeric(strsplit(format(object.size(df), units = "MiB"), "\\s+")[[1]][1])
    
    data.frame(Rows = df.rows,
               Size = df.size)
}

#
# Collect size (rows and bytes) of non-optimized frequency table for n-grams.
# Returns the data frame with columns "N" (contains the n), "Rows" (contains
# the number of rows) and "Size" (contains the size in Mb).
#
# @param n the parameter for n-grams.
# @param removeStopwords TRUE to collect statistics for n-grams with stop words
#       removed. Defaults to FALSE.
#
# @return collected statistics on the non-optimized frequency table.
#
stat.ngram.extended.n.build <- function(n, removeStopwords = FALSE) {
    # Load the data.
    ngram.extended <- ngram.extended.cache(n, removeStopwords)

    # Calculate the statistics.
    stat.ngram.build(ngram.extended) %>%
        mutate(N = n) %>%
        select(N, Rows, Size)
}

#
# Collect size (rows and bytes) of non-optimized frequency tables for all
# n-grams. Returns the data frame with columns "N" (contains the n), "Rows"
# (contains the number of rows) and "Size" (contains the size in Mb).
#
# @param removeStopwords TRUE to collect statistics for n-grams with stop words
#       removed. Defaults to FALSE.
#
# @return collected statistics on the non-optimized frequency tables.
#
stat.ngram.extended.all.build <- function(removeStopwords = FALSE) {
    rbind(stat.ngram.extended.n.build(1, removeStopwords),
          stat.ngram.extended.n.build(2, removeStopwords),
          stat.ngram.extended.n.build(3, removeStopwords),
          stat.ngram.extended.n.build(4, removeStopwords),
          stat.ngram.extended.n.build(5, removeStopwords))
}

#
# Returns cached size (rows and bytes) of non-optimized frequency tables for all
# n-grams.
#
# @param removeStopwords TRUE to collect statistics for n-grams with stop words
#       removed. Defaults to FALSE.
#
# @return collected statistics on the non-optimized frequency tables.
#
stat.ngram.extended.all.cache <- function(removeStopwords = FALSE) {
    var.name <- "stat.ngram.extended.all"
    var.build <- function() stat.ngram.extended.all.build(removeStopwords)
    
    get.var.cache(var.name, var.build, removeStopwords)
}

#
# Collect size (rows and bytes) of frequency table for n-grams with optimized
# prefixes.
#
# Returns the data frame with columns "N" (contains the n), "Rows" (contains
# the number of rows) and "Size" (contains the size in Mb).
#
# @param n the parameter for n-grams.
# @param removeStopwords TRUE to collect statistics for n-grams with stop words
#       removed. Defaults to FALSE.
#
# @return collected statistics on the frequency table with optimized prefixes.
#
stat.ngram.optimize.prefix.n.build <- function(n, removeStopwords = FALSE) {
    # Load the data.
    ngram.table <- ngram.optimize.prefix.cache(n, removeStopwords)
    
    # Calculate the statistics.
    stat.ngram.build(ngram.table) %>%
        mutate(N = n) %>%
        select(N, Rows, Size)
}

#
# Collect size (rows and bytes) of frequency tables with optimized prefixes for
# all n-grams.
#
# Returns the data frame with columns "N" (contains the n), "Rows" (contains the
# number of rows) and "Size" (contains the size in Mb).
#
# @param removeStopwords TRUE to collect statistics for n-grams with stop words
#       removed. Defaults to FALSE.
#
# @return collected statistics on frequency tables with optimized prefixes.
#
stat.ngram.optimize.prefix.all.build <- function(removeStopwords = FALSE) {
    rbind(stat.ngram.optimize.prefix.n.build(1, removeStopwords),
          stat.ngram.optimize.prefix.n.build(2, removeStopwords),
          stat.ngram.optimize.prefix.n.build(3, removeStopwords),
          stat.ngram.optimize.prefix.n.build(4, removeStopwords),
          stat.ngram.optimize.prefix.n.build(5, removeStopwords))
}

#
# Returns cached size (rows and bytes) of frequency tables with optimized
# prefixes for all n-grams.
#
# @param removeStopwords TRUE to collect statistics for n-grams with stop words
#       removed. Defaults to FALSE.
#
# @return collected statistics on frequency tables with optimized prefixes.
#
stat.ngram.optimize.prefix.all.cache <- function(removeStopwords = FALSE) {
    var.name <- "stat.ngram.opt.pref.all"
    var.build <- function() stat.ngram.optimize.prefix.all.build(removeStopwords)
    
    get.var.cache(var.name, var.build, removeStopwords)
}

#
# Collect size (rows and bytes) of frequency table for n-grams with optimized
# probabilities.
#
# Returns the data frame with columns "N" (contains the n), "Rows" (contains
# the number of rows) and "Size" (contains the size in Mb).
#
# @param n the parameter for n-grams.
# @param removeStopwords TRUE to collect statistics for n-grams with stop words
#       removed. Defaults to FALSE.
#
# @return collected statistics on the frequency table with optimized
#       probabilities.
#
stat.ngram.optimize.prob.n.build <- function(n, removeStopwords = FALSE) {
    # Load the data.
    ngram.table <- ngram.optimize.prob.cache(n, removeStopwords)
    
    # Calculate the statistics.
    stat.ngram.build(ngram.table) %>%
        mutate(N = n) %>%
        select(N, Rows, Size)
}

#
# Collect size (rows and bytes) of frequency tables with optimized probabilities
# for all n-grams.
#
# Returns the data frame with columns "N" (contains the n), "Rows" (contains the
# number of rows) and "Size" (contains the size in Mb).
#
# @param removeStopwords TRUE to collect statistics for n-grams with stop words
#       removed. Defaults to FALSE.
#
# @return collected statistics on frequency tables with optimized probabilities.
#
stat.ngram.optimize.prob.all.build <- function(removeStopwords = FALSE) {
    rbind(stat.ngram.optimize.prob.n.build(1, removeStopwords),
          stat.ngram.optimize.prob.n.build(2, removeStopwords),
          stat.ngram.optimize.prob.n.build(3, removeStopwords),
          stat.ngram.optimize.prob.n.build(4, removeStopwords),
          stat.ngram.optimize.prob.n.build(5, removeStopwords))
}

#
# Returns cached size (rows and bytes) of frequency tables with optimized
# probabilities for all n-grams.
#
# @param removeStopwords TRUE to collect statistics for n-grams with stop words
#       removed. Defaults to FALSE.
#
# @return collected statistics on frequency tables with optimized probabilities.
#
stat.ngram.optimize.prob.all.cache <- function(removeStopwords = FALSE) {
    var.name <- "stat.ngram.opt.prob.all"
    var.build <- function() stat.ngram.optimize.prob.all.build(removeStopwords)
    
    get.var.cache(var.name, var.build, removeStopwords)
}

#
# Collect size (rows and bytes) of optimized frequency table for n-grams
# with the specified filtering threshold (exclude rows with frequency equals
# or below the threshold).
#
stat.sb.table.opt.n <- function(n, threshold = 5, removeStopwords = FALSE) {
    table.orig <- table.optimize.sb.cache(n, threshold = threshold,
                                          removeStopwords = removeStopwords)
    
    table.orig.nrow <- nrow(table.orig)
    table.orig.size <- as.integer(object.size(table.orig))
            
    data.frame(N = n,
               Threshold = threshold,
               Rows = table.orig.nrow,
               Size = table.orig.size)
}

#
# Collect size (rows and bytes) of optimized frequency table for all n-grams
# with the specified filtering threshold (exclude rows with frequency equals
# or below the threshold).
#
stat.sb.table.opt <- function(threshold = 5, removeStopwords = FALSE) {
    rbind(stat.sb.table.opt.n(1, threshold = threshold, removeStopwords = removeStopwords),
          stat.sb.table.opt.n(2, threshold = threshold, removeStopwords = removeStopwords),
          stat.sb.table.opt.n(3, threshold = threshold, removeStopwords = removeStopwords),
          stat.sb.table.opt.n(4, threshold = threshold, removeStopwords = removeStopwords),
          stat.sb.table.opt.n(5, threshold = threshold, removeStopwords = removeStopwords))
}

#
# Cache size (rows and bytes) of optimized frequency table for all n-grams
# with the specified filtering threshold (exclude rows with frequency equals
# or below the threshold).
#
stat.sb.table.opt.cache <- function(threshold = 5, removeStopwords = FALSE) {
    var.name <- paste0("stat.sb.opt.", threshold)
    var.build <- function() stat.sb.table.opt(threshold, removeStopwords)
    
    get.var.cache(var.name, var.build, removeStopwords)
}

#
# Calculates size (rows and bytes) of optimized frequency table for all n-grams
# and all filtering thresholds.
#
stat.sb.table.opt.all <- function(removeStopwords = FALSE) {
    sb.opt.0 <- stat.sb.table.opt.cache(threshold = 0, removeStopwords = removeStopwords)
    sb.opt.1 <- stat.sb.table.opt.cache(threshold = 1, removeStopwords = removeStopwords)
    sb.opt.2 <- stat.sb.table.opt.cache(threshold = 2, removeStopwords = removeStopwords)
    sb.opt.3 <- stat.sb.table.opt.cache(threshold = 3, removeStopwords = removeStopwords)
    sb.opt.4 <- stat.sb.table.opt.cache(threshold = 4, removeStopwords = removeStopwords)
    sb.opt.5 <- stat.sb.table.opt.cache(threshold = 5, removeStopwords = removeStopwords)
    
    data.frame(N = sb.opt.0$N,
               Rows.0 = sb.opt.0$Rows,
               Size.0 = sb.opt.0$Size,
               Rows.1 = sb.opt.1$Rows,
               Size.1 = sb.opt.1$Size,
               Rows.2 = sb.opt.2$Rows,
               Size.2 = sb.opt.2$Size,
               Rows.3 = sb.opt.3$Rows,
               Size.3 = sb.opt.3$Size,
               Rows.4 = sb.opt.4$Rows,
               Size.4 = sb.opt.4$Size,
               Rows.5 = sb.opt.5$Rows,
               Size.5 = sb.opt.5$Size
    )
}

#
# Collects statistics of prediction.
#
stat.predict.sb.mono.collect.n <- function(source, type,
                                           preprocess.suffix = FALSE,
                                           threshold = 5, removeStopwords = FALSE) {
    predict.tbl <- predict.test.sb.cache(source, type, n.samples = 100000,
                                         preprocess.suffix = preprocess.suffix,
                                         threshold = threshold, removeStopwords = removeStopwords)
    
    # Split on 100 blocks x 1000 samples.
    # For each block calculate:
    # * Number of samples where the right word was top 1st predicted.
    # * Number of samples where the right word was in top 3 predicted.
    # * Number of samples where the right word was in top 5 predicted.
    stat.avg <- lapply(1:100, function(n) {
        row.min <- (n - 1) * 1000 + 1
        row.max <- n * 1000
        
        predict.tbl.n <- predict.tbl[row.min:row.max,]
        predict.tbl.n.length <- nrow(predict.tbl.n)
        
        stat.1.avg <- predict.tbl.n %>% filter(Match.Index <= 1) %>% nrow() / predict.tbl.n.length
        stat.3.avg <- predict.tbl.n %>% filter(Match.Index <= 3) %>% nrow() / predict.tbl.n.length
        stat.5.avg <- predict.tbl.n %>% filter(Match.Index <= 5) %>% nrow() / predict.tbl.n.length
        
        c(stat.1.avg, stat.3.avg, stat.5.avg)
    })
    
    # Extract number of samples for various levels of matches.
    stat.avg.1 <- sapply(stat.avg, "[[", 1)
    stat.avg.3 <- sapply(stat.avg, "[[", 2)
    stat.avg.5 <- sapply(stat.avg, "[[", 3)
    
    # Combine the data into a data frame.
    tbl.avg.1 <- data.frame(Source = rep(source, 100),
                            Type = rep(type, 100),
                            RemoveStopwords = rep(removeStopwords, 100),
                            Threshold = rep(threshold, 100),
                            Rank = rep(1, 100),
                            Match = stat.avg.1,
                            stringsAsFactors = FALSE)
    tbl.avg.3 <- data.frame(Source = rep(source, 100),
                            Type = rep(type, 100),
                            RemoveStopwords = rep(removeStopwords, 100),
                            Threshold = rep(threshold, 100),
                            Rank = rep(3, 100),
                            Match = stat.avg.3,
                            stringsAsFactors = FALSE)
    tbl.avg.5 <- data.frame(Source = rep(source, 100),
                            Type = rep(type, 100),
                            RemoveStopwords = rep(removeStopwords, 100),
                            Threshold = rep(threshold, 100),
                            Rank = rep(5, 100),
                            Match = stat.avg.5,
                            stringsAsFactors = FALSE)
    
    rbind(tbl.avg.1, tbl.avg.3, tbl.avg.5)
}

stat.predict.sb.mono.collect <- function(source, type,
                                         preprocess.suffix = FALSE,
                                         removeStopwords = FALSE) {
    stat.0 <- stat.predict.sb.mono.collect.n(source, type, preprocess.suffix = preprocess.suffix,
                                     threshold = 0, removeStopwords = removeStopwords)
    stat.1 <- stat.predict.sb.mono.collect.n(source, type, preprocess.suffix = preprocess.suffix,
                                     threshold = 1, removeStopwords = removeStopwords)
    stat.2 <- stat.predict.sb.mono.collect.n(source, type, preprocess.suffix = preprocess.suffix,
                                     threshold = 2, removeStopwords = removeStopwords)
    stat.3 <- stat.predict.sb.mono.collect.n(source, type, preprocess.suffix = preprocess.suffix,
                                     threshold = 3, removeStopwords = removeStopwords)
    stat.4 <- stat.predict.sb.mono.collect.n(source, type, preprocess.suffix = preprocess.suffix,
                                     threshold = 4, removeStopwords = removeStopwords)
    stat.5 <- stat.predict.sb.mono.collect.n(source, type, preprocess.suffix = preprocess.suffix,
                                     threshold = 5, removeStopwords = removeStopwords)
    
    rbind(stat.0, stat.1, stat.2, stat.3, stat.4, stat.5)
}

stat.predict.sb.mono.collect.all <- function() {
    blogs.sw <- stat.predict.sb.mono.collect("blogs", "testing", removeStopwords = FALSE)
    news.sw <- stat.predict.sb.mono.collect("news", "testing", removeStopwords = FALSE)
    twitter.sw <- stat.predict.sb.mono.collect("twitter", "testing", removeStopwords = FALSE)
    
    blogs.nosw <- stat.predict.sb.mono.collect("blogs", "testing", removeStopwords = TRUE)
    news.nosw <- stat.predict.sb.mono.collect("news", "testing", removeStopwords = TRUE)
    twitter.nosw <- stat.predict.sb.mono.collect("twitter", "testing", removeStopwords = TRUE)
    
    rbind(blogs.sw, news.sw, twitter.sw, blogs.nosw, news.nosw, twitter.nosw)
}

predict.test.sb.cache <- function(source, type, n.samples = 100000,
                                  preprocess.suffix = FALSE,
                                  threshold = 5, removeStopwords = FALSE) {
    file.name <- paste0("cache.save/cache/predicted.", source, ".", type, ".",
                        n.samples, ".prep-", preprocess.suffix,
                        ".thr-", threshold, ".remsw-", removeStopwords, ".RDS")
    
    readRDS(file.name)    
}

#
# Calculates precision of prediction (average and confidence interval) for
# samples with a monomodal threshold (the same threshold for all n-grams).
#
stat.predict.sb.mono.n <- function(source, type,
                                   preprocess.suffix = FALSE,
                                   threshold = 5, removeStopwords = FALSE) {
    predict.tbl <- predict.test.sb.cache(source, type, n.samples = 100000,
                                         preprocess.suffix = preprocess.suffix,
                                         threshold = threshold, removeStopwords = removeStopwords)
    
    # Split on 100 blocks x 1000 samples.
    # For each block calculate:
    # * Number of samples where the right word was top 1st predicted.
    # * Number of samples where the right word was in top 3 predicted.
    # * Number of samples where the right word was in top 5 predicted.
    stat.avg <- lapply(1:100, function(n) {
        row.min <- (n - 1) * 1000 + 1
        row.max <- n * 1000
        
        predict.tbl.n <- predict.tbl[row.min:row.max,]
        predict.tbl.n.length <- nrow(predict.tbl.n)

        stat.1.avg <- predict.tbl.n %>% filter(Match.Index <= 1) %>% nrow() / predict.tbl.n.length
        stat.3.avg <- predict.tbl.n %>% filter(Match.Index <= 3) %>% nrow() / predict.tbl.n.length
        stat.5.avg <- predict.tbl.n %>% filter(Match.Index <= 5) %>% nrow() / predict.tbl.n.length
        
        c(stat.1.avg, stat.3.avg, stat.5.avg)
    })
    
    # Extract number of samples for various levels of matches.
    stat.avg.1 <- sapply(stat.avg, "[[", 1)
    stat.avg.3 <- sapply(stat.avg, "[[", 2)
    stat.avg.5 <- sapply(stat.avg, "[[", 3)

    # Calculate an average and confidence interval.
    stat.avg.1.t <- t.test(stat.avg.1)
    stat.avg.3.t <- t.test(stat.avg.3)
    stat.avg.5.t <- t.test(stat.avg.5)
    
    # Combine into a data frame.
    data.frame(Source = rep(source, 3),
               Type = rep(type, 3),
               RemoveStopwords = rep(removeStopwords, 3),
               Threshold = rep(threshold, 3),
               Rank = c(1, 3, 5),
               Mean = c(stat.avg.1.t$estimate, stat.avg.3.t$estimate, stat.avg.5.t$estimate),
               ConfIntLow = c(stat.avg.1.t$conf[1], stat.avg.3.t$conf[1], stat.avg.5.t$conf[1]),
               ConfIntHigh = c(stat.avg.1.t$conf[2], stat.avg.3.t$conf[2], stat.avg.5.t$conf[2]),
               stringsAsFactors = FALSE)
}

#
# Calculates precision of prediction (average and confidence interval) for
# samples with all monomodal thresholds (the same threshold for all n-grams).
#
stat.predict.sb.mono <- function(source, type,
                                 preprocess.suffix = FALSE,
                                 removeStopwords = FALSE) {
    stat.0 <- stat.predict.sb.mono.n(source, type, preprocess.suffix = preprocess.suffix,
                                     threshold = 0, removeStopwords = removeStopwords)
    stat.1 <- stat.predict.sb.mono.n(source, type, preprocess.suffix = preprocess.suffix,
                                     threshold = 1, removeStopwords = removeStopwords)
    stat.2 <- stat.predict.sb.mono.n(source, type, preprocess.suffix = preprocess.suffix,
                                     threshold = 2, removeStopwords = removeStopwords)
    stat.3 <- stat.predict.sb.mono.n(source, type, preprocess.suffix = preprocess.suffix,
                                     threshold = 3, removeStopwords = removeStopwords)
    stat.4 <- stat.predict.sb.mono.n(source, type, preprocess.suffix = preprocess.suffix,
                                     threshold = 4, removeStopwords = removeStopwords)
    stat.5 <- stat.predict.sb.mono.n(source, type, preprocess.suffix = preprocess.suffix,
                                     threshold = 5, removeStopwords = removeStopwords)
    
    rbind(stat.0, stat.1, stat.2, stat.3, stat.4, stat.5)
}

#
# Calculates precision of prediction (average and confidence interval) for
# all monomodal samples, including with and without stopwords.
#
stat.predict.sb.mono.all <- function() {
    blogs.sw <- stat.predict.sb.mono("blogs", "testing", removeStopwords = FALSE)
    news.sw <- stat.predict.sb.mono("news", "testing", removeStopwords = FALSE)
    twitter.sw <- stat.predict.sb.mono("twitter", "testing", removeStopwords = FALSE)
    
    blogs.nosw <- stat.predict.sb.mono("blogs", "testing", removeStopwords = TRUE)
    news.nosw <- stat.predict.sb.mono("news", "testing", removeStopwords = TRUE)
    twitter.nosw <- stat.predict.sb.mono("twitter", "testing", removeStopwords = TRUE)
    
    rbind(blogs.sw, news.sw, twitter.sw, blogs.nosw, news.nosw, twitter.nosw)
}

#
# Collect statistics of validation.
#
stat.predict.sb.validation.all <- function() {
    validation.blogs <- stat.predict.sb.mono.collect.n("blogs", "validation")
    validation.news <- stat.predict.sb.mono.collect.n("news", "validation")
    validation.twitter <- stat.predict.sb.mono.collect.n("twitter", "validation")
    validation.all <- stat.predict.sb.mono.collect.n("all", "validation")

    rbind(validation.blogs, validation.news, validation.twitter, validation.all)
}

stat.predict.sb.validation.all.chart <- function() {
    if (!exists("stat.validation")) {
        stat.validation <- stat.predict.sb.validation.all()
    }
    
    ggplot(data = stat.validation, aes(x = as.factor(Rank), y = Match * 100, fill = Source)) +
        geom_boxplot() +
        scale_fill_discrete(name = "Source",
                            labels = c("Blogs", "News", "Twitter", "Aggregated")) +
        labs(title = "Prediction precision") +
        labs(x = "Top N predictions") +
        labs(y = "Correct prediction in top N, %") + 
        theme_bw(base_size = 14)
}

stat.predict.sb.validation.all.violin.chart <- function() {
    if (!exists("stat.validation")) {
        stat.validation <- stat.predict.sb.validation.all()
    }
    
    ggplot(data = stat.validation, aes(x = as.factor(Rank), y = Match * 100, fill = Source)) +
        geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
        scale_fill_discrete(name = "Source",
                            labels = c("Blogs", "News", "Twitter", "Aggregated")) +
        labs(title = "Prediction precision") +
        labs(x = "Top N predictions") +
        labs(y = "Correct prediction in top N, %") + 
        theme_bw(base_size = 14)
}

stat.predict.sb.testing.all.chart <- function() {
    if (!exists("stat.testing")) {
        stat.testing <- stat.predict.sb.mono.collect.all()
    }
    stat.testing <- stat.testing %>%
        filter(RemoveStopwords == FALSE) %>%
        mutate(Group = paste0(toTitleCase(Source), " (rank ", Rank, ")"))

    ggplot(data = stat.testing, aes(x = as.factor(Threshold), y = Match * 100, fill = Group)) +
        geom_boxplot() +
#        scale_fill_discrete(name = "Source",
#                            labels = c("Blogs", "News", "Twitter", "Aggregated")) +
        labs(title = "Prediction precision") +
        labs(x = "Top N predictions") +
        labs(y = "Correct prediction in top N, %") + 
        theme_bw(base_size = 14)
}

stat.predict.sb.testing.all.box.chart <- function() {
    if (!exists("stat.testing")) {
        stat.testing <- stat.predict.sb.mono.collect.all()
    }

    stat.testing.1 <- stat.testing %>%
        filter(RemoveStopwords == FALSE) %>%
        filter(Rank == 1)
    stat.testing.3 <- stat.testing %>%
        filter(RemoveStopwords == FALSE) %>%
        filter(Rank == 3)
    stat.testing.5 <- stat.testing %>%
        filter(RemoveStopwords == FALSE) %>%
        filter(Rank == 5)
    
    ggplot(data = stat.testing.1, aes(x = as.factor(Threshold), y = Match * 100, fill = Source)) +
        geom_boxplot() +
        geom_boxplot(data = stat.testing.3, aes(x = as.factor(Threshold), y = Match * 100, fill = Source)) +
        geom_boxplot(data = stat.testing.5, aes(x = as.factor(Threshold), y = Match * 100, fill = Source)) +
        #        scale_fill_discrete(name = "Source",
        #                            labels = c("Blogs", "News", "Twitter", "Aggregated")) +
        labs(title = "Prediction precision") +
        labs(x = "Top N predictions") +
        labs(y = "Correct prediction in top N, %") + 
        theme_bw(base_size = 14)
}

stat.predict.sb.testing.all.line.chart <- function() {
    if (!exists("stat.testing")) {
        stat.testing <- stat.predict.sb.mono.collect.all()
    }
    
    stat.testing.mean <- stat.testing %>%
        filter(RemoveStopwords == FALSE) %>%
        group_by(Source, Threshold, Rank) %>%
        summarise(Match.Mean = mean(Match)) %>%
        ungroup()

    stat.testing.1 <- stat.testing.mean %>%
        filter(Rank == 1)
    stat.testing.3 <- stat.testing.mean %>%
        filter(Rank == 3)
    stat.testing.5 <- stat.testing.mean %>%
        filter(Rank == 5)
    
    ggplot(data = stat.testing.1, aes(x = Threshold, y = Match.Mean * 100, color = Source)) +
        geom_line(size = 1) +
        geom_line(data = stat.testing.3, aes(x = Threshold, y = Match.Mean * 100, color = Source), size = 1) +
        geom_line(data = stat.testing.5, aes(x = Threshold, y = Match.Mean * 100, color = Source), size = 1) +
        scale_color_discrete(name = "Source",
                        labels = c("Blogs", "News", "Twitter")) +
        annotate(geom = "text", x = 4.5, y = 17.1, label = "Correct prediction") +
        annotate(geom = "text", x = 4.5, y = 26.6, label = "Top 3") +
        annotate(geom = "text", x = 4.5, y = 31.6, label = "Top 5") +
        labs(title = "Prediction precision") +
        labs(x = "Minimum frequency threshold of n-grams table") +
        labs(y = "Correct prediction in top N, %") + 
        theme_bw(base_size = 14)
}



