#
# Collect statistics about the n-grams and Stupid Backoff prediction algorithm.
#

library(dplyr)
library(ggplot2)
library(hms)
library(tools)

source("include/cache.R")
source("include/ngram.build.R")
source("include/ngram.optimize.R")
source("include/predict.test.R")

################################################################################
#
# Statistics on tables with n-grams.
#
################################################################################

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
# Collect size (rows and bytes) of an optimized probability table for n-grams
# with the specified threshold.
# 
# Returns the data frame with columns "N" (contains the n), "Threshold"
# (contains the threshold), "Rows" (contains the number of rows) and "Size"
# (contains the size in Mb).
#
# @param n the parameter for n-grams.
# @param threshold the threshold for the probability table. The table contains
#       only entries which appear at least this number of times.
# @param removeStopwords TRUE to collect statistics for n-grams with stop words
#       removed. Defaults to FALSE.
#
# @return collected statistics on the optimized probability table.
#
stat.ngram.optimize.n.build <- function(n, threshold, removeStopwords = FALSE) {
    # Load the data.
    ngram.table <- ngram.optimize.cache(n, threshold, removeStopwords)
    
    # Calculate the statistics.
    stat.ngram.build(ngram.table) %>%
        mutate(N = n, Threshold = threshold) %>%
        select(N, Threshold, Rows, Size)
}

#
# Collect size (rows and bytes) of an optimized probability table for all
# n-grams.
# 
# Returns the data frame with columns "N" (contains the n), "Threshold"
# (contains the threshold), "Rows" (contains the number of rows) and "Size"
# (contains the size in Mb).
#
# @param removeStopwords TRUE to collect statistics for n-grams with stop words
#       removed. Defaults to FALSE.
#
# @return collected statistics on the optimized probability table.
#
stat.ngram.optimize.all.build <- function(removeStopwords = FALSE) {
    stat <- data.frame(N = integer(),
                       Threshold = integer(),
                       Rows = integer(),
                       Size = numeric())
    for (n in 1:5) {
        for (threshold in 1:6) {
            stat <- rbind(stat, stat.ngram.optimize.n.build(n, threshold, removeStopwords))
        }
    }
    
    stat
}

#
# Returns cached size (rows and bytes) of an optimized probability table for all
# n-grams.
# 
# Returns the data frame with columns "N" (contains the n), "Threshold"
# (contains the threshold), "Rows" (contains the number of rows) and "Size"
# (contains the size in Mb).
#
# @param removeStopwords TRUE to collect statistics for n-grams with stop words
#       removed. Defaults to FALSE.
#
# @return collected statistics on the optimized probability table.
#
stat.ngram.optimize.all.cache <- function(removeStopwords = FALSE) {
    var.name <- "stat.ngram.opt.all"
    var.build <- function() stat.ngram.optimize.all.build(removeStopwords)
    
    get.var.cache(var.name, var.build, removeStopwords)
}

################################################################################
#
# Statistics on predictions.
#
################################################################################

#
# Collect statistics on predictions for the specified threshold and source,
# taking into account batches.
#
# @param threshold the threshold for the probability table. The table contains
#       only entries which appear at least this number of times.
# @param source the source of the testing data, shall be one of "blogs", "news",
#       "twitter", or "all".
# @param type the type of the testing data, shall be one of "training",
#       "testing", "validation".
# @param removeStopwords TRUE to collect statistics for predictions with stop
#       words removed. Defaults to FALSE.
#
# @return collected statistics on the predictions.
#
stat.predict.n.build <- function(threshold, source, type,
                                 removeStopwords = FALSE) {
    predict.tbl <- predict.test.sb.cache(source, type, n.samples = 100000,
                                         threshold = threshold,
                                         removeStopwords = removeStopwords)
    
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

#
# Collect statistics on predictions for the specified threshold and all sources,
# taking into account batches.
#
# @param threshold the threshold for the probability table. The table contains
#       only entries which appear at least this number of times.
# @param type the type of the testing data, shall be one of "training",
#       "testing", "validation".
# @param removeStopwords TRUE to collect statistics for predictions with stop
#       words removed. Defaults to FALSE.
#
# @return collected statistics on the predictions.
#
stat.predict.sources.n.build <- function(threshold, type, removeStopwords = FALSE) {
    stat.blogs <- stat.predict.n.build(threshold, "blogs", type, removeStopwords)
    stat.news <- stat.predict.n.build(threshold, "news", type, removeStopwords)
    stat.twitter <- stat.predict.n.build(threshold, "twitter", type, removeStopwords)
    stat.all <- stat.predict.n.build(threshold, "all", type, removeStopwords)
    
    rbind(stat.blogs, stat.news, stat.twitter, stat.all)
}

#
# Caches statistics on predictions for the specified threshold and all sources,
# taking into account batches.
#
# @param threshold the threshold for the probability table. The table contains
#       only entries which appear at least this number of times.
# @param type the type of the testing data, shall be one of "training",
#       "testing", "validation".
# @param removeStopwords TRUE to collect statistics for predictions with stop
#       words removed. Defaults to FALSE.
#
# @return collected statistics on the predictions.
#
stat.predict.sources.n.cache <- function(threshold, type, removeStopwords = FALSE) {
    var.name <- paste0("stat.predict.", type, ".", threshold)
    var.build <- function() stat.predict.sources.n.build(threshold, type, removeStopwords)
    
    get.var.cache(var.name, var.build, removeStopwords)
}

#
# Collects statistics on predictions for all thresholds and all sources,
# taking into account batches.
#
# @param type the type of the testing data, shall be one of "training",
#       "testing", "validation".
# @param removeStopwords TRUE to collect statistics for predictions with stop
#       words removed. Defaults to FALSE.
#
# @return collected statistics on the predictions.
#
stat.predict.sources.all.build <- function(type, removeStopwords = FALSE) {
    rbind(stat.predict.sources.n.cache(1, type, removeStopwords),
          stat.predict.sources.n.cache(2, type, removeStopwords),
          stat.predict.sources.n.cache(3, type, removeStopwords),
          stat.predict.sources.n.cache(4, type, removeStopwords),
          stat.predict.sources.n.cache(5, type, removeStopwords),
          stat.predict.sources.n.cache(6, type, removeStopwords))
}

#
# Collects the aggregated statistics (mean, 95% confidence interval) on
# predictions for the specified threshold and source.
#
# @param threshold the threshold for the probability table. The table contains
#       only entries which appear at least this number of times.
# @param source the source of the testing data, shall be one of "blogs", "news",
#       "twitter", or "all".
# @param type the type of the testing data, shall be one of "training",
#       "testing", "validation".
# @param removeStopwords TRUE to collect statistics for predictions with stop
#       words removed. Defaults to FALSE.
#
# @return collected aggregated statistics on the predictions.
#
stat.predict.agg.n.build <- function(threshold, source, type,
                                     removeStopwords = FALSE) {
    # It is faster to load the cached statistics and filter out a particular
    # source, as to build it from scratch.
    stat <- stat.predict.sources.n.cache(threshold, type, removeStopwords) %>%
        filter(Source == source)
    
    # Extract number of samples for various levels of matches.
    match.1 <- stat %>% filter(Rank == 1)
    match.3 <- stat %>% filter(Rank == 3)
    match.5 <- stat %>% filter(Rank == 5)
    
    # Calculate an average and confidence interval.
    match.1.t <- t.test(match.1$Match)
    match.3.t <- t.test(match.3$Match)
    match.5.t <- t.test(match.5$Match)
    
    # Combine into a data frame.
    data.frame(Source = rep(source, 3),
               Type = rep(type, 3),
               RemoveStopwords = rep(removeStopwords, 3),
               Threshold = rep(threshold, 3),
               Rank = c(1, 3, 5),
               Mean = c(match.1.t$estimate, match.3.t$estimate, match.5.t$estimate),
               ConfIntLow = c(match.1.t$conf[1], match.3.t$conf[1], match.5.t$conf[1]),
               ConfIntHigh = c(match.1.t$conf[2], match.3.t$conf[2], match.5.t$conf[2]),
               stringsAsFactors = FALSE)
}

#
# Collects the aggregated statistics (mean, 95% confidence interval) on
# predictions for the specified threshold and all sources.
#
# @param threshold the threshold for the probability table. The table contains
#       only entries which appear at least this number of times.
# @param type the type of the testing data, shall be one of "training",
#       "testing", "validation".
# @param removeStopwords TRUE to collect statistics for predictions with stop
#       words removed. Defaults to FALSE.
#
# @return collected aggregated statistics on the predictions.
#
stat.predict.sources.agg.n.build <- function(threshold, type,
                                             removeStopwords = FALSE) {
    stat.blogs <- stat.predict.agg.n.build(threshold, "blogs", type, removeStopwords)
    stat.news <- stat.predict.agg.n.build(threshold, "news", type, removeStopwords)
    stat.twitter <- stat.predict.agg.n.build(threshold, "twitter", type, removeStopwords)
    stat.all <- stat.predict.agg.n.build(threshold, "all", type, removeStopwords)
    
    rbind(stat.blogs, stat.news, stat.twitter, stat.all)
}

#
# Collects the aggregated statistics (mean, 95% confidence interval) on
# predictions for all thresholds and all sources.
#
# @param type the type of the testing data, shall be one of "training",
#       "testing", "validation".
# @param removeStopwords TRUE to collect statistics for predictions with stop
#       words removed. Defaults to FALSE.
#
# @return collected aggregated statistics on the predictions.
#
stat.predict.sources.agg.all.build <- function(type, removeStopwords = FALSE) {
    rbind(stat.predict.sources.agg.n.build(1, type, removeStopwords),
          stat.predict.sources.agg.n.build(2, type, removeStopwords),
          stat.predict.sources.agg.n.build(3, type, removeStopwords),
          stat.predict.sources.agg.n.build(4, type, removeStopwords),
          stat.predict.sources.agg.n.build(5, type, removeStopwords),
          stat.predict.sources.agg.n.build(6, type, removeStopwords))
}

#
# Returns the data frame with statistics on duration of tbe testing.
#
stat.predict.time <- function() {
    # Raw data.
    stat <- data.frame(
        Threshold = 1:6,
        Start = c("12:48:05", "16:37:58", "19:20:17", "21:55:34", "00:17:38","02:31:06"),
        End = c("16:37:57", "19:20:15", "21:55:33", "00:17:36", "02:31:05", "04:35:41")
    )
    
    # Transform to timestamps.
    stat$Start.Ts <- strptime(stat$Start, format = "%H:%M:%S")
    stat$End.Ts <- strptime(stat$End, format = "%H:%M:%S")
    
    # Calculate the duration.
    stat$Duration <- difftime(stat$End.Ts, stat$Start.Ts, unit = "secs")
    
    # Adjust for the over-the-midnight.
    stat$Duration <- stat$Duration + ifelse(stat$Duration > 0, 0, 24 * 60 * 60)

    # Format the duration.
    stat$Duration.Fmt <- sprintf("%s", as_hms(stat$Duration))
    
    # We have processed 400.000 rows (100.000 rows for each of bogs, news and
    # Twitter, as well as 100.000 for the aggregated data source).
    # Calculate the average processing time.
    stat$Processing.Avg.Ms <- as.numeric(stat$Duration / 400000 * 1000)
    
    stat
}

################################################################################
#
# Charts.
#
################################################################################

#
# Chart: prediction quality with threshold = 1.
#
stat.predict.threshold.1.chart <- function(type = "boxplot") {
    # Collect statistics with and without stop words.
    stat.sw <- stat.predict.sources.n.cache(1, "testing", removeStopwords = FALSE)
    stat.sw$Group <- factor(paste0(stat.sw$Source, stat.sw$RemoveStopwords))
    
    stat.nosw <- stat.predict.sources.n.cache(1, "testing", removeStopwords = TRUE)
    stat.nosw$Group <- factor(paste0(stat.nosw$Source, stat.nosw$RemoveStopwords))
    
    palette.fill = c("#F9A602",
                     "#F9A60240",
                     "#B80F0A",
                     "#B80F0A40",
                     "#4CBB17",
                     "#4CBB1740",
                     "#0F52BA",
                     "#0F52BA40")
    
    if (type == "boxplot") {
        geom_func <- function(...) geom_boxplot(...)
    } else if (type == "violin") {
        geom_func <- function(...) geom_violin(..., adjust = 0.5,
                                               draw_quantiles = c(0.25, 0.5, 0.75))
    }
    
    ggplot(data = stat.sw, aes(x = as.factor(Rank), y = Match * 100, fill = Group)) +
        geom_func() +
        geom_func(data = stat.nosw, aes(x = as.factor(Rank), y = Match * 100, fill = Group)) +
        scale_fill_manual(name = "Source",
                          labels = c("Aggregated (with stop words)",
                                     "Aggregated (without stop words)",
                                     "Blogs (with stop words)",
                                     "Blogs (without stop words)",
                                     "News (with stop words)",
                                     "News (without stop words)",
                                     "Twitter (with stop words)",
                                     "Twitter (without stop words)"),
                          values = palette.fill) +
        labs(title = "Prediction precision") +
        labs(x = "Top N predictions") +
        labs(y = "Correct prediction in top N, %") + 
        theme_bw(base_size = 14)
}

#
# Chart: prediction quality with threshold = 1...6.
#
stat.predict.threshold.all.chart <- function() {
    stat.testing.all <- stat.predict.sources.agg.all.build("testing")
    
    stat.testing.1 <- stat.testing.all %>% filter(Rank == 1)
    stat.testing.3 <- stat.testing.all %>% filter(Rank == 3)
    stat.testing.5 <- stat.testing.all %>% filter(Rank == 5)
    
    palette.color = c("#F9A602",
                      "#B80F0A",
                      "#4CBB17",
                      "#0F52BA")
    
    ggplot(data = stat.testing.1, aes(x = Threshold, y = Mean * 100, color = Source)) +
        geom_line(size = 1) +
        geom_line(data = stat.testing.3, aes(x = Threshold, y = Mean * 100, color = Source), size = 1) +
        geom_line(data = stat.testing.5, aes(x = Threshold, y = Mean * 100, color = Source), size = 1) +
        scale_x_continuous(breaks=c(1:6)) +
        scale_color_manual(name = "Source",
                           labels = c("Aggregated",
                                      "Blogs",
                                      "News",
                                      "Twitter"),
                           values = palette.color) +
        annotate(geom = "text", x = 4.5, y = 17.3, label = "Correct prediction") +
        annotate(geom = "text", x = 4.5, y = 26.9, label = "Top 3") +
        annotate(geom = "text", x = 4.5, y = 31.6, label = "Top 5") +
        labs(title = "Prediction precision") +
        labs(x = "Minimum frequency threshold of n-grams table") +
        labs(y = "Correct prediction in top N, %") + 
        theme_bw(base_size = 14)
}

#
# Chart: impact of threshold on the size of n-grams in memory.
#
stat.ngram.optimize.all.chart <- function() {
    stat.size <- stat.ngram.optimize.all.cache()
    
    # Change the order of N.
    stat.size$N <- factor(stat.size$N, ordered = TRUE)
    
    ggplot(data = stat.size, aes(x = Threshold, y = Size, fill = N)) +
        geom_bar(stat="identity") +
        scale_x_continuous(breaks=c(1:6)) +
        scale_fill_brewer(palette = "Greens") +
        labs(title = "Size of n-gram tables in memory") +
        labs(x = "Threshold") +
        labs(y = "Size, MiB") +
        theme_bw(base_size = 14)
}

#
# Chart: impact of theshold on the duration of prediction.
#
stat.predict.time.chart <- function() {
    stat <- stat.predict.time()
    
    ggplot(data = stat, aes(x = Threshold, y = Processing.Avg.Ms)) +
        geom_line(size = 1) +
        geom_point(size = 2.5) +
        scale_x_continuous(breaks=c(1:6)) +
        expand_limits(y = 0) +
        labs(title = "Average duration of prediction vs. threshold") +
        labs(x = "Threshold") +
        labs(y = "Duration, ms") +
        theme_bw(base_size = 14)
}

#
# Chart: prediction quality for validation.
#
stat.predict.validation.chart <- function(type = "boxplot") {
    # Collect statistics.
    stat.sw <- stat.predict.sources.n.cache(6, "validation", removeStopwords = FALSE)
    stat.sw$Group <- factor(paste0(stat.sw$Source, stat.sw$RemoveStopwords))

    palette.fill = c("#F9A602",
                     "#B80F0A",
                     "#4CBB17",
                     "#0F52BA")
    
    if (type == "boxplot") {
        geom_func <- function(...) geom_boxplot(...)
    } else if (type == "violin") {
        geom_func <- function(...) geom_violin(..., adjust = 0.5,
                                               draw_quantiles = c(0.25, 0.5, 0.75))
    }
    
    ggplot(data = stat.sw, aes(x = as.factor(Rank), y = Match * 100, fill = Group)) +
        geom_func() +
        scale_fill_manual(name = "Source",
                          labels = c("Aggregated",
                                     "Blogs",
                                     "News",
                                     "Twitter"),
                          values = palette.fill) +
        labs(title = "Prediction precision (validation)") +
        labs(x = "Top N predictions") +
        labs(y = "Correct prediction in top N, %") + 
        theme_bw(base_size = 14)
}
