#
# Shows a table with a number of 2- to 6-grams.
#

library(dplyr)
library(data.table)
library(ggplot2)

if (FALSE) {
    if (!exists("all.95.2gram.freq")) {
        message("Loading 2-grams (95% coverage)")
        all.95.2gram.freq <- readRDS("cache/all.95.2gram.freq.RDS")
        message("done loading 2-grams (95% coverage)")
    }
    if (!exists("all.95.3gram.freq")) {
        message("Loading 3-grams (95% coverage)")
        all.95.3gram.freq <- readRDS("cache/all.95.3gram.freq.RDS")
        message("done loading 3-grams (95% coverage)")
    }
    if (!exists("all.95.4gram.freq")) {
        message("Loading 4-grams (95% coverage)")
        all.95.4gram.freq <- readRDS("cache/all.95.4gram.freq.RDS")
        message("done loading 4-grams (95% coverage)")
    }
    if (!exists("all.95.5gram.freq")) {
        message("Loading 5-grams (95% coverage)")
        all.95.5gram.freq <- readRDS("cache/all.95.5gram.freq.RDS")
        message("done loading 5-grams (95% coverage)")
    }
    if (!exists("all.95.6gram.freq")) {
        message("Loading 6-grams (95% coverage)")
        all.95.6gram.freq <- readRDS("cache/all.95.6gram.freq.RDS")
        message("done loading 6-grams (95% coverage)")
    }
}

#
# Enrich
#
# Enrich 2-grams: add columns indicating whether the 1st or 2nd words are
# an UNK token.
# Enrich 2-grams: add cumulative percentage for each token type.
if (TRUE) {
    # Use log-scale for sub-sampling the X axis.
    all.2gram.95.freq.idx <- unique(as.integer(exp((1:1000) * log(nrow(all.2gram.95.freq)) / 1000)))
    
    all.2gram.95.freq.enriched <- as_tibble(all.2gram.95.freq) %>%
    dplyr::mutate(Freq.Pct = Freq / sum(Freq),
                  UNK_N = str_count(Terms, "UNK"),
                  UNK_0.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 0, Freq.Pct, 0)),
                  UNK_1.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 1, Freq.Pct, 0)),
                  UNK_2.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 2, Freq.Pct, 0))
    ) %>%
    dplyr::slice(all.2gram.95.freq.idx)
    
    saveRDS(all.2gram.95.freq.enriched, "cache/all.2gram.95.freq.enriched.RDS")
}

if (TRUE) {
    all.3gram.95.freq.idx <- unique(as.integer(exp((1:250) * log(nrow(all.3gram.95.freq)) / 250)))
    
    # Enrich 3-grams: add a column with a count of UNK token.
    # Enrich 3-grams: add cumulative percentage for each token type.
    all.3gram.95.freq.enriched <- as_tibble(all.3gram.95.freq) %>%
        dplyr::mutate(UNK_N = str_count(Terms, "UNK"),
                      Freq.Pct = Freq / sum(Freq),
                      UNK_0.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 0, Freq.Pct, 0)),
                      UNK_1.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 1, Freq.Pct, 0)),
                      UNK_2.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 2, Freq.Pct, 0)),
                      UNK_3.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 3, Freq.Pct, 0))
        ) %>%
        dplyr::slice(all.3gram.95.freq.idx)
    
    saveRDS(all.3gram.95.freq.enriched, "cache/all.3gram.95.freq.enriched.RDS")
}

if (TRUE) {
    all.4gram.95.freq.idx <- unique(as.integer(exp((1:250) * log(nrow(all.4gram.95.freq)) / 250)))
    
    # Enrich 4-grams: add a column with a count of UNK token.
    # Enrich 4-grams: add cumulative percentage for each token type.
    all.4gram.95.freq.enriched <- as_tibble(all.4gram.95.freq) %>%
        dplyr::mutate(UNK_N = str_count(Terms, "UNK"),
                      Freq.Pct = Freq / sum(Freq),
                      UNK_0.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 0, Freq.Pct, 0)),
                      UNK_1.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 1, Freq.Pct, 0)),
                      UNK_2.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 2, Freq.Pct, 0)),
                      UNK_3.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 3, Freq.Pct, 0)),
                      UNK_4.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 4, Freq.Pct, 0))
        ) %>%
        dplyr::slice(all.4gram.95.freq.idx)
    
    saveRDS(all.4gram.95.freq.enriched, "cache/all.4gram.95.freq.enriched.RDS")
}

if (TRUE) {
    all.5gram.95.freq.idx <- unique(as.integer(exp((1:250) * log(nrow(all.5gram.95.freq)) / 250)))
    
    # Enrich 5-grams: add a column with a count of UNK token.
    # Enrich 5-grams: add cumulative percentage for each token type.
    all.5gram.95.freq.enriched <- as_tibble(all.5gram.95.freq) %>%
        dplyr::mutate(UNK_N = str_count(Terms, "UNK"),
                      Freq.Pct = Freq / sum(Freq),
                      UNK_0.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 0, Freq.Pct, 0)),
                      UNK_1.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 1, Freq.Pct, 0)),
                      UNK_2.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 2, Freq.Pct, 0)),
                      UNK_3.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 3, Freq.Pct, 0)),
                      UNK_4.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 4, Freq.Pct, 0)),
                      UNK_5.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 5, Freq.Pct, 0))
        ) %>%
        dplyr::slice(all.5gram.95.freq.idx)
    
    saveRDS(all.5gram.95.freq.enriched, "cache/all.5gram.95.freq.enriched.RDS")
}

if (TRUE) {
    all.6gram.95.freq.idx <- unique(as.integer(exp((1:250) * log(nrow(all.6gram.95.freq)) / 250)))
    
    # Enrich 6-grams: add a column with a count of UNK token.
    # Enrich 6-grams: add cumulative percentage for each token type.
    all.6gram.95.freq.enriched <- as_tibble(all.6gram.95.freq) %>%
        dplyr::mutate(UNK_N = str_count(Terms, "UNK"),
                      Freq.Pct = Freq / sum(Freq),
                      UNK_0.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 0, Freq.Pct, 0)),
                      UNK_1.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 1, Freq.Pct, 0)),
                      UNK_2.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 2, Freq.Pct, 0)),
                      UNK_3.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 3, Freq.Pct, 0)),
                      UNK_4.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 4, Freq.Pct, 0)),
                      UNK_5.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 5, Freq.Pct, 0)),
                      UNK_6.Freq.Cum.Pct = cumsum(ifelse(UNK_N == 6, Freq.Pct, 0))
        ) %>%
        dplyr::slice(all.6gram.95.freq.idx)
    
    saveRDS(all.6gram.95.freq.enriched, "cache/all.6gram.95.freq.enriched.RDS")
}

all.ngram.freq.tbl <-
    data.frame(
        "Corpora Coverage" = c("25%", "50%", "75%", "95%"),
        "2-grams" = c(
            nrow(all.2gram.95.freq[all.2gram.95.freq$Freq.Cum.Pct <= 0.25]) / nrow(all.2gram.95.freq) * 100,
            nrow(all.2gram.95.freq[all.2gram.95.freq$Freq.Cum.Pct <= 0.50]) / nrow(all.2gram.95.freq) * 100,
            nrow(all.2gram.95.freq[all.2gram.95.freq$Freq.Cum.Pct <= 0.75]) / nrow(all.2gram.95.freq) * 100,
            nrow(all.2gram.95.freq[all.2gram.95.freq$Freq.Cum.Pct <= 0.95]) / nrow(all.2gram.95.freq) * 100
        ),
        "3-grams" = c(
            nrow(all.3gram.95.freq[all.3gram.95.freq$Freq.Cum.Pct <= 0.25]) / nrow(all.3gram.95.freq) * 100,
            nrow(all.3gram.95.freq[all.3gram.95.freq$Freq.Cum.Pct <= 0.50]) / nrow(all.3gram.95.freq) * 100,
            nrow(all.3gram.95.freq[all.3gram.95.freq$Freq.Cum.Pct <= 0.75]) / nrow(all.3gram.95.freq) * 100,
            nrow(all.3gram.95.freq[all.3gram.95.freq$Freq.Cum.Pct <= 0.95]) / nrow(all.3gram.95.freq) * 100
        ),
        "4-grams" = c(
            nrow(all.4gram.95.freq[all.4gram.95.freq$Freq.Cum.Pct <= 0.25]) / nrow(all.4gram.95.freq) * 100,
            nrow(all.4gram.95.freq[all.4gram.95.freq$Freq.Cum.Pct <= 0.50]) / nrow(all.4gram.95.freq) * 100,
            nrow(all.4gram.95.freq[all.4gram.95.freq$Freq.Cum.Pct <= 0.75]) / nrow(all.4gram.95.freq) * 100,
            nrow(all.4gram.95.freq[all.4gram.95.freq$Freq.Cum.Pct <= 0.95]) / nrow(all.4gram.95.freq) * 100
        ),
        "5-grams" = c(
            nrow(all.5gram.95.freq[all.5gram.95.freq$Freq.Cum.Pct <= 0.25]) / nrow(all.5gram.95.freq) * 100,
            nrow(all.5gram.95.freq[all.5gram.95.freq$Freq.Cum.Pct <= 0.50]) / nrow(all.5gram.95.freq) * 100,
            nrow(all.5gram.95.freq[all.5gram.95.freq$Freq.Cum.Pct <= 0.75]) / nrow(all.5gram.95.freq) * 100,
            nrow(all.5gram.95.freq[all.5gram.95.freq$Freq.Cum.Pct <= 0.95]) / nrow(all.5gram.95.freq) * 100
        ),
        "6-grams" = c(
            nrow(all.6gram.95.freq[all.6gram.95.freq$Freq.Cum.Pct <= 0.25]) / nrow(all.6gram.95.freq) * 100,
            nrow(all.6gram.95.freq[all.6gram.95.freq$Freq.Cum.Pct <= 0.50]) / nrow(all.6gram.95.freq) * 100,
            nrow(all.6gram.95.freq[all.6gram.95.freq$Freq.Cum.Pct <= 0.75]) / nrow(all.6gram.95.freq) * 100,
            nrow(all.6gram.95.freq[all.6gram.95.freq$Freq.Cum.Pct <= 0.95]) / nrow(all.6gram.95.freq) * 100
        )
    )

all.ngram.freq.tbl[,2] <- sprintf("%.2f", as.double(all.ngram.freq.tbl[,2]))
all.ngram.freq.tbl[,3] <- sprintf("%.2f", as.double(all.ngram.freq.tbl[,3]))
all.ngram.freq.tbl[,4] <- sprintf("%.2f", as.double(all.ngram.freq.tbl[,4]))
all.ngram.freq.tbl[,5] <- sprintf("%.2f", as.double(all.ngram.freq.tbl[,5]))
all.ngram.freq.tbl[,6] <- sprintf("%.2f", as.double(all.ngram.freq.tbl[,6]))


