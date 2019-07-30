#
# Predicts next word based on pre-calculated n-grams using the Stupid Backoff
# algorithm.
#

source("include/preprocess.R")
source("include/ngram.optimize.R")

#
# Pre-processes text before predicting next word based on the text.
# The pre-processing transforms words to a format expected for the prefix:
# only low case, only stems, no punctuation etc.
#
# @param data.text the text to be pre-processed before prediction.
# @param removeStopwords TRUE to remove stop words from the text.
#
# @return the character vector with tokens of the pre-processed text.
#
preprocess.text.predict <- function(data.text, removeStopwords = FALSE) {
    # Split the text on sentences and keep only the last one.
    data.sentences <- unlist(tokenizers::tokenize_sentences(data.text))
    data.sentences.length <- length(data.sentences)
    if (data.sentences.length < 1) {
        return (NA)
    }
    text <- data.sentences[data.sentences.length]
    
    # Preprocess the text.
    text <- preprocess.removeUrl(text)
    text <- preprocess.removeEmail(text)
    text <- preprocess.removeTagsAndHandles(text)
    text <- preprocess.removeUnderscores(text)
    text <- preprocess.addMissingSpace(text)
    
    tokens <- preprocess.tokenize(text)
    tokens <- preprocess.replaceWords(tokens)
    if (removeStopwords) {
        tokens <- preprocess.removeStopwords(tokens)
    }
    tokens <- preprocess.removeNonLatin(tokens)
    tokens <- preprocess.stem.word(tokens, stems.top.cache(removeStopwords))
    tokens <- preprocess.addSentenceTokens(tokens)
    
    tokens
}

#
# Returns predicted candidate words following the specified tokens.
#
# The returned data frame contains the following columns:
#
# * Prefix - the text prefix used for prediction.
# * Suffix - the predicted next word.
# * N - the n parameter of n-gram used for this prediction.
# * Prob - the column "Prob" from the n-gram table used for this prediction.
# * Score - the score of this prediction. Score is not necessary a probability
#       (scores do not sum to 1), but a higher score implies a higher
#       probability.
#
# The returned data frame is sorted by score in the descending order, so that
# the higher-scored (and thus higher-probability) candidates are on top.
# 
# @param prefix character vector with tokens.
# @param word.pattern the regular expression for the predicted candidates.
#       The pattern is used when predicting partially entered words.
#       Defaults to NULL to not use any regular expression.
# @param n number of candidates to return. Defaults to 5.
# @param threshold the minimum number of n-gram occurencies to use for
#       prediction. Defaults to 5.
# @param removeStopwords TRUE to use n-grams with removed stop words.
#       Defaults to FALSE.
#
# @return the data frame with predicted candidates.
#
sb.predict <- function(prefix, word.pattern = NULL, n = 5, threshold = 6, removeStopwords = FALSE) {
    # Make the threshold the right length.
    threshold <- rep(threshold, length.out = 5)
    
    prefix.length <- length(prefix)
    
    # Load the table for encoding the prefixes.        
    dict.hash <- dict.hash.cache(removeStopwords)
    
    # Create a table to hold results.
    candidates <- data.frame(Prefix = character(),
                             Suffix = character(),
                             N = integer(),
                             SuffixProb = numeric(),
                             Score = numeric())
    
    # Choose candidates from 5- to 2-grms.
    # 1-grams (context-free prediction which completely ignores the prefix)
    # are used only as the last resort if not enough candidates are found.
    for (i in 4:1) {
        if (prefix.length >= i) {
            # Choose the right encoding function.
            tokens.encode <- tokens.encode.n(i)
            
            # Encode the (i-1) part of the prefix.
            prefix.n <- tail(prefix, i)
            prefix.n.code <- tokens.encode(prefix.n, dict.hash)
            
            # Choose candidates starting with our prefix from optimized
            # table with n-grams.
            table.ngram.n <- ngram.optimize.cache(i + 1,
                                                  threshold = threshold[i + 1],
                                                  removeStopwords = removeStopwords)
            table.candidates.n <- table.ngram.n[.(prefix.n.code), nomatch = NULL][!(Suffix %in% candidates$Suffix)]
            if (!is.null(word.pattern)) {
                table.candidates.n <- table.candidates.n %>%
                    filter(grepl(word.pattern, Suffix))
            }
            
            if (nrow(table.candidates.n) > 0) {
                table.candidates.n <- table.candidates.n %>%
                    transmute(Prefix = paste0(prefix.n, collapse = " "),
                              Suffix = Suffix,
                              N = i,
                              SuffixProb = Prob,
                              Score = exp(Prob / 1000000 + log(0.4) * (min(4, prefix.length) - i)))
                
                candidates <- rbind(candidates, table.candidates.n)
            }
        }
    }
    
    # If we still have not enough candidates, choose from 1-grams
    # context-free, that is without taking the prefix in the consideration.
    candidates.found <- nrow(candidates)
    if (candidates.found < n) {
        table.ngram.1 <- ngram.optimize.cache(1, threshold = threshold[1],
                                              removeStopwords = removeStopwords)
        table.candidates.1 <- table.ngram.1 %>%
            filter(!(Suffix %in% candidates$Suffix))
        if (!is.null(word.pattern)) {
            table.candidates.1 <- table.candidates.1 %>%
                filter(grepl(word.pattern, Suffix))
        }
        table.candidates.1 <- table.candidates.1 %>%
            transmute(Prefix = "",
                      Suffix = Suffix,
                      N = 0,
                      Prob = Prob,
                      Score = exp(Prob / 1000000 + 4 * log(0.4))) %>%
            head(n - candidates.found)
        
        candidates <- rbind(candidates, table.candidates.1)
    }
    
    if (nrow(candidates) == 0 && !is.na(word.pattern)) {
        word.pattern.clean <- gsub("[^a-zA-Z]", "", word.pattern)
        suggestions <- hunspell_suggest(word.pattern.clean)
        suggestions.length <- length(suggestions[[1]])
        if (suggestions.length >= 1) {
            candidates <- data.frame(Prefix = "",
                                     Suffix = suggestions[[1]],
                                     N = 0,
                                     Prob = 0,
                                     Score = 1e-10 * (suggestions.length : 1))
        }
    }
    
    candidates %>%
        arrange(desc(Score)) %>%
        head(n) %>%
        mutate_if(is.factor, as.character)
}

#
# Returns predicted candidate words following the specified text.
#
# The prediction algorithm optionally supports predicting partially entered
# words, if the argument predict.partial is TRUE. If this is the case, we check
# if the text ends with a space character or not. If the last character is not
# the space, we assume that the last word is entered only partially and attempt
# to predict the ending of this word, using all preceding words as a prefix.
#
# The returned data frame contains the following columns:
#
# * Prefix - the text prefix used for prediction.
# * Suffix - the predicted next word.
# * N - the n parameter of n-gram used for this prediction.
# * Prob - the column "Prob" from the n-gram table used for this prediction.
# * Score - the score of this prediction. Score is not necessary a probability
#       (scores do not sum to 1), but a higher score implies a higher
#       probability.
#
# The returned data frame is sorted by score in the descending order, so that
# the higher-scored (and thus higher-probability) candidates are on top.
# 
# @param text the text to predict the next word for.
# @param predict.partial if TRUE, predict partially entered words.
# @param n number of candidates to return. Defaults to 5.
# @param threshold the minimum number of n-gram occurencies to use for
#       prediction. Defaults to 5.
# @param removeStopwords TRUE to use n-grams with removed stop words.
#       Defaults to FALSE.
#
# @return the data frame with predicted candidates.
#
sb.predict.text <- function(text, predict.partial = FALSE, n = 5, threshold = 5, removeStopwords = FALSE) {
    use.pattern <- predict.partial && !(substring(text, nchar(text)) %in% c("", " "))
    
    word.pattern <- NULL
    if (use.pattern) {
        # Split the last word.
        splitted <- strsplit(text, "\\s+(?=[^\\s]+$)", perl = TRUE)
        splitted.length = length(splitted[[1]])
        if (splitted.length > 1) {
            text <- splitted[[1]][1]
            word.pattern <- paste0("^", splitted[[1]][2])
        } else if (splitted.length == 1) {
            text <- ""
            word.pattern <- paste0("^", splitted[[1]][1])
        }
    }
    
    prefix.tokens <-
        preprocess.text.predict(text, removeStopwords = removeStopwords)
    sb.predict(
        prefix.tokens,
        word.pattern = word.pattern,
        n = n,
        threshold = threshold,
        removeStopwords = removeStopwords
    )
}
