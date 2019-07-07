#
# Predicts next word based on pre-calculated n-grams.
#
# if (!exists('ngram.predict')) {
    ngram.predict <- TRUE
    
    library(dplyr)
    library(fastmatch)
    library(readr)
    library(stringr)
    
    all.ngram.collapsed.load.n <- function(n) {
        all.ngram.collapsed.var <- paste0("all.", n, "gram.collapsed")
        if (!exists(all.ngram.collapsed.var)) {
            message("Loading collapsed ", n, "-grams")
            all.ngram.collapsed.file <- paste0("cache/all.", n, "gram.collapsed.RDS")
            assign(all.ngram.collapsed.var,
                   readRDS(all.ngram.collapsed.file),
                   inherits = TRUE)
            message("Done loading collapsed ", n, "-grams")
        }
    }
    
    all.ngram.collapsed.load <- function() {
        all.ngram.collapsed.load.n(2)
        all.ngram.collapsed.load.n(3)
        all.ngram.collapsed.load.n(4)
    }
    
    all.ngram.hash.cache <- function() {
        all.ngram.collapsed.load()
        
        all.ngram.hash <- list()
        
        all.ngram.hash$Prefix.2 <- fmatch.hash("test", all.2gram.collapsed$Prefix)
        all.ngram.hash$Prefix.3 <- fmatch.hash("test", all.3gram.collapsed$Prefix)
        all.ngram.hash$Prefix.4 <- fmatch.hash("test", all.4gram.collapsed$Prefix)
        
        return (all.ngram.hash)
    }
    
    # Remove URLs. The regular expression detects http(s) and ftp(s) protocols.
    preprocess.removeUrl <- function(x) gsub("(ht|f)tp(s?)://\\S+", "", x)
    
    # Remove e-mail addresses.
    # The regular expression from Stack Overflow:
    # https://stackoverflow.com/questions/201323/how-to-validate-an-email-address-using-a-regular-expression
    preprocess.removeEmail <- function(x) gsub("(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])", "", x, perl = TRUE)
    
    # Remove hash tags (the character # and the following word) and twitter handles
    # (the character @ and the following word).
    preprocess.removeTagsAndHandles <- function(x) gsub("[@#]\\S+", "", x)
    
    # Surround punctuation marks which does not appear inside a word with space
    # characters. Without this step, fragments with a missing space are transformed
    # to a single non-existing word when punctuation is removed.
    # Example: corpus contains
    # "I had the best day yesterday,it was like two kids in a candy store"
    # Without this step, "yesterday,it" is transformed to a non-existing word
    # "yesterdayit" when removing punctuation. This step transforms it to
    # "yesterday, it"
    preprocess.addMissingSpace <- function(x) gsub("[,()\":;”…]", " ", x)
    
    # Replace words in a sentence with replacements available from the table.
    # Keep words which are not in the replacement table "as is".
    # As a side effect, removes punctuation and transforms to a lower case.
    replacements.text <- readr::read_csv("replacements.txt",
                                         col_names = c("token", "replacement"),
                                         col_types = list(col_character(), col_character()))
    
    preprocess.replaceWords <- function(text, replacements) {
        # Replace character often used in the sample text instead of the
        # upper quote in abbreviations.
        text <- gsub("’", "'", text)
        text <- gsub("\u0092", "'", text)
        
        # Split text on words. Keep the words uppercase.
        tokens.orig <- tokenizers::tokenize_words(text,
                                                  lowercase = FALSE,
                                                  simplify = TRUE,
                                                  strip_numeric = TRUE)
        
        # Attempt to replace each word.
        unlist(sapply(tokens.orig, function(x) {
            # Search if a replacement exist.
            replacement.index <- match(x, replacements$token)
            if (is.na(replacement.index)) {
                # Can't find a replacement, try lowercase.
                replacement.index <- match(tolower(x), replacements$token)
                if (is.na(replacement.index)) {
                    # Still can't find a replacement, fall back on the token.
                    return (x)
                } else {
                    # Found a replacement with lowercase. replace the token.
                    return (tokenizers::tokenize_words(
                        replacements$replacement[replacement.index],
                        lowercase = TRUE,
                        simplify = TRUE,
                        strip_numeric = TRUE))
                }
            } else {
                # Replace the token.
                return (tokenizers::tokenize_words(
                    replacements$replacement[replacement.index],
                    lowercase = TRUE,
                    simplify = TRUE,
                    strip_numeric = TRUE))
            }
        }, USE.NAMES = FALSE))
    }
    
    preprocess.stems.top.load <- function() {
        # Load all stems.
        stems.freq.file <- "cache/stems.freq.RDS"
        stems.freq <- readRDS(stems.freq.file)
        
        # We will use 2 bytes to encode each stem, but we require 2 additional
        # special tokens: STOS for Start-Of-Sentence, and UNK for a Unknown
        # Word, that is a word which is not included in our encoding table.
        # The token STOS is already included in stems, so we must make place
        # only for the token UNK, that is we may encode (256 * 256 - 2) top
        # stems.
        tibble(Terms = stems.freq$Terms[1:(256 * 256 - 1)])
    }    
    
    preprocess.stems.top <- unlist(preprocess.stems.top.load())
    
    # Keep the word, if it is included in the table with top stems.
    # Otherwise, replaces the word with the specified token.
    preprocess.top.stem.keep <- function(stem) {
        ifelse(is.na(fmatch(stem, preprocess.stems.top)), "UNK", stem)
    }
    
    # Stem the specified token, transform it to the lower case, and
    # replace with the UNK token if it is not in the list of top stems.
    preprocess.stem.word <- function(x) {
        ifelse(x == "STOS",
               x,
               preprocess.top.stem.keep(SnowballC::wordStem(tolower(x), language = "en")))
    }
    
    # Add tokens representing start of a sentence.
    # STOS = Start Of Sentence.
    preprocess.addSentenceTokens <- function(x) c("STOS", x)
    
    # Collapse space characters: if there are more than 1 space character in a row,
    # replace with a single one.
    preprocess.collapseWhitespace <- function(x) gsub("\\s+", " ", x)
    
    # Define severel functions and combine them in a pre-processing chain.
    preprocess.text <- function(data.text) {
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
        text <- preprocess.addMissingSpace(text)
        text <- preprocess.replaceWords(text, replacements.text)
        text <- preprocess.stem.word(text)
        text <- preprocess.addSentenceTokens(text)

        return(text)
    }

    verbose.message <- function(...) {
        if (exists("verbose") && (verbose == TRUE)) {
            message(...)
        }
    }
    
    predict.word <- function(text, verbose = TRUE) {
        tokens.all <- preprocess.text(text)
        verbose.message("tokens.all=", paste(tokens.all, collapse = " "))
        
        tokens <- tail(tokens.all, 3)
        if (length(tokens) >= 3) {
            prefix <- paste0(tokens, collapse = " ")
            verbose.message("prefix.4=", prefix)
            
            index <- fmatch(prefix, all.ngram.hash$Prefix.4)
            if (!is.na(index)) {
                found <- all.4gram.collapsed[index,]
                print(found)
            } else {
                message("Can't find 4-gram")
            }
        }
        
        tokens <- tail(tokens.all, 2)
        if (length(tokens) >= 2) {
            prefix <- paste0(tokens, collapse = " ")
            verbose.message("prefix.3=", prefix)
            
            index <- fmatch(prefix, all.ngram.hash$Prefix.3)
            if (!is.na(index)) {
                found <- all.3gram.collapsed[index,]
                print(found)
            } else {
                message("Can't find 3-gram")
            }
        }
        
        tokens <- tail(tokens.all, 1)
        if (length(tokens) >= 1) {
            prefix <- paste0(tokens, collapse = " ")
            verbose.message("prefix.2=", prefix)
            
            index <- fmatch(prefix, all.ngram.hash$Prefix.2)
            if (!is.na(index)) {
                found <- all.2gram.collapsed[index,]
                print(found)
            } else {
                message("Can't find 2-gram")
            }
        }
    }
    
    quiz.3 <- function() {
        predict.word("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
        predict.word("You're the reason why I smile everyday. Can you follow me please? It would mean the")
        predict.word("Hey sunshine, can you follow me and make me the")
        predict.word("Very early observations on the Bills game: Offense still struggling but the")
        predict.word("Go on a romantic date at the")
        predict.word("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my")
        predict.word("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some")
        predict.word("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little")
        predict.word("Be grateful for the good times and keep the faith during the")
        predict.word("If this isn't the cutest thing you've ever seen, then you must be")
    }
    
# }


