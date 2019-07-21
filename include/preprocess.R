#
# Preprocess and clean-up the data.
#
# Exported functions:
#
# split.sentences() - split data from one file on sentences.
# split.sentences.all() - split data from all files on sentences.
# preprocess.text() - preprocess text from the specified character vector.
# preprocess.file() - preprocess text from the specified file.
# preprocess.all() - preprocess text from all files.
#
# if (!exists('include.preprocess')) {
    include.preprocess <- TRUE
    
    library(fastmatch)
    library(readr)
    library(parallel)
    library(stopwords)
    library(stringr)
    
    split.sentences <- function(name, messageName) {
        message("Loading ", messageName)
        data.text <- read_lines(name)
        message("Splitting ", messageName, " on sentences")
        data.text <- unlist(tokenizers::tokenize_sentences(data.text))
        message("Done loading ", messageName)        
    }
    
    split.sentences.all <- function() {
        if (!exists("blogs.text")) {
            blogs.text <<- split.sentences("cache/en_US.blogs.training.txt", "blogs")
        }
        if (!exists("news.text")) {
            news.text <<- split.sentences("cache/en_US.news.training.txt", "news")
        }
        if (!exists("twitter.text")) {
            twitter.text <<- split.sentences("cache/en_US.twitter.training.txt", "twitter")
        }
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
    
    # Remove all underscore characters ("_").
    # There are different occurence or underscore in the corpora.
    # Sometimes several underscores are used to indicate a place to insert
    # a word. Sometimes underscores are used as an emphasis. Finally, sometimes
    # underscores are used to hide profanity.
    # We replace underscores with space characters to keep words separated
    # in case when underscores were used as separators.
    preprocess.removeUnderscores <- function(x) gsub("_", " ", x)
    
    # Surround punctuation marks which does not appear inside a word with space
    # characters. Without this step, fragments with a missing space are
    # transformed to a single non-existing word when punctuation is removed.
    # Example: corpus contains
    # "I had the best day yesterday,it was like two kids in a candy store"
    # Without this step, "yesterday,it" is transformed to a non-existing word
    # "yesterdayit" when removing punctuation. This step transforms it to
    # "yesterday, it"
    preprocess.addMissingSpace <- function(x) gsub("[,.!?()\":;”…]", " ", x)
    
    # Split text on words. Keep the words uppercase.
    preprocess.tokenize <- function(text) {
        tokenizers::tokenize_words(text,
                                   lowercase = FALSE,
                                   simplify = TRUE,
                                   strip_numeric = TRUE)
    }
    
    # Replace words in a sentence with replacements available from the table.
    # Keep words which are not in the replacement table "as is".
    # As a side effect, removes punctuation and transforms to a lower case.
    #
    # Extract tokens to a separate variable to be able to use fast match.
    replacements.tokens <- readr::read_csv("replacements.txt",
                                         col_names = c("token", "replacement"),
                                         col_types = list(col_character(), col_character()))
    replacements.tokens.token <- replacements.tokens$token

    # Prepare a vector with stopwords.
    # The vector is cached to be able to use fast-search (fmatch).
    stopwords.tokens <- stopwords::stopwords()
    
    preprocess.replaceWords <- function(tokens) {
        # Replace character often used in the sample text instead of the
        # upper single quote in abbreviations.
        tokens <- gsub("’", "'", tokens)
        tokens <- gsub("\u0092", "'", tokens)

        # Attempt to replace each word.
        tokens <- sapply(tokens, function(x) {
            # Search if a replacement exist.
            replacement.index <- fmatch(x, replacements.tokens.token)
            replacement.index.tolower <- fmatch(tolower(x), replacements.tokens.token)
            
            ifelse(!is.na(replacement.index),
                   # Replace the token.
                   replacements.tokens$replacement[replacement.index],
                   # Can't find a replacement, try lowercase.
                   ifelse(!is.na(replacement.index.tolower),
                          # Found a replacement with lowercase. replace the token,
                          # changing the first letter to uppercase.
                          str_to_sentence(replacements.tokens$replacement[replacement.index.tolower]),
                          # Still can't find a replacement, fall back on the token.
                          x
                   )
            )
        }, USE.NAMES = FALSE)
        
        tokens <- unlist(preprocess.tokenize(tokens))
    }
    
    preprocess.removeStopwords <- function(tokens) {
        # Step 1: look up lower-cased input in stopwords. The output is
        # a vector with the same length as input, with NA where no match found
        # and an index of a stopword where a stopword is found.
        # Step 2: transform to a boolean vector where NA (no match found)
        # are TRUE.
        # Step 3: select only elements with TRUE, that is which are not matched
        # as a stopword.
        words.keep.index <- is.na(fmatch(tolower(tokens), stopwords.tokens))
        tokens[words.keep.index]
    }
    
    preprocess.removeNonEnglish <- function(tokens) {
        # Valid characters:
        # * A-Z, a-z
        # * Extended Latin (accented): \u00c0 - \u00ff
        # * Upper single quote
        tokens.valid <- grepl("^[A-Za-z'\u00c0-\u00ff]+$", tokens)
        tokens[tokens.valid]
    }

    # Add tokens representing start of a sentence.
    # STOS = Start Of Sentence.
    preprocess.addSentenceTokens <- function(tokens) c("STOS", tokens)

    # Define severel functions and combine them in a pre-processing chain.
    preprocess.text <- function(x) {
        text <- preprocess.removeUrl(x)
        text <- preprocess.removeEmail(text)
        text <- preprocess.removeTagsAndHandles(text)
        text <- preprocess.removeUnderscores(text)
        text <- preprocess.addMissingSpace(text)
        
        tokens <- preprocess.tokenize(text)
        tokens <- preprocess.replaceWords(tokens)
#        tokens <- preprocess.removeStopwords(tokens)
        tokens <- preprocess.removeNonEnglish(tokens)
        tokens <- preprocess.addSentenceTokens(tokens)

        paste(tokens, collapse = " ")
    }
    
    preprocess.file <- function(x, name, messageName) {
        if (!file.exists(name)) {
            message("Pre-processing ", messageName)
            ts <- system.time(text.preprocessed <- unlist(mclapply(x, preprocess.text)))
            print(ts)
            message("Removing empty lines from pre-processed ", messageName)            
            text.preprocessed.empty <- grepl("STOS\\s*$", text.preprocessed)
            text.preprocessed <- text.preprocessed[!text.preprocessed.empty]
            message("Saving pre-processed ", messageName)
            write_lines(text.preprocessed, name)
            message("Done pre-processing ", messageName)
        } else {
            message("Loading pre-processed ", messageName)
            text.preprocessed <- read_lines(name)
            message("Done loading pre-processed ", messageName)
        }

        return (text.preprocessed)
    }
    
    preprocess.all <- function() {
        if (!exists("blogs.text.preprocessed")) {
            blogs.text <- read_lines("cache/en_US.blogs.training.txt")
            blogs.text <- unlist(tokenizers::tokenize_sentences(blogs.text))
            blogs.text.preprocessed <<- preprocess.file(blogs.text,
                                                        "cache/blogs.text.preprocessed.txt",
                                                        "blogs")
        }
        if (!exists("news.text.preprocessed")) {
            news.text <- read_lines("cache/en_US.news.training.txt")
            news.text <- unlist(tokenizers::tokenize_sentences(news.text))
            news.text.preprocessed <<- preprocess.file(news.text,
                                                       "cache/news.text.preprocessed.txt",
                                                       "news")
        }
        if (!exists("twitter.text.preprocessed")) {
            twitter.text <- read_lines("cache/en_US.twitter.training.txt")
            twitter.text <- unlist(tokenizers::tokenize_sentences(twitter.text))
            twitter.text.preprocessed <<- preprocess.file(twitter.text,
                                                        "cache/twitter.text.preprocessed.txt",
                                                        "twitter")
        }
    }
#}
