#
# Splits data file on training, testing and validation subsets.
#
# Exported functions:
#
# split.file() - splits one file on subsets.
# split.all() - splits all 3 sources (blogs, news, twitter) on subsets.

if (!exists('include.split')) {
    include.split <- TRUE
    
    # Arguments:
    # name - the file to split
    # out.dir - output directory
    split.file <- function(name, out.dir) {
        # Reading dataset from the input file.
        data <- read_lines(name)
        
        # Prepare list with indexes of all data items.
        data.index <- 1:length(data)
        
        # Sample indices for the training data set, and create a set with remaining
        # indices.
        training.index <- sample(data.index, 0.6 * length(data.index))
        remaining.index <- data.index[! data.index %in% training.index]
        
        # Sample indices for the testing data set, and use remaining indices
        # for a validation data set.
        testing.index <- sample(remaining.index, 0.5 * length(remaining.index))
        validation.index <- remaining.index[! remaining.index %in% testing.index]
        
        # Split the data.
        data.training <- data[training.index]
        data.testing <- data[testing.index]
        data.validation <- data[validation.index]
        
        # Create an output directory, if it does not exist.
        if (!dir.exists(out.dir)) {
            dir.create(out.dir)
        }
        
        # Prepare names for output files. We append suffixes "training", "testing"
        # and "validation" to the input file name before the extension.
        base <- basename(name)
        outTraining <- file.path(out.dir, sub("(.)\\.[^.]+$", "\\1.training.txt", base))
        outTesting <- file.path(out.dir, sub("(.)\\.[^.]+$", "\\1.testing.txt", base))
        outValidation <- file.path(out.dir, sub("(.)\\.[^.]+$", "\\1.validation.txt", base))
        
        # Writing datasets to output files.
        write_lines(data.training, outTraining)
        write_lines(data.testing, outTesting)
        write_lines(data.validation, outValidation)
    }
    
    split.all <- function() {
        if (!file.exists("cache/en_US.blogs.training.txt")) {
            message("Splitting blogs")
            splitFile("cache/final/en_US/en_US.blogs.txt", "cache")
            message("Done splitting blogs")
        }
        if (!file.exists("cache/en_US.news.training.txt")) {
            message("Splitting news")
            splitFile("cache/final/en_US/en_US.news.txt", "cache")
            message("Done splitting news")
        }
        if (!file.exists("cache/en_US.twitter.training.txt")) {
            message("Splitting twitter")
            splitFile("cache/final/en_US/en_US.twitter.txt", "cache")
            message("Done splitting twitter")
        }        
    }
}
