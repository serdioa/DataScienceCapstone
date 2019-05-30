#
# Split the corpora on 3 parts:
#  * Training set (60%)
#  * Testing set (20%)
#  * Validation set (20%)
#
# The training set will be used to build and train the algorithm.
#
# The testing set will be used to test the algorithm during it's development.
# This set may be used more than once.
#
# The validation set will be used for a final validation and estimation
# of out-of-sample performance. This set will be used only once.
#

# Split the specified file on training, testing and validation sets.
splitFile <- function(name) {
    outTraining <- sub("(.)\\.[^.]+$", "\\1.training.txt", name)
    outTesting <- sub("(.)\\.[^.]+$", "\\1.testing.txt", name)
    outValidation <- sub("(.)\\.[^.]+$", "\\1.validation.txt", name)

    print(sprintf("Reading dataset from file %s...", name))
    inCon <- file(name, open = "r")
    data <- readLines(con = inCon)
    close(inCon)

    # Select datasets.
    print("Splitting dataset")
    selectedProb <- runif(length(data))
    selectedTraining <- (selectedProb < 0.6)
    selectedTesting <- (selectedProb >= 0.6 & selectedProb < 0.8)
    selectedValidation <- (selectedProb >= 0.8)

    print(sprintf("Writing training dataset to file %s...", outTraining))
    outCon <- file(outTraining, open = "w")
    writeLines(data[selectedTraining], outCon)
    close(outCon)

    print(sprintf("Writing testing dataset to file %s...", outTesting))
    outCon <- file(outTesting, open = "w")
    writeLines(data[selectedTesting], outCon)
    close(outCon)

    print(sprintf("Writing validation dataset to file %s...", outValidation))
    outCon <- file(outValidation, open = "w")
    writeLines(data[selectedValidation], outCon)
    close(outCon)
}

# Set the seed of the RND generator to make results reproduceable.
set.seed(20190530)

# Split files.
splitFile("data/en_US.blogs.txt")
splitFile("data/en_US.news.txt")
splitFile("data/en_US.twitter.txt")
