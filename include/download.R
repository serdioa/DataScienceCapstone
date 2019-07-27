#
# Downloads data from remote location.
#
# Exported functions:
#
# downloadData() - download data from remote location.

#
# Downloads and unzips the data set into directory "cache".
#
download.data <- function() {
    if (!dir.exists("cache")) {
        dir.create("cache")
    }
    file.name <- file.path("cache", "Coursera-SwiftKey.zip")
    if (!file.exists(file.name)) {
        download.file(url = "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
                      destfile = file.name, method = "curl")
        unzip(file.name, exdir = "cache")
    }        
}
