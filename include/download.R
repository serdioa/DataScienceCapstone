#
# Downloads data from remote location.
#
# Exported functions:
#
# downloadData() - download data from remote location.
#
if (!exists('include.download')) {
    include.download <- TRUE
    
    download.data <- function() {
        if (!dir.exists("cache")) {
            dir.create("cache")
        }
        if (!file.exists("cache/Coursera-SwiftKey.zip")) {
            download.file(url = "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
                          destfile = "cache/Coursera-SwiftKey.zip", method = "curl")
            unzip("cache/Coursera-SwiftKey.zip", exdir = "cache")
        }        
    }
}
