library(tidyr)

# change this to the actual location of the log files
setwd("C:/temp/ssc1pwa3 logs")

# get list of files
files <- list.files(pattern = "^log.*\\.txt")

# create progress bar
pb <- winProgressBar(title = "test progress bar",
                     label = "Some information in %",
                     min = 0,
                     max = length(files),
                     initial = 0)

# create data frame for storing data to analyze
loggedSaves <- data.frame(matrix(ncol = 5, nrow = 0))
columnNames <- c("timestamp", "user", "database", "path", "version")
colnames(loggedSaves) <- columnNames

for (i in 1:length(files)) {
#for (i in 1:10) {
    # show which file we're working on
    setWinProgressBar(pb = pb,
                      value = i,
                      label = files[i])

    # read the file
    logRawLines = readLines(files[i], skipNul = TRUE)

    # we only want the line that have a save event in them
    filteredRawLogLines = logRawLines[grep(pattern = "Save item.*master", x = logRawLines)]

    # filter to the lines that contain save events
    filteredLogLines <- as.data.frame(filteredRawLogLines)

    if (nrow(filteredLogLines) == 0) next

    # separate the data into columns

    df <- extract(data = filteredLogLines,
                col = filteredRawLogLines,
                into = c("timestamp", "user", "database", "path", "version"),
                regex = ".* ([:digit:][:digit:]:[:digit:][:digit:]:[:digit:][:digit:]) .* AUDIT \\((.+)\\)\\: Save item\\: ([^:]+)\\:([^,]*),.*version: ([^,]*),.*")

    # get date of the file based on the file name
    dateOfLogFile = gsub("log\\.([[:digit:]]{8}).*$",
                         "\\1",
                         x = files[i])

    # convert time column to actual time value
    df$timestamp <- strptime(x = paste(dateOfLogFile, df$timestamp, sep = " "),
                       "%Y%m%d %H:%M:%S",
                       tz = Sys.timezone())

    loggedSaves <- rbind(loggedSaves, df)
}

# close the status dialog
close(con = pb)

# figured out what are repeated saves on the same version by the same person
# this will reduce inconsequential edits, although it will miss edits to
# items without versions, like widgets
# main point is to eliminate editors whose behaviors are chatty in the log files
loggedSaves$repeatedEdit <- duplicated(x = loggedSaves[c("user", "path", "version")])

# do some analysis
library(ggplot2)
ggplot(data = loggedSaves[loggedSaves$repeatedEdit != TRUE,], aes(loggedSaves[loggedSaves$repeatedEdit != TRUE,]$timestamp)) +
    geom_histogram()

# just form edits
#formEdits <- 