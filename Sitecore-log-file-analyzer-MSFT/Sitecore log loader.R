library(tidyr)
library(DBI)
source("settings.R")

# get list of files
files <- list.files(path = logpath,
                    pattern = "^log.*\\.txt")

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

#for (i in 1:length(files)) {
for (i in 1:10) {
    # show which file we're working on
    setWinProgressBar(pb = pb,
                      value = i,
                      label = files[i])

    # read the file
    logRawLines = readLines(paste(logpath, "/", files[i], sep = ""), skipNul = TRUE)

    # we only want the line that have a save event in them
    filteredRawLogLines = logRawLines[grep(pattern = "Save item.*master", x = logRawLines)]

    # filter to the lines that contain save events
    filteredLogLines <- as.data.frame(filteredRawLogLines)

    if (nrow(filteredLogLines) == 0) next

    # separate the data into columns

    df <- extract(data = filteredLogLines,
                col = filteredRawLogLines,
                into = c("timestamp", "user", "database", "path", "version"),
                regex = ".* ([[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}) .* AUDIT \\((.+)\\)\\: Save item\\: ([^:]+)\\:([^,]*),.*version: ([^,]*),.*")

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

# split the user column into domain and user
loggedSaves <- separate(data = loggedSaves,
         col = "user",
         into = (c("domain", "user")),
         sep = "\\\\",
         remove = TRUE,
         fill = "left")

dbcon <- dbConnect(RSQLite::SQLite(), "sitecore-logfiles.sqlite")
dbWriteTable(conn = dbcon,
             name = "loggedSaves",
             value = loggedSaves,
             overwrite = TRUE)

dbDisconnect(dbcon)