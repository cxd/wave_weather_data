
# process list of files and assign colnames
processFileList <- function(files, names, skipLines=0, sepr=",") {
  result <- lapply(files, function(file) {
    data <- read.csv(file, header=TRUE, sep=sepr, skip=skipLines)
    colnames(data) <- names
    return (data)
  })
  return(result)
}

# read all files into a collection of data frames
processDir <- function(dir, skipLines=0) {
  files <- dir(dir, recursive=TRUE, full.name=TRUE, pattern="\\.csv$")
  result <- lapply(files, function(file) {
    read.csv(file, header=TRUE, sep=",", skip=skipLines)
  })
  return(result)
}
# merge a list of data frames into a single data set
mergeFiles <- function(dataFrames) {
  datalist <- do.call("rbind", dataFrames)
  return (datalist)
}