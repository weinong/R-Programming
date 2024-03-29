complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  result = data.frame()
  for (i in id)
  {
    fileName <- sprintf("%s/%03d.csv", directory, i)
    # print(paste("Opening", fileName))
    csv = read.csv(fileName)
    s <- sum(!is.na(csv[,"sulfate"]) & !is.na(csv[,"nitrate"]))
    result <- rbind(result, c(i, s))
  }
  names(result) <- c("id", "nobs")
  return(result)
}