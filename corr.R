corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  source("complete.R")
  completed <- complete(directory)
  completed <- cbind(completed, completed[,"nobs"] > threshold)
  result = as.numeric(NULL)
  for (row in seq_len(nrow(completed)))
  {
    if (completed[row,][[3]] == FALSE)
    {
      next;
    }
    
    file <- sprintf("%s/%03d.csv", directory, completed[row,][[1]])
    # print(paste("Opening", file))
    csv = read.csv(file)
    c <- !is.na(csv[,"sulfate"]) & !is.na(csv[,"nitrate"])
    good <- csv[c,]
    result <- c(result, cor(good[,"sulfate"], good[,"nitrate"]))
  }
  return(result)
}