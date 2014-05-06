rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("~/Downloads/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses="character")
  
  outcome <- gsub(" ", ".", outcome)
  regex <- paste0("^Hospital\\.30\\.Day\\.Death.*", outcome)
  colIndex = grep(regex, names(data), ignore.case=T)
  if (length(colIndex) == 0)
  {
    stop("invalid outcome")
  }
  data <- data[!grepl("Not Available", data[,colIndex]),]
  data[,colIndex] <- as.numeric(data[,colIndex])
  data <- data[,c(7, 2, colIndex)]
  
  data <- split(data, data[,1])
  data <- lapply(data, function(x) x[order(x[,3], x[,2]),])
  
  data <- lapply(data, function(x) {
    if (num == "best")
      num <- 1
    else if (num == "worst")
      num <- nrow(x)
    else if (num > nrow(x))
      return (NA)
    
    return(x[num, 2])
  }
  )
  data <- unlist(data)
  data <- cbind(data, names(data))
  data <- as.data.frame(data)
  names(data) <- c("hospital", "state")
  return(data)
}