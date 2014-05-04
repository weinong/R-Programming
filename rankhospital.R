rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("~/Downloads/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses="character")
  
  outcome <- gsub(" ", ".", outcome)
  regex <- paste0("^Hospital\\.30\\.Day\\.Death.*", outcome)
  colIndex = grep(regex, names(data), ignore.case=T)
  if (length(colIndex) == 0)
  {
    stop("invalid outcome")
  }
  data <- data[grepl(state, data[,7], ignore.case=T),]
  if (nrow(data) == 0)
  {
    stop("invalid state")
  }
  data <- data[!grepl("Not Available", data[,colIndex]),]
  data[,colIndex] <- as.numeric(data[,colIndex])
  
  if (num == "best")
    num <- 1
  else if (num == "worst")
    num <- nrow(data)
  else if (num > nrow(data))
    return(NA)
  else if (is.character(num))
    return(NA)
  
  data[order(data[,colIndex], data[,2])[num],2]
}