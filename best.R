best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("~/Downloads/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses="character")
  
  outcome <- gsub(" ", ".", outcome)
  regex <- paste0("^Lower\\.Mortality.*", outcome)
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
  
  data[order(data[,colIndex], data[,2])[1],2]
}