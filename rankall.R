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
}