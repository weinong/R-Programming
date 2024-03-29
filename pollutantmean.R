pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  total = 0;
  count = 0;
  for (i in id)
  {
    fileName <- sprintf("%s/%03d.csv", directory, i)
    # print(paste("Opening", fileName))
    csv = read.csv(fileName)
    c <- sum(!is.na(csv[pollutant]))
    if (c != 0)
    {
      total <- total + sum(csv[pollutant], na.rm=T)
      count <- count + c
    }
  }
  m <- total/count
}