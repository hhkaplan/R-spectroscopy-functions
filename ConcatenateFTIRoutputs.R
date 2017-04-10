#Define a file path for example to run
#fpath <- "/Users/hannahkaplan/Downloads/CaF2_KnollSampleError_GG1_3_9_14"

ConcatenateFTIRoutputs <- function(fpath) {
  
  #import all CSV files from folder listed in path and add to list
  filenames <- list.files(fpath, pattern="*.CSV", full.names=TRUE)
  ldf <- lapply(filenames, read.csv)
  
  #combine all data into one matrix taking wavenumber info from the first file
  dataMatrix <- ldf[[1]]
  for (i in 2:length(ldf)) {
    
    add <- ldf[[i]]
    dataMatrix <- cbind(dataMatrix, add[2])
    
  }
  
  #Divide reflectance data by 100 to get fraction relative to 1
  dataMatrix[, 2:ncol(dataMatrix)] <- dataMatrix[, 2:ncol(dataMatrix)]/100
  
  #Insert a row of containing wavelength (um and nm)
  dataMatrix <- cbind((10000/dataMatrix[,1]), dataMatrix[, 2:ncol(dataMatrix)])
  
  #Sort data by ascending wavelength
  dataMatrix <- dataMatrix[ nrow(dataMatrix):1, ]
  
  
  #Plot results
  plot.new()
  frame()
  plot(dataMatrix[,1], dataMatrix[,2])
  
  return(dataMatrix)
}
