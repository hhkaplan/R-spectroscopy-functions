#This function subsets spectra by specificied left (lower) and right (higher)
#wavelengths (e.g. 3.1 and 3.8 microns). 
#inputDat must have a first column with wavelengths in ascending numeric order
#the remainder of the data will be subset using this column.


SubsetSpectraByWav <- function(inputData,lwav,rwav){
  
  left_vector_ID <- which(inputData[,1] >= lwav)[1]
  right_vector_ID <- which(inputData[,1] >= rwav)[1]
  data_subset <- inputData[left_vector_ID:right_vector_ID,]
  
}