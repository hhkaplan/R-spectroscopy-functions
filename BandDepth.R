#This function takes reflectance data, usually already continuum removed, and finds
#band depth by subtracting reflectance values from 1. Band Depth at a
#specific wavelength can be found searching the BD output:

BandDepth <- function(inputData){
  
  BD <- 1 - inputData[,2:ncol(inputData)]
  BD <- cbind(inputData[,1], BD)

}