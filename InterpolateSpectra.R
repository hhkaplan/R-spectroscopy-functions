
InterpolateSpectra <- function( wavelengths, datain){
  
  #define original wavelength and wavelengths for interpolation
  wavin <- datain[,1]
  wavout <- data.matrix(wavelengths)
  output <- matrix(0, dim(wavout)[1], dim(datain)[2]-1)
  
  for (i in 2:ncol(datain)) {
    
    #interpolate with the approx function
    specin <- datain[,2]
    specout <- approx(wavin, specin, wavout, method = "linear")
    specout2 <- specout$y
    
    #replace NAs with zero for points outside of the wavelength range
    specout2[is.na(specout2)] <- 0
    
    #add into a larger dataset
    output[,i-1] <- specout2
    
  }
  
  #finalize and return output
  foutput <- cbind(wavout, output)
  return(foutput)
  
  #plot the last example
  plot.new()
  frame()
  plot(wavin, specin, type = 'l', col = 'black')
  points(wavout, specout2, col = 'red')

}