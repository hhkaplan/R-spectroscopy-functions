
SpliceSpectra <- function(short_data,long_data, splice_wav){
  
  #Define the wavelength column and find the vectorID of the splice
  #wavelength within both short and long wavelength data
  short_wav <- short_data[,1]
  ind1 <- which(short_wav >= splice_wav)[1]
  long_wav <- long_data[,1]
  ind2 <- which(long_wav >= splice_wav)[1]
  
  #Trim data > splice point wavelength for shorter wavelength data and
  #<splice point for longer wavelength data
  short_data <- short_data[1:ind1,]
  long_data <- long_data[ind2:nrow(long_data),]
  
  #Determine the appropriate scale factor for each spectrum (column).
  #Scale the longer wavelength (usually FTIR) data to match the shorter(usually
  #ASD) data.
  scale_factor <- short_data[nrow(short_data),2:ncol(short_data)]/long_data[1,2:ncol(long_data)]
  n_scale_factor <-  scale_factor[rep(seq_len(nrow(scale_factor)), nrow(long_data)), ]
  
  #Multiply the long wavelength data by the scale factors
  long_data_scaled <- long_data[,2:ncol(long_data)]*n_scale_factor
  long_data_scaled <- cbind(long_data[,1],long_data_scaled)
  
  #Merge the short and scaled data into a single matrix and do the same
  #for their corresponding wavelength and wavenumber vectors.
  spliced_spectra <- mapply(c,short_data, long_data_scaled)
  spliced_spectra.df <- as.data.frame(spliced_spectra)
  
  
  #plot the results
  plot.new()
  frame()
  plot(spliced_spectra[,1], spliced_spectra[,2], col = "black", type = "l",xlab="Wavelength", ylab="Reflectance")
  lines(short_data[,1], short_data[,2], col = "red")
  lines(long_data[,1], long_data[,2], col ="blue")
  legend('topright',c("Spliced", "Short Wavelengths", "Long Wavelengths"),lty = c(1,1,1),
         col=c('black','red','blue'),bty ="n")
  return(spliced_spectra.df)
}