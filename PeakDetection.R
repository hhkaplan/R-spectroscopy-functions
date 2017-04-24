
PeakDetection <- function(spec_data, minHeight = 0.001, minDistance = 10, nups = 7){
  
  require("pracma")
  full_list = list()
  
  for (i in 2:ncol(spec_data)){
    
    #find minima using find peaks in pracma package
    bd <- 1-spec_data[,i]
    rxz <- findpeaks(bd, nups = 7, minpeakdistance = minDistance, minpeakheight = minHeight)
    
    #tabulate the data
    l = data.frame("Wavelengths" = spec_data[rxz[,2],1], "BDs" = 1 - spec_data[rxz[,2],i])
    
    name = paste("num",i,sep = '')
    full_list[[name]] <- l
    
    #plot the result for each spectrum
    dev.new()
    plot(spec_data[,1], spec_data[,i], type = 'l')
    points(spec_data[rxz[,2],1], spec_data[rxz[,2],i], col = "red")
    
  }
  
  #Return a list of wavelengths and band depths of the peaks for each spectrum input
  return(full_list)
  
  
  
}