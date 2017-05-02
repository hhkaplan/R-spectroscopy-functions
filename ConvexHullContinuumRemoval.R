# inputData <- dataMatrix 
# lwav <- 3.1
# rwav <- 3.8

ConvexHullContinuumRemoval <- function(inputData,lwav,rwav){
  #require the necessary packages to run the function
  require("grDevices")
  
  #subset the data based on the wavelength positions given
  ind1 <- min(which(inputData[,1]>lwav))
  ind2 <- min(which(inputData[,1]>rwav))
  subData <- inputData[ind1:ind2,]
  
  #pre allocate space
  cont_rem <- matrix(0, dim(subData)[1], dim(subData)[2]-1)
  cont_fit <- matrix(0, dim(subData)[1], dim(subData)[2]-1)
  
  #Run through each data point
  for (i in 2:dim(subData)[2]){
    
    #Fit a convex hull to the reflectance columns
    cpoints <- chull(subData[,c(1,i)])
    pos <- which(diff(cpoints)>0) #remove the bottom points to get only upper hull
    upperhull <- cpoints[pos]
    upperhull_full <- approx(subData[upperhull,1], subData[upperhull,i], subData[,1], method = "linear")
    cont_fit[,i-1] <- upperhull_full$y 
    
    #Divide out the continuum
    cont_rem[,i-1] <- subData[,i] / cont_fit[,i-1]
    
  }
  
  #finalize matrices to include wavelengths
  cont_rem <- cbind(subData[,1],cont_rem)
  cont_fit <- cbind(subData[,1],cont_fit)

  
  #Plot the results
  plot.new()
  frame()
  par(mfrow=c(2,1))
  plot(subData[,1], subData[,2], col = "black")
  lines(cont_fit[,1], cont_fit[,2], col = "red")
  plot(subData[,1], subData[,3], col = "black")
  lines(cont_fit[,1], cont_fit[,3], col = "red")
  
  #return both cont_rem and cont_fits labeled in a list
  return(list(cont_rem = cont_rem,cont_fit = cont_fit))
}