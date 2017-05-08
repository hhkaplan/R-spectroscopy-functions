# Endmembers <- read.csv('/Users/hannahkaplan/Desktop/EndmemberSSA.csv')
# Data <- read.csv('/Users/hannahkaplan/Desktop/GrSSA.csv')

LinearLSFitting <- function(Data,Endmembers,Weights = NULL, AddToOne = FALSE){
  
  require("pracma")
  
  #Define columns
  EndWav <- Endmembers[,1]
  EndSpec <- Endmembers[,2:ncol(Endmembers)]
  DatWav <- Data[,1]
  DatSpec <- Data[,2:ncol(Data)]
  
  #Make sure wavelengths are identical
  if (!all(EndWav == DatWav)){
    print("Wavelengths do not match")
  }
  
  #Set weights to 1s if none are given
  if (is.null(Weights)){
    Weights <- rep(1, length(EndWav))
  }
  
  #Define bounds
  lb = rep(0, ncol(EndSpec))
  ub = rep(1, ncol(EndSpec))
  Aeq = rep(1, ncol(EndSpec))
  beq = 1
  results = list()
  
  #Solve the linear least squares problem using lsqlincon
  for (col in 1:ncol(DatSpec)){
    Spec <- DatSpec[,col]
    if (AddToOne){
      res <- lsqlincon(Weights*as.matrix(EndSpec), Weights*Spec,  A = NULL, b = NULL, Aeq, beq, lb,  ub) 
    }else{
      res <- lsqlincon(Weights*as.matrix(EndSpec), Weights*Spec,  A = NULL, b = NULL, Aeq = NULL, beq = NULL, lb,  ub)
    }
    results[col] <- res
   
    #Plot results
    plot.new()
    plot(DatWav, Spec, type = "l", col ="black") 
    lines(DatWav, as.matrix(EndSpec)%*%res, type = "l", col = "red")
    legend("topright", c("Measured","Modeled"))
    
    
  }
  
  return(results)
}
  
