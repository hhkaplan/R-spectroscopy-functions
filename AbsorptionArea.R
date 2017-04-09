#This function finds the area of absorptions in spec_data. This assumes
#that spec_data is reflectance data (i.e. stronger absorptions lead to
#weaker reflectance values) and, traditionally, this function should only be performed on
#continuum removed data. There is an additional ability to find absorption
#area for a subset of the input spectrum, which can be defined using two
#wavelengths lwav and rwav. Area is calculated using Matlab's trapz
#function.
AbsorptionArea <- function(inputData, lwav = NULL, rwav = NULL){
  
  require(pracma)
  AUC <- matrix(0, nrow = ncol(inputData)-1)
  
  #If wavelengths are not specificed by function input:  
  if (is.null(lwav) || is.null(rwav)){
    
    #define measurements columns
    wavelength <- inputData[,1]
    spectrum <- inputData[,2:ncol(inputData)]
    BD <- 1 - spectrum
    
    #Calculate area of absorptions
    for (i in 1:ncol(BD)){
      AUC[[i]] <- trapz(wavelength, BD[,i])
    }
    return(AUC)
    
    #If wavelengths are specificed subset data:
  } else {

    wavelength <- inputData[,1]
    lwav_ID <- which(wavelength >= lwav)[1]
    rwav_ID <- which(wavelength >= rwav)[1]
    
    #define measurements columns
    wavelength <- wavelength[lwav_ID:rwav_ID]
    spectrum <- inputData[lwav_ID:rwav_ID,2:ncol(inputData)]
    BD <- 1 - spectrum
    
    #Calculate area of absorptions
    for (i in 1:ncol(BD)){
      AUC[[i]] <- trapz(wavelength, BD[,i])
    }
    return(AUC)
  }
}