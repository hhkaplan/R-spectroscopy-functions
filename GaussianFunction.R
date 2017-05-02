GaussianFunction <- function(a,x){
  
  g_add = 1
  
  for(i in ncol(a))
  
  #add the Gaussian equations
  g = a[i,2]*exp(-(x-a[i,1])^2/(2*a[i,3]^2))
  g_add = g_add + g
  
  
}