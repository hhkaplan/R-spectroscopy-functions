#Require necessary library
require('pracma')
require('minpack.lm')

#Inputs
Data <- cont$cont_rem
Wavelengths = c(3.31, 3.35,3.38, 3.42,3.45, 3.48, 3.50)
maxWidth = 0.02
maxShift = 0.001
func = "LorentzianFunction"


#Define Gaussian Function
GaussianFunction <- function(a,x){
  g_add = 1
  for(i in 1:size(a)[1]){
    #add the Gaussian equations
    g = a[i,2]*exp(-(x-a[i,1])^2/(2*a[i,3]^2))
    g_add = g_add + g
  }
  return(g_add)
}
#Define Lorentzian Function
LorentzianFunction <- function(a,x){
  l_add = 1
  for(i in 1:size(a)[1]){
    l =a[2,i]*(a[3,i]^2/((x-a[1,i])^2+a[3,i]^2))
    l_add = l_add + l
  }
  return(l_add)
}
#Define PseudoVoigt Function
PseudoVoigtFunction <- function(a,x){
  PV_add = 1
  x0 = a[1,]
  amp = a[2,]
  FWHM = a[3,]
  n = a[4,]
  for(i in 1:size(a)[1]){
    Lorentz = amp[i]*(FWHM[i]^2./((x-x0[i])^2+FWHM[i]^2))
    Gauss = amp[i]*exp(-(x-x0[i])^2/(2*(FWHM[i]/2.355)^2))
    PV = (1-n[i])*Gauss + n[i]*Lorentz
    PV_add = PV_add + PV
  }
  return(PV_add)
}
#Define skew gaussian function
SkewGaussFunction <- function(a,x){
  SG_add = 1
  for(i in 1:size(a)[1]){
    pdf_g = a[2, i]*exp(-(x-a[1,i])^2/(2*a[3,i]^2))
    cdf_g = .5*(1+erf(a[4,i]*(x-a[1,i])/(sqrt(2)*a[3,i]))) 
    skew_pdf_g = 2*pdf_g*cdf_g
    SG_add = SG_add + skew_pdf_g
  }
  return(SG_add)
}

myfunc = as.name(func)

#Create input matrices
Amplitudes = rep(-0.5, length(Wavelengths)) 
Widths = (maxWidth/2)*rep(1, length(Wavelengths)) 
Ns = rep(0.5, length(Wavelengths))

if (func == "SkewGaussFunction" || func == "PseudoVoigtFunction"){
  #Set bounds
  a0 = cbind(Wavelengths, Amplitudes, Widths, Ns)
  lb = cbind(Wavelengths-maxShift,rep(-1, length(Wavelengths)),rep(0, length(Wavelengths)),rep(0, length(Wavelengths)))
  ub = cbind(Wavelengths+maxShift,rep(0, length(Wavelengths)), maxWidth*rep(1, length(Wavelengths)),rep(1, length(Wavelengths)))
}else{
  #Set bounds
  a0 = cbind(Wavelengths, Amplitudes, Widths)
  lb = cbind(Wavelengths-maxShift,rep(-1, length(Wavelengths)),rep(0, length(Wavelengths)))
  ub = cbind(Wavelengths+maxShift,rep(0, length(Wavelengths)), maxWidth*rep(1, length(Wavelengths)))
}
#Data to be fit
xdata = Data[,1]
ydata = Data[,2]
ydata[is.na(ydata)] <- 0

#Minimization function
residF <-function(a,x,y){
  return(y - myfunc(a,x))
}

#Fitting routine
a = nls.lm(par = a0, lower = lb, upper = ub, fn = residF, x = xdata, y = ydata)

#Plot ther results
plot(xdata,ydata,type = "l", col = "black")
lines(xdata, myfunc(a$par,xdata), col = "red")
for(i in 1:size(a$par)[1]){
  lines(xdata, myfunc(t(matrix(a$par[i,])),xdata), col = "blue")
}
