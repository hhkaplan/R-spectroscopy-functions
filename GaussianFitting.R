#Inputs
Data <- cont$cont_rem
Wavelengths = c(3.31, 3.38, 3.42, 3.45, 3.48, 3.50)
maxWidth = 0.03
maxShift = 0.001
source("GaussianFunction.R")

#Require necessary library
require('pracma')

#Create input matrices
Amplitudes = rep(-0.5, length(Wavelengths)) 
Widths = maxWidth*rep(1, length(Wavelengths)) 
a0 = cbind(Wavelengths, Amplitudes, Widths)

#Set bounds
lowerbound = cbind(Wavelengths-maxShift,rep(-1, length(Wavelengths)), maxWidth*rep(1, length(Wavelengths)))
upperbound = cbind(Wavelengths+maxShift,rep(0, length(Wavelengths)), rep(0, length(Wavelengths)))

#Data to be fit
xdata = Data[,1]
ydata = Data[,2]

#Set options
options= list('maxeval',100000, 'tau',1e-5)

#Fitting routine
res = nls
#res = lsqcurvefit(fun=GaussianFunction, a0, xdata, ydata, lowerbound, upperbound, options)

