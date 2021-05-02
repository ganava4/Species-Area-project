# Using gnlm package
# Swihart B and Lindsey J (2019). gnlm: Generalized Nonlinear Regression Models. R
#                                 package version 1.1.1. 
#                                 https://CRAN.R-project.org/package=gnlm
library(gnlm)
# mu defines the expression involving the predictor and the location parameters 
#    (k=p[1], z=p[2]) 
mu <- function(p) p[1]*a^p[2]
# Fitting a generalized non linear model, accounting for overdispersion: 
# negative binomial error. A shape parameter must be added to the gnlr function
# pmu= initial estimate of the shape parameter
# glnr estimates, in addition to k and z, the shape parameter. Thus, Npar = 3
galap.nb.gnlr <- gnlr(s, dist="negative binomial", mu=mu, pmu=c(28,0.33), pshape=0)
galap.nb.gnlr
# It seems that the aic function from the gnlm package has omitted the factor 2
AIC.nb.gnlr <- 2*galap.nb.gnlr$aic
# Are there 3 estimated parameters in the model? k, z, shape, i.e. Npar=3?
AICC.nb.gnlr <- AIC.nb.gnlr + 24/(ntotal-2)       # AICc = AIC + 2*Npar*(Npar + 1)/(ntotal - Npar + 1)
# Setting up a set of x values (areas) for curves plotting
x.area <- seq(0,max(galap$a),0.1)
# Computing fitted richness. Model: power function with negative binomial error
sest.nb.gnlr <- galap.nb.gnlr$coefficients[1]*x.area^galap.nb.gnlr$coefficients[2]
# Adding the curves
lines(x.area,sest.nb.gnlr, col="red",lty="solid",lwd=2)
legend("topright",c("Gaussian","Quasi-Poisson","Negbin"),
       title="Error distribution",lwd=2,col=c("dodgerblue2","green","red")) # Legend
title(main="Species-area relation\nPower function fitted to the Galapagos data")
detach(galap)
