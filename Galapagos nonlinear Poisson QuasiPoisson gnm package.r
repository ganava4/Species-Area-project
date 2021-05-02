# Now we make use of the gnm package. It includes generalized non linear models
# with Poisson and quasi-Poisson errors
library(gnm)
### Creating nonlin function. 
spar <- function(x){
  list(predictors = list(k = 1, z = 1),
       variables = list(substitute(x)),
       term = function(predictors, variables) {
         paste(predictors[1], "*", variables[1], "^", predictors[2], sep = "")
       })
}
class(spar) <- "nonlin"
# Invoking gnm with starting values k=30, z=0.3. Poisson error
galap.poi  <- gnm(s ~ spar(a) - 1, start=c(30,0.3), family = poisson(link=identity), trace=TRUE, data=galap)
summary(galap.poi)
# Setting up a set of x values (areas) for curves plotting
x.area <- seq(0,max(galap$a),0.1)
# Computing fitted richness. Model: power function with Poisson error
# The fitted values are the same as those for the quasi-Poisson model. Therefore,
# the curves will overlap. Plot will be postponed until Quasi-Poisson model
# is fitted.
# lines(x.area,sest.qp, col="green",lty="solid",lwd=2)
# Invoking gnm with starting values k=30, z=0.3. Quasi-Poisson error
galap.qp  <- gnm(s ~ spar(a) - 1, start=c(33,0.3), family = quasipoisson(link=identity), trace=TRUE, data=galap)
summary(galap.qp)
# Computing fitted richness. Model: power function with Quasi-Poisson error
sest.qp <- galap.qp$coefficients[1]*x.area^galap.qp$coefficients[2]
# Adding the quasi-Poisson model
lines(x.area,sest.qp, col="green",lty="solid",lwd=2)
