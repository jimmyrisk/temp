
gpFit <- function(formula,data,covtype="gauss",nugget.estim=TRUE,trend=FALSE){
  require(dummies)
  require(DiceKriging)
  data <- na.omit(data)
  warn <- options()$warn; options( warn = -1 ) # always gives warning, turn off
  data <- dummy.data.frame(data)
  options(warn = warn) # return to what it was before
  yName <- all.vars(formula)[1] # get y variable name
  xNames <- all.vars(formula)[-1]
  i <- which(names(data)==yName) # get y variable name index
  X <- subset(data,select=xNames) # pull X data columns
  y <- data[,i] # pull y from data using formula
  if(trend==TRUE){
    fitGP_nug <- km(formula=formula,design=X,response=y,covtype=covtype,nugget.estim=nugget.estim)
  } else{
    fitGP_nug <- km(formula= ~1,design=X,response=y,covtype=covtype,nugget.estim=nugget.estim)
  }
  
  nug <- fitGP_nug@covariance@nugget
  fitGP <- km(formula=fitGP_nug@trend.formula, design = fitGP_nug@X,
              response = fitGP_nug@y, noise.var = rep(nug,fitGP_nug@n),
              coef.trend = fitGP_nug@trend.coef,
              coef.cov = fitGP_nug@covariance@range.val,
              coef.var = fitGP_nug@covariance@sd2,
              covtype = fitGP_nug@covariance@name)
  return(fitGP)
}