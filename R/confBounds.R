#' @title
#' Confidence Bounds for Regression Lines
#'
#' @description
#' This function calculates the Working-Hotelling confidence bounds for a regression line.
#' 
#' @param mod A model fit using `lm`
#' @param conf.level The level of confidence. Defaults to 0.95
#' @param from The starting x-value for the confidence bounds
#' @param to The ending x-value for the confidence bounds
#' @param smoothness The step size. Larger values make the bounds smoother, but increase calculation time.
#' 
#' @return List consisting of the x- and y-values for the regression line, the upper and lower confidence bounds for each x-value, and the confidence level.
#' 
#' @details
#' This function implements Working-Hotelling confidence bands, which provide a 
#' confidence region for the entire regression line over a specified range of predictor
#' values. Unlike pointwise confidence intervals, these bands guarantee that the entire
#' line lies within the bounds with the specified confidence level.
#'
#' The method is based on the F-distribution and uses the formula:
#' \deqn{\hat{y}(x) \pm \sqrt{2F_{1-\alpha, 2, n-2}} \cdot SE(\hat{y}(x))}
#' where \eqn{SE(\hat{y}(x))} is the standard error of the fitted value at \eqn{x}.
#'
#' The `from` and `to` parameters define the x-range over which the bands are calculated.
#' If not provided, the minimum and maximum observed predictor values are used.
#' The `smoothness` parameter controls the increment between x-values; smaller values
#' produce more detailed bands but require more computation.
#'
#' The returned list contains the following components:
#' \itemize{
#'   \item{x}{The sequence of x-values at which the bounds are calculated}
#'   \item{y}{The predicted values of the regression line at each x}
#'   \item{lcb}{The lower confidence bound at each x}
#'   \item{ucb}{The upper confidence bound at each x}
#'   \item{conf.level}{The confidence level used}
#' }
#'
#' Note that these bands are wider than pointwise confidence intervals because they 
#' account for simultaneous inference across all x-values in the specified range.
#' 
#' @references
#' Wynn, Henry P., and P. Bloomfield. “Simultaneous Confidence Bands 
#' in Regression Analysis.” Journal of the Royal Statistical Society. 
#' Series B (Methodological) 33, no. 2 (1971): 202–217.
#' 
#' Working, Holbrook, and Harold Hotelling. “Applications of the Theory 
#' of Error to the Interpretation of Trends.” Journal of the American 
#' Statistical Association 24, no. 165 (1929): 73–85.
#' 
#' 
#' @export

confBounds <- function(mod, conf.level=0.95, from=NA, to=NA, smoothness=1e4)  {

indepVars = names(mod$model)[-1]
nVars = length(indepVars)
if(nVars>1) {
  stop("This procedure only works in the case of 1 independent variable.")
}

xx=mod$model
x=xx[[2]]
y=xx[[1]]

minX = from; maxX = to
if(is.na(from)) minX = min(x)
if(is.na(to))   maxX = max(x)


## distributional multiplier
n = length(x)
FCV = sqrt( 2*qf(conf.level, df1=2,df2=n-2) )

## fit and standard error
newX = seq(minX,maxX,length=smoothness)
modTemp = lm(y~x)
pr = predict(modTemp, newdata=data.frame(x=newX), se.fit=TRUE)

ucb = pr$fit + FCV * pr$se.fit
lcb = pr$fit - FCV * pr$se.fit

results = list(x=newX, y=pr$fit, ucb=ucb, lcb=lcb, conf.level=conf.level)

return(results)
}

