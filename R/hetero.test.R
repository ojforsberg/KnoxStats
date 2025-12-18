#' @title
#' Heteroskedasticity Test
#'
#' @description
#' This function tests for the presence of heteroskedasticity in a model.
#' 
#' @param e A vector of numbers. These are the residuals from the model.
#' @param x A vector of numbers. These are the values of the independent variable used in the model.
#' @param degree The exponent.
#' 
#' @return A `htest` class variable
#' 
#' @details
#' This function performs a regression-based test for heteroskedasticity by 
#' examining whether the squared residuals (or residuals raised to a specified 
#' power) are correlated with the independent variable. The test is conducted 
#' as follows:
#' 
#' 1. Residuals `e` are raised to the power specified by the `degree` argument 
#'    (default is 2, giving squared residuals: \eqn{e^2}).
#' 2. An auxiliary linear regression is estimated: \eqn{e^degree \sim x}
#' 3. A t-test is performed on the slope coefficient from this regression.
#' 
#' The null hypothesis is that the model errors are homoskedastic (constant 
#' variance). A small p-value (typically < 0.05) provides evidence against the 
#' null hypothesis, suggesting the presence of heteroskedasticity.
#' 
#' The test is similar in spirit to the Breusch-Pagan test but uses a direct 
#' regression approach without scaling by the error variance. The function 
#' returns an object of class `htest`, consistent with R's standard hypothesis 
#' testing functions, containing the test statistic, degrees of freedom, 
#' p-value, and other diagnostic information.
#' 
#' @examples
#' # Generate example data
#' set.seed(123)
#' x <- rnorm(100)
#' y <- 2 + 3*x + rnorm(100, sd = 0.5*abs(x))  # Heteroskedastic errors
#' 
#' # Fit a linear model
#' model <- lm(y ~ x)
#' 
#' # Test for heteroskedasticity using squared residuals (default)
#' hetero.test(residuals(model), x)
#' 
#' # Test using residuals raised to the 4th power
#' hetero.test(residuals(model), x, degree = 4)
#' 
#' @export

hetero.test <- function(e,x, degree=2) { 

METHOD <- "Regression test of homoskedasticity"
eName = deparse(substitute(e))
xName = deparse(substitute(x))

DNAME = paste(eName,", as ordered by ",xName, sep="")

  e2 = e^degree
  n = length(x)

  modTest = lm(e2~x)
  tt=summary(modTest)

  PVAL = coefficients(tt)[2,4]
  STATISTIC = coefficients(tt)[2,3]
  names(STATISTIC) <- "t"

  PARAMETER = n-1
  names(PARAMETER) <- "df"

  res = list(statistic = STATISTIC, parameter = PARAMETER, 
        p.value = PVAL, method = METHOD, data.name = DNAME)

class(res) <- "htest"
res
}