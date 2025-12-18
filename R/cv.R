#' @title
#' Coefficient of Variation
#'
#' @description
#' This function calculates the coefficient of variation for univariate data.
#' 
#' @param x A vector of numbers
#' @param na.rm What to do with missing values. By default, all missing values are removed.
#' 
#' @return Scalar value of the coefficient of variation.
#' 
#' @details
#' The coefficient of variation (CV) is calculated as the ratio of the standard 
#' deviation to the mean. The formula used is:
#' 
#' \deqn{CV = \frac{s}{\bar{x}}}
#' 
#' Where:
#' \itemize{
#'   \item \eqn{s} is the sample standard deviation
#'   \item \eqn{\bar{x}} is the sample mean
#' }
#' 
#' This function uses \code{sd(x, na.rm = na.rm)} for the standard deviation 
#' calculation and \code{mean(x, na.rm = na.rm)} for the mean calculation. The 
#' CV is unitless and expresses variability relative to the mean, making it 
#' useful for comparing dispersion across datasets with different units or 
#' vastly different means.
#' 
#' Note that the coefficient of variation is only meaningful for ratio-scale 
#' data (where zero represents an absence of the quantity) and when all values 
#' are positive. The function will return a result for negative values, but 
#' interpretation may not be meaningful in such cases.
#' 
#' @examples
#' # Basic usage
#' cv(c(1, 2, 3, 4, 5))
#' 
#' # With missing values
#' cv(c(1, 2, NA, 4, 5), na.rm = TRUE)
#' 
#' # Compare variability between datasets
#' cv_data1 <- cv(c(10, 12, 14, 16, 18))
#' cv_data2 <- cv(c(100, 120, 140, 160, 180))
#' 
#' # cv_data1 and cv_data2 will be equal, showing relative variability is the same
#'
#' 
#' @export

cv <- function(x, na.rm=TRUE) {
 s = sd(x, na.rm=na.rm)
 xbar = mean(x, na.rm=na.rm)
return(s/xbar)
}

