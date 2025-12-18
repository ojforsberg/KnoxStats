#' @title
#' Autocorrelation Test
#'
#' @description
#' `autocor.test` calculates the correlation between one element in the vector and the next.
#'
#' 
#' @param x Numeric vector
#' @param ... Additional parameters sent to `cor.test`
#' 
#' @return The function returns the results of a correlation test as a class `htest`.
#' 
#' @details
#' This function performs an autocorrelation test at lag 1 by calculating the 
#' correlation between each element in the vector and its immediate successor. 
#' Internally, it creates two vectors: one containing all elements except the last, 
#' and another containing all elements except the first. These vectors are then 
#' passed to `stats::cor.test()` for hypothesis testing.
#' 
#' The test assesses whether there is significant serial correlation in the data,
#' which can be important for time series analysis and checking independence 
#' assumptions in statistical models. The null hypothesis is that the true 
#' autocorrelation at lag 1 equals zero (no serial correlation).
#' 
#' Note that this implementation specifically tests lag-1 autocorrelation. For
#' testing autocorrelation at different lags or multiple lags simultaneously,
#' consider using `stats::acf()` or other time series-specific functions.
#' 
#' @examples
#' x = rnorm(100)
#' autocor.test(x)
#' 
#' y = seq(1,100)
#' autocor.test(y)
#' #'
#' @export
autocor.test <- function(x, na.rm=TRUE, ...) {
  y = as.numeric(x)
  n = length(y)
  if(n<1) { stop("Input vector must be numeric.") }
  s1 = y[-1]
  s2 = y[-n]
return(stats:::cor.test(s1,s2, na.rm=na.rm, ...))
}

