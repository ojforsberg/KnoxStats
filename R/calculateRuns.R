#' @title
#' Runs Calculations
#'
#' @description
#' `calculateRuns` calculates the number of runs (above or below 0) in an ordered vector
#'
#' 
#' @param r A vector of numbers
#' 
#' @return Scalar of the number of runs in the vector `r`
#' 
#' @details
#' A "run" is defined as a consecutive sequence of values that are all 
#' either strictly positive (greater than 0) or strictly negative (less than 0). 
#' Values exactly equal to 0 are treated as breaking runs, meaning they reset 
#' the run count. The function calculates the total number of these runs in the 
#' input vector, counting from the beginning to the end of the sequence.
#' 
#' This implementation follows the standard runs test methodology where:
#' \itemize{
#'   \item Each change from positive to negative (or vice versa) increments the run count
#'   \item Zero values create breaks between runs
#'   \item The minimum number of runs is 1 (if all values have the same sign and no zeros)
#'   \item The maximum number of runs is \code{n} (if values alternate sign frequently)
#' }
#' 
#' Note: The function expects ordered data (typically time series) as input since 
#' runs are calculated based on the given sequence order.
#'
#'
#' @export

calculateRuns <- function(r) {
 n = length(r)
 x = sign(r)
 y = x[-n]-x[-1]
 runs = 1+sum(y!=0)
return(runs)
}

