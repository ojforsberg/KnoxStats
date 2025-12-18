#' @title
#' Concordance Ratio
#'
#' @description
#' `concordanceRatio` calculates the number of concordant and discordant points in bivariate data
#' 
#' @param x A vector or a matrix of numbers.
#' @param y A vector of numbers. This is required if x is a vector. 
#' 
#' @return List of the number of concordant and discordant points and the concordance ratio.
#' 
#' @details
#' This function calculates the concordance ratio for bivariate data, which measures 
#' the proportion of concordant pairs relative to all comparable pairs. 
#' 
#' A pair of points (i, j) is considered **concordant** if:
#' \itemize{
#'   \item \code{x[i] > x[j]} and \code{y[i] > y[j]}, or
#'   \item \code{x[i] < x[j]} and \code{y[i] < y[j]}
#' }
#' 
#' A pair is **discordant** if:
#' \itemize{
#'   \item \code{x[i] > x[j]} and \code{y[i] < y[j]}, or
#'   \item \code{x[i] < x[j]} and \code{y[i] > y[j]}
#' }
#' 
#' Pairs where \code{x[i] == x[j]} or \code{y[i] == y[j]} are neither concordant 
#' nor discordant and are excluded from the calculation.
#' 
#' The **concordance ratio** is calculated as:
#' \deqn{\text{ratio} = \frac{\text{concordant}}{\text{concordant} + \text{discordant}}}
#' 
#' This ratio ranges from 0 (perfect discordance) to 1 (perfect concordance), 
#' with 0.500 indicating no association.
#' 
#' When `x` is a matrix, it should have two columns where the first column 
#' is treated as `x` and the second as `y`, and the `y` parameter is ignored.
#' 
#' @export

concordanceRatio <- function(x, y=NULL) {
if(dim(x)>1) {
y = x[,2]
x = x[,1]
}
if(is.null(y)) {
stop("This is a bivariate function.\nBoth an x and a y vector are required.")
}
 xbar = mean(x)
 ybar = mean(y)
 conc = sum( ((x-xbar)*(y-ybar)) >0 )
 disc = sum( ((x-xbar)*(y-ybar)) <0 )
 concRat = (conc-disc)/(length(x))
return( list( concordant=conc, discordant=disc, ratio=concRat) )
}
