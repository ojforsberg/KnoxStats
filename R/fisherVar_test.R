#' @title
#' Fisher's Variance Test
#'
#' @description
#' Tests for difference in variances between two numeric vectors
#' 
#' @param x Either a numeric vector or a numeric nx2 matrix.
#' @param y A numeric vector, required if x is a vector.
#' 
#' @return A `htest` object
#' 
#' @details
#' This function performs Fisher's variance test by applying the Wilcoxon rank-sum test
#' to the absolute deviations from the median. The test is robust to non-normal data
#' and outliers compared to traditional variance tests like the F-test.
#' 
#' The function first computes the absolute deviations of each vector from its median:
#' \eqn{X' = |x - \text{median}(x)|} and \eqn{Y' = |y - \text{median}(y)|}.
#' It then performs a Wilcoxon rank-sum test on \eqn{X'} and \eqn{Y'} to determine if
#' their distributions differ, which indicates a difference in variances between the
#' original vectors.
#' 
#' The function accepts input in two formats:
#' \itemize{
#'   \item Two separate numeric vectors: `x` and `y`
#'   \item A single nx2 matrix where the first column becomes `x` and the second column becomes `y`
#' }
#' 
#' @examples
#' # Using separate vectors
#' x <- rnorm(30, mean = 0, sd = 1)
#' y <- rnorm(25, mean = 0, sd = 2)
#' fisher.var.test(x, y)
#' 
#' # Using a matrix
#' data <- cbind(x, y)
#' fisher.var.test(data)
#' 
#' @seealso
#' [wilcox.test()] for the underlying test, [var.test()] for the traditional F-test
#' for equality of variances
#' 
#' @references
#' Fisher, R.A. (1925) Statistical Methods for Research Workers. Edinburgh: Oliver and Boyd.
#' 
#' 
#' @export

fisher.var.test <- function(x, y=NULL) { 
if(dim(x)>1.5) {
y = x[,2]
x = x[,1]
}
  X = abs(x-median(x))
  Y = abs(y-median(y))
  wilcox.test(X,Y)
}