#' @title 
#' Value Range Checker
#'
#' @description
#' Determines whether numeric values fall within a specified range, with options
#' for inclusive or exclusive boundary checking. The function is vectorized,
#' allowing it to work efficiently with vectors of any length.
#'
#' @param x A numeric vector or scalar to check against the range boundaries.
#' @param min The lower boundary of the range (default: 0).
#' @param max The upper boundary of the range (default: 1).
#' @param inclusive Logical flag indicating whether the range boundaries should
#'   be inclusive (\code{TRUE}) or exclusive (\code{FALSE}). Default is \code{TRUE}.
#'
#' @return
#' A logical vector of the same length as \code{x}, where \code{TRUE} indicates
#' the corresponding element in \code{x} is within the specified range, and
#' \code{FALSE} indicates it is outside the range.
#'
#' @details
#' The function performs element-wise range checking. When \code{inclusive = TRUE}
#' (default), the range includes the boundary values (\code{min <= x <= max}).
#' When \code{inclusive = FALSE}, the range excludes the boundary values
#' (\code{min < x < max}).
#'
#' The function is vectorized using \code{Vectorize()}, allowing it to handle
#' vectors of any length efficiently. For single values, it returns a single
#' logical; for vectors, it returns a logical vector with the same length.
#'
#' #' @examples
#' # Basic usage with inclusive boundaries (default)
#' isBetween(0.5)                  # TRUE (0.5 is between 0 and 1)
#' isBetween(c(-1, 0, 0.5, 1, 2))  # FALSE, TRUE, TRUE, TRUE, FALSE
#'
#' # Using exclusive boundaries
#' isBetween(c(0, 0.5, 1), inclusive = FALSE)  # FALSE, TRUE, FALSE
#'
#' # Custom range
#' isBetween(c(10, 15, 20), min = 10, max = 20)  # TRUE, TRUE, TRUE
#' isBetween(c(10, 15, 20), min = 10, max = 20, inclusive = FALSE)  # FALSE, TRUE, FALSE
#'
#' # With NA values
#' isBetween(c(NA, 0.5, NA))  # NA, TRUE, NA
#'
#' # Reversed boundaries (always FALSE)
#' isBetween(5, min = 10, max = 0)  # FALSE
#'
#'
#' @seealso
#' \code{\link{findInterval}} for finding which interval values fall into,
#' \code{\link{cut}} for dividing a numeric vector into intervals,
#' \code{\link{between}} from the \code{dplyr} package for similar functionality,
#' \code{\link{Vectorize}} for understanding how the function is vectorized
#'
#'
#' @export
isBetween <- function(x, min=0, max=1, inclusive=TRUE) {
if(min>=max) {
stop("The minimum value (min) must be less than the maximum value (max).")
}
if(inclusive) {
tf = ( x>= min ) & (x <= max)
} else {
tf = ( x> min ) & (x < max)
}

return( tf )
}

isBetween <- Vectorize(isBetween)
