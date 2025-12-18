#' @title
#' Bar Chart
#'
#' @description
#' `barchart` creates a barchart for both tabulated and raw data
#'
#' 
#' @param x Vector or Table
#' @param ... Additional parameters sent to `barplot`
#' 
#' @return The function creates a bar chart of the given data
#' 
#' @details
#' This function is a wrapper around \code{\link[graphics]{barplot}} that simplifies 
#' the creation of bar charts from both raw and tabulated data. When \code{x} is 
#' a vector of raw (untabulated) values, the function automatically computes 
#' frequency counts before plotting. When \code{x} is already a table or matrix, 
#' it passes the data directly to \code{barplot}.
#' 
#' For additional customization (colors, spacing, axis labels, etc.), use the 
#' \code{...} argument to pass parameters to the underlying \code{barplot} function.
#' 
#' @seealso
#' \code{\link[graphics]{barplot}} for the underlying plotting function and its 
#' full set of parameters.
#' 
#' @examples
#' # From raw data
#' barchart(c("A", "B", "A", "C", "B", "B"))
#' 
#' # From a table
#' tab <- table(sample(letters[1:4], 100, replace = TRUE))
#' barchart(tab, col = "steelblue", main = "Sample Bar Chart")
#' 
#' # With additional barplot arguments
#' barchart(tab, horiz = TRUE, border = "darkred", las = 1)
#' 
#'
#' @export
barchart <- function(x, ...) {
if( is.table(x) ) { 
  graphics:::barplot(x, ...)
} else {
 t = table(x)
 graphics:::barplot(t, ...)
}
}
