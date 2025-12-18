#' @title 
#' Pareto Chart
#'
#' @description
#' Creates a Pareto chart (ordered bar chart) which displays values in descending
#' order of frequency along with a cumulative percentage line.
#' 
#' @param x A numeric vector or a table object containing the data to be plotted.
#' @param ... Additional arguments passed to \code{\link{barplot}}.
#' 
#' @return 
#' A Pareto chart (invisible) and returns the ordered frequencies as a numeric vector.
#' 
#' @details
#' A Pareto chart is a type of chart that contains both bars and a line graph,
#' where individual values are represented in descending order by bars, and
#' the cumulative total is represented by the line. This visualization is based
#' on the Pareto principle (also known as the 80/20 rule), which states that,
#' for many events, roughly 80% of the effects come from 20% of the causes.
#' 
#' The function accepts either:
#' \itemize{
#'   \item{A numeric vector: The function will first create a frequency table}
#'   \item{A table object: The function will use the table directly}
#' }
#' 
#' The chart is created using \code{\link{barplot}} with bars sorted in
#' descending order. No cumulative percentage line is added in this basic
#' implementation, but users can enhance it with additional customization.
#'
#' @examples
#' # Example 1: Basic usage with a vector
#' data <- c("A", "B", "A", "C", "B", "A", "A", "D", "B", "A")
#' paretochart(data)
#' 
#' # Example 2: Using a table object
#' tab <- table(c("Low", "Medium", "High", "Medium", "High", "High"))
#' paretochart(tab)
#' 
#' # Example 3: Customizing the barplot
#' paretochart(data, 
#'             main = "Defect Analysis",
#'             xlab = "Defect Type", 
#'             ylab = "Frequency",
#'             col = "steelblue",
#'             las = 2)
#' 
#' # Example 4: With numeric data
#' numeric_data <- c(1, 2, 1, 3, 2, 1, 1, 4, 2, 1)
#' paretochart(numeric_data)
#'
#' @references
#' \itemize{
#'   \item{Pareto, V. (1906). Manual of Political Economy.}
#'   \item{Juran, J. M. (1951). Quality Control Handbook.}
#'   \item{Wikipedia contributors. (2023). Pareto chart. 
#'         In Wikipedia, The Free Encyclopedia. Retrieved from 
#'         \url{https://en.wikipedia.org/wiki/Pareto_chart}}
#' }
#'
#' @seealso
#' \code{\link{barplot}} for the underlying plotting function,
#' \code{\link{table}} for creating frequency tables,
#' \code{\link{sort}} for ordering the values
#'

#' @export
paretochart <- function(x, ...) {
 if(class(x)=="table") {
  barplot( sort( x, decr=TRUE ), ... )
 } else {
  barplot( sort( table(x), decr=TRUE ), ... )
 }
}