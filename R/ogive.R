#' @title 
#' Create an Ogive (Cumulative Frequency Graph)
#' 
#' @description
#' Creates a data frame for plotting an ogive, which is a cumulative frequency graph.
#' An ogive shows the cumulative relative frequency of data values and is useful
#' for visualizing percentiles and understanding data distributions.
#'
#' @param x A numeric vector of data values. Non-numeric values will be coerced
#'          to numeric if possible.
#'
#' @return A data frame with two columns:
#' \itemize{
#'   \item{\code{x}: The sorted unique values from the input data}
#'   \item{\code{relFreq}: The cumulative relative frequency for each value}
#' }
#' 
#' @details
#' An ogive (pronounced "oh-jive") is a line graph that shows cumulative frequencies
#' or cumulative relative frequencies. It is useful for finding percentiles and
#' understanding the distribution of data. The function calculates the cumulative
#' relative frequency, which represents the proportion of observations less than or
#' equal to each data value.
#' 
#' The ogive is created by:
#' 1. Sorting the data values
#' 2. Calculating the frequency of each unique value
#' 3. Computing the cumulative sum of frequencies
#' 4. Converting to relative frequencies by dividing by the total number of observations
#' 
#' This function prepares the data for plotting; you can create the actual graph
#' using \code{plot()} or \code{ggplot2} functions.
#'
#' @examples
#' # Basic example with small dataset
#' test_scores <- c(65, 72, 78, 82, 82, 88, 90, 92, 95, 100)
#' ogive_data <- ogive(test_scores)
#' ogive_data
#' 
#' # Create the ogive plot
#' plot(ogive_data$x, ogive_data$relFreq, 
#'      type = "o",  # "o" for points connected by lines
#'      main = "Ogive of Test Scores",
#'      xlab = "Test Score",
#'      ylab = "Cumulative Relative Frequency",
#'      col = "blue",
#'      pch = 19)
#' abline(h = 0.5, col = "gray", lty = 2)  # Add median reference line
#' 
#' # Example with repeated values
#' heights <- c(62, 64, 64, 65, 66, 67, 67, 67, 68, 70, 72)
#' ogive(heights)
#' 
#' # Using with ggplot2 (if installed)
#' \dontrun{
#' if(require(ggplot2)) {
#'   ogive_data <- ogive(test_scores)
#'   ggplot(ogive_data, aes(x = x, y = relFreq)) +
#'     geom_line(color = "blue", size = 1) +
#'     geom_point(color = "blue", size = 2) +
#'     labs(title = "Ogive of Test Scores",
#'          x = "Score",
#'          y = "Cumulative Relative Frequency") +
#'     theme_minimal()
#' }
#' }
#'
#' @references
#' Devore, Jay L. 2011. *Probability and Statistics for Engineering and the Sciences.*
#' 8th ed. Boston: Cengage Learning.
#' 
#' 
#' @seealso
#' \code{\link{ecdf}} for the empirical cumulative distribution function,
#' \code{\link{cumsum}} for cumulative sums,
#' \code{\link{table}} for frequency tables,
#' \code{\link{plot}} for basic plotting functions
#'
#' @export
ogive <- function(x) {
  # Input validation
  if (!is.numeric(x)) {
    warning("Coercing input to numeric")
    x <- as.numeric(x)
  }
  
  if (any(is.na(x))) {
    warning("Removing NA values from input")
    x <- na.omit(x)
  }
  
  # Create frequency table and cumulative relative frequencies
  x_freq <- table(x)
  x_sorted <- sort(unique(x))
  cum_freq <- cumsum(x_freq)
  rel_freq <- cum_freq / length(x)
  
  return(data.frame(x = x_sorted, relFreq = rel_freq))
}
