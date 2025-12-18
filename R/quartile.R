#' @title 
#' Calculate Quartiles
#'
#' @description
#' A user-friendly function to calculate quartiles (0th through 4th) for a numeric vector.
#' Quartiles divide a dataset into four equal parts, making them essential for 
#' understanding data distribution in introductory statistics.
#' 
#' @param x A numeric vector of data values. Missing values (`NA`s) are allowed 
#'          but will be removed before calculation.
#' @param q A numeric vector specifying which quartiles to calculate. 
#'          Default is `0:4` (all five quartiles: minimum, Q1, Q2, Q3, maximum).
#'          Must be values between 0 and 4, inclusive.
#'
#' @return 
#' A numeric vector containing the requested quartile values, with quartile numbers
#' as names. The 0th quartile is the minimum, 2nd quartile is the median, 
#' and 4th quartile is the maximum.
#'
#' @details
#' This function provides a simplified interface to R's built-in `quantile()` function
#' specifically for quartile calculations. Quartiles are important measures in 
#' descriptive statistics that help identify:
#' \itemize{
#'   \item{The **minimum** (0th quartile or 0th percentile)}
#'   \item{The **first quartile** (25th percentile) - 25% of data falls below this}
#'   \item{The **median** (50th percentile) - middle value of the dataset}
#'   \item{The **third quartile** (75th percentile) - 75% of data falls below this}
#'   \item{The **maximum** (100th percentile)}
#' }
#' 
#' The function includes error checking to ensure quartile values are valid (0-4)
#' and provides clear error messages if invalid values are provided.
#'
#' @examples
#' # Example 1: Basic usage with default quartiles
#' exam_scores <- c(65, 72, 78, 85, 88, 90, 92, 95, 98, 100)
#' quartile(exam_scores)
#' 
#' # Example 2: Request specific quartiles
#' quartile(exam_scores, q = c(0, 1, 2, 3, 4))  # Same as default
#' quartile(exam_scores, q = c(1, 3))           # Just Q1 and Q3
#' quartile(exam_scores, q = 2)                 # Just the median
#' 
#' # Example 3: Compare with boxplot statistics
#' boxplot.stats(exam_scores)$stats  # Should match quartiles 0, 1, 2, 3, 4
#' 
#' # Example 4: Handle missing values
#' scores_with_na <- c(65, 72, NA, 85, 88, 90, NA, 95, 98, 100)
#' quartile(scores_with_na)  # NAs are automatically removed
#' 
#' # Example 5: Small dataset
#' quartile(c(10, 20, 30, 40, 50))
#' 
#' # Example 6: Using with real data
#' quartile(iris$Sepal.Length)
#' 
#' @section Common Uses in Statistics:
#' Quartiles are frequently used for:
#' \itemize{
#'   \item{Creating boxplots (Tukey 1977)}
#'   \item{Identifying outliers (values outside Q1 - 1.5*IQR or Q3 + 1.5*IQR)}
#'   \item{Understanding data spread through the interquartile range (IQR = Q3 - Q1)}
#'   \item{Comparing distributions between groups}
#' }
#'
#' @references
#' \itemize{
#'   \item{Tukey, John W. 1977. *Exploratory Data Analysis*. Reading, MA: Addison-Wesley.}
#'   \item{Moore, David S., George P. McCabe, and Bruce A. Craig. 2021. *Introduction to the Practice of Statistics*. 10th ed. New York: W. H. Freeman.}
#'   \item{"Quartile." 2023. In *Wikipedia*. https://en.wikipedia.org/wiki/Quartile.}
#' }
#'
#' @seealso
#' \code{\link{quantile}} for general percentile calculations with more options,
#' \code{\link{median}} for calculating just the median,
#' \code{\link{boxplot}} for visualizing quartiles graphically,
#' \code{\link{IQR}} for calculating the interquartile range,
#' \code{\link{fivenum}} for Tukey's five-number summary
#'
#' @importFrom stats quantile
#' @export
quartile <- function(x, q = 0:4) {
  # Input validation
  if (!is.numeric(x)) {
    stop("Input 'x' must be a numeric vector")
  }
  
  # Remove NA values with warning if present
  if (any(is.na(x))) {
    warning("NA values removed from input data")
    x <- x[!is.na(x)]
  }
  
  # Validate quartile requests
  if (any(q < 0)) {
    stop("Specified quartile less than 0.\nPlease use quartiles between 0 and 4, inclusive")
  }
  if (any(q > 4)) {
    stop("Specified quartile greater than 4.\nPlease use quartiles between 0 and 4, inclusive")
  }
  
  # Calculate quartiles
  ptile <- q / 4
  res <- stats::quantile(x, probs = ptile, na.rm = TRUE, names = FALSE)
  
  # Set meaningful names
  names(res) <- paste0("Q", q)
  names(res)[q == 0] <- "Min"
  names(res)[q == 2] <- "Median"
  names(res)[q == 4] <- "Max"
  
  return(res)
}
