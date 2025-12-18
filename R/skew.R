#' @title 
#' Calculate Sample Skewness
#' 
#' @description
#' Computes the sample skewness coefficient (also called Fisher-Pearson 
#' standardized third moment) for a numeric vector. Skewness measures the 
#' asymmetry of a probability distribution.
#' 
#' @param x A numeric vector containing the data values. Missing values 
#'   (`NA`) will cause an error.
#' 
#' @return 
#' A single numeric value representing the sample skewness coefficient.
#'   - Positive values indicate right-skewed (positively skewed) distributions
#'   - Negative values indicate left-skewed (negatively skewed) distributions
#'   - Values close to zero indicate approximately symmetric distributions
#' 
#' @details
#' This function calculates the unbiased estimator of population skewness using 
#' the formula:
#' 
#' \deqn{g_1 = \frac{m_3}{m_2^{3/2}} \cdot \frac{\sqrt{n(n-1)}}{n-2}}
#' 
#' where \eqn{m_2} and \eqn{m_3} are the second and third sample moments about 
#' the mean, and \eqn{n} is the sample size.
#' 
#' The adjustment factor \eqn{\sqrt{n(n-1)}/(n-2)} provides an unbiased estimate
#' for normally distributed populations (Joanes and Gill 1998).
#' 
#' **Interpretation guidelines:**
#' - Values between -0.5 and 0.5: approximately symmetric
#' - Values between -1 and -0.5 or 0.5 and 1: moderately skewed  
#' - Values < -1 or > 1: highly skewed
#' 
#' **Common examples of skewness:**
#' - Right-skewed (positive): income distributions, house prices
#' - Left-skewed (negative): exam scores (when there's a maximum score)
#' - Approximately symmetric: heights, weights
#' 
#' @examples
#' # Example 1: Symmetric distribution (skewness ≈ 0)
#' symmetric_data <- c(1, 2, 3, 4, 5, 4, 3, 2, 1)
#' skew(symmetric_data)
#' 
#' # Example 2: Right-skewed distribution (positive skewness)
#' right_skewed <- c(1, 1, 1, 2, 2, 3, 4, 5, 10, 20)
#' skew(right_skewed)
#' 
#' # Example 3: Left-skewed distribution (negative skewness)
#' left_skewed <- c(20, 10, 5, 4, 3, 2, 2, 1, 1, 1)
#' skew(left_skewed)
#' 
#' # Example 4: Compare with built-in functions from other packages
#' # Note: Different packages may use slightly different formulas
#' if (requireNamespace("e1071", quietly = TRUE)) {
#'   library(e1071)
#'   skewness(symmetric_data, type = 2)  # G1 calculation
#' }
#' 
#' # Example 5: Visualize skewness with a histogram
#' set.seed(123)
#' normal_data <- rnorm(1000, mean = 50, sd = 10)
#' hist(normal_data, main = paste("Skewness =", round(skew(normal_data), 3)))
#' 
#' # Example 6: Real-world skewed data
#' # Simulated household income data (typically right-skewed)
#' income_data <- c(
#'   rep(30000, 20), rep(45000, 15), rep(60000, 10),
#'   rep(80000, 8), rep(100000, 5), rep(150000, 2)
#' )
#' skew(income_data)
#' hist(income_data, main = paste("Income Skewness =", 
#'                                round(skew(income_data), 3)))
#' 
#' 
#' @seealso
#' For related functions and concepts:
#' - \code{\link{mean}}, \code{\link{sd}} for other descriptive statistics
#' - \code{\link{kurtosis}} (in KnoxStats package) for measuring "tailedness"
#' - \code{\link{hist}} for visualizing distributions
#' 
#' @importFrom stats sd
#' 
#' @export

skew <- function(x) {
  # Input validation
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector")
  }
  
  if (length(x) < 3) {
    stop("At least 3 observations are required to calculate skewness")
  }
  
  if (any(is.na(x))) {
    stop("Input contains missing values (NA). Remove or impute them first.")
  }
  
  n <- length(x)
  xbar <- mean(x)
  
  # Calculate second and third moments about the mean
  deviations <- x - xbar
  m2 <- sum(deviations^2) / n
  m3 <- sum(deviations^3) / n
  
  # Avoid division by zero for constant data
  if (m2 == 0) {
    return(0)  # Zero variance implies zero skewness
  }
  
  # Calculate skewness with bias correction
  g1 <- m3 / (m2^(3/2))
  
  # Apply small-sample bias correction (returns NA for n < 3, but we already checked)
  bias_corrected <- g1 * sqrt(n * (n - 1)) / (n - 2)
  
  return(bias_corrected)
}
