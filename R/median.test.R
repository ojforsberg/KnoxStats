#' @title 
#' Median Test for One Sample
#' 
#' @description
#' Performs a nonparametric sign test for the median of a single sample. This 
#' test determines whether the population median differs from a specified value 
#' without assuming the data follows a normal distribution.
#' 
#' @param x A numeric vector of data values. Missing values (`NA`) are removed.
#' @param mu A single number indicating the hypothesized population median. 
#'   Default is 0.
#' @param conf.level Confidence level for the test. Currently not implemented 
#'   in this version but included for consistency with other hypothesis tests.
#'   
#' @return 
#' An object of class `"htest"` containing the following components:
#' \item{statistic}{The test statistic M (minimum of counts above/below mu)}
#' \item{p.value}{The p-value for the test}
#' \item{null.value}{The hypothesized median (`mu`)}
#' \item{alternative}{A character string describing the alternative hypothesis}
#' \item{method}{The name of the test method}
#' \item{data.name}{A character string giving the name(s) of the data}
#' 
#' @details
#' The median test (also called the sign test) is a nonparametric alternative 
#' to the one-sample t-test. It works by counting how many observations are 
#' above and below the hypothesized median. The test is particularly useful when:
#' \itemize{
#'   \item{Data are not normally distributed}
#'   \item{The sample size is small}
#'   \item{Data contain outliers}
#'   \item{Data are measured on an ordinal scale}
#' }
#' 
#' The test statistic M is the smaller of the number of observations above or 
#' below the hypothesized median. Observations exactly equal to the hypothesized 
#' median are excluded from the calculation. The p-value is calculated using 
#' the binomial distribution with probability of success 0.500 under the null 
#' hypothesis.
#' 
#' @examples
#' # Example 1: Basic usage with default mu = 0
#' set.seed(123)
#' data <- rnorm(20, mean = 5, sd = 2)
#' median.test(data)
#' 
#' # Example 2: Test against a specific median
#' # Test if the median of student exam scores is 75
#' exam_scores <- c(68, 72, 75, 78, 80, 82, 85, 88, 90, 92)
#' median.test(exam_scores, mu = 75)
#' 
#' # Example 3: Real-world example - test reaction times
#' # Test if median reaction time is 250 milliseconds
#' reaction_times <- c(230, 245, 250, 255, 260, 265, 270, 280, 290, 300, 310)
#' median.test(reaction_times, mu = 250)
#' 
#' # Example 4: Data with missing values (NA)
#' data_with_na <- c(10, 15, NA, 20, 25, NA, 30)
#' median.test(data_with_na, mu = 20)
#' 
#' # Example 5: Visualize the test
#' test_result <- median.test(exam_scores, mu = 75)
#' print(test_result)
#' 
#' # To extract specific components:
#' test_result$p.value
#' test_result$statistic
#' 
#' @references
#' Gibbons, Jean Dickinson, and Subhabrata Chakraborti. 2011. 
#' \emph{Nonparametric Statistical Inference}. 5th ed. Boca Raton, FL: 
#' Chapman & Hall/CRC.
#' 
#' Hollander, Myles, Douglas A. Wolfe, and Eric Chicken. 2014. 
#' \emph{Nonparametric Statistical Methods}. 3rd ed. Hoboken, NJ: 
#' John Wiley & Sons.
#' 
#' Siegel, Sidney. 1956. \emph{Nonparametric Statistics for the Behavioral 
#' Sciences}. New York: McGraw-Hill.
#' 
#' @seealso
#' \code{\link{wilcox.test}} for the Wilcoxon signed-rank test (a more powerful 
#'   nonparametric alternative when data are symmetric),
#' \code{\link{t.test}} for the parametric one-sample t-test,
#' \code{\link{pbinom}} for the binomial distribution function used in p-value 
#'   calculation,
#' \code{\link{median}} for calculating the sample median
#' 
#' @export
median.test <- function(x, mu = 0, conf.level = 0.95) {
  
  # Remove missing values with a clear message for students
  if (any(is.na(x))) {
    original_length <- length(x)
    x <- x[!is.na(x)]
    removed <- original_length - length(x)
    if (removed > 0) {
      message(paste("Note:", removed, "missing value(s) removed from the data."))
    }
  }
  
  # Count observations above, below, and equal to hypothesized median
  above <- sum(x > mu)
  below <- sum(x < mu)
  equal <- sum(x == mu)
  
  # Calculate test statistic (minimum of above/below counts)
  test_statistic <- min(above, below)
  
  # Calculate p-value using binomial distribution
  n_effective <- length(x) - equal  # Exclude values exactly equal to mu
  p_one_sided <- pbinom(test_statistic, size = n_effective, prob = 0.5)
  p_value <- 2 * min(p_one_sided, 1 - p_one_sided)
  
  # Create descriptive output
  data_name <- deparse(substitute(x))
  method <- "One-Sample Median Test (Sign Test)"
  
  # Create result object
  results <- list(
    statistic = c(M = test_statistic),
    p.value = p_value,
    null.value = c("population median" = mu),
    alternative = "two.sided",
    method = method,
    data.name = data_name,
    # Include additional helpful information for students
    counts = c(
      above = above,
      below = below,
      equal = equal,
      total = length(x)
    ),
    sample.median = median(x)
  )

# To-do:
  # Add confidence level information (currently not implemented)
  # results$conf.int <- c(NA, NA)
  # attr(results$conf.int, "conf.level") <- conf.level
  
  class(results) <- "htest"
  
return(results)
}
