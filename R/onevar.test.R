#' @title
#' One-Sample Variance Test (Chi-Square Test)
#'
#' @description
#' Performs a hypothesis test for the variance of a single population using the 
#' chi-square distribution. This test determines whether the population variance 
#' is equal to a specified value, or whether it is less than or greater than that 
#' value. It is particularly useful in quality control and when checking 
#' assumptions for other statistical tests.
#'
#' @param x A numeric vector of sample data. Missing values (`NA`) are not allowed.
#' @param s2 The hypothesized population variance under the null hypothesis. 
#'   Default is 1. Must be a positive number.
#' @param conf.level The confidence level for the confidence interval. 
#'   Must be between 0 and 1. Default is 0.95 (95% confidence).
#' @param alternative The direction of the alternative hypothesis. Must be one of:
#'   `"two.sided"` (default, σ² ≠ s2), `"less"` (σ² < s2), or `"greater"` (σ² > s2).
#'
#' @return
#' A list with class `"htest"` containing the following components:
#' \item{statistic}{The chi-square test statistic (X²)}
#' \item{parameter}{Degrees of freedom (n - 1)}
#' \item{p.value}{The p-value for the test}
#' \item{conf.int}{Confidence interval for the population variance}
#' \item{estimate}{Sample variance}
#' \item{null.value}{The hypothesized population variance}
#' \item{alternative}{Character string describing the alternative hypothesis}
#' \item{method}{Character string indicating the test performed}
#' \item{data.name}{Character string giving the name of the data}
#'
#' @details
#' This function performs a chi-square test for a single population variance.
#' The test assumes that the sample comes from a normally distributed population.
#' The test statistic follows a chi-square distribution with `n - 1` degrees of 
#' freedom, where `n` is the sample size.
#'
#' The test statistic is calculated as:
#' X² = (n - 1) * s² / σ²
#' where s² is the sample variance and σ² is the hypothesized population variance.
#'
#' Confidence intervals are constructed using the chi-square distribution:
#' • Lower bound = (n - 1) * s² / χ²(1 - α/2)
#' • Upper bound = (n - 1) * s² / χ²(α/2)
#' for two-sided tests, with appropriate adjustments for one-sided alternatives.
#'
#' @section Assumptions:
#' 1. The sample is randomly selected from the population.
#' 2. The population follows a normal distribution.
#' 3. Observations are independent of each other.
#'
#' @section Warning:
#' This test is very sensitive to violations of the normality assumption. If the data
#' are not normally distributed, the results will  be unreliable. Consider using
#' nonparametric alternatives or transformations if normality is questionable.
#'
#' @examples
#' # Example 1: Testing if variance equals a specific value
#' set.seed(123)
#' data <- rnorm(30, mean = 100, sd = 15)  # Normally distributed data
#' 
#' # Two-sided test (default)
#' result <- onevar.test(data, s2 = 225)  # Test if variance = 225 (sd = 15)
#' print(result)
#' 
#' # Example 2: One-sided test for smaller variance
#' # Test if variance is less than 250
#' result_less <- onevar.test(data, s2 = 250, alternative = "less")
#' print(result_less)
#' 
#' # Example 3: One-sided test for larger variance
#' # Test if variance is greater than 200
#' result_greater <- onevar.test(data, s2 = 200, alternative = "greater")
#' print(result_greater)
#' 
#' # Example 4: Using different confidence level
#' result_99 <- onevar.test(data, s2 = 225, conf.level = 0.99)
#' print(result_99)
#' 
#' # Example 5: Practical application - quality control
#' # Suppose a machine produces parts with length variance that should be 0.04
#' parts <- c(10.1, 9.9, 10.0, 10.2, 9.8, 10.1, 9.9, 10.0, 10.2, 9.8)
#' qc_test <- onevar.test(parts, s2 = 0.04)
#' print(qc_test)
#'
#' @seealso
#' • \code{\link{var.test}} for comparing two variances
#' • \code{\link{t.test}} for testing population means
#' • \code{\link{shapiro.test}} for testing normality assumption
#' • \code{\link[KnoxStats]{fisherVar_test}} for the non-parametric alternative
#'
#' @references
#' Devore, Jay L. 2011. *Probability and Statistics for Engineering and the Sciences*. 
#' 8th ed. Boston: Cengage Learning.
#' 
#' Moore, David S., George P. McCabe, and Bruce A. Craig. 2017. *Introduction to the 
#' Practice of Statistics*. 9th ed. New York: W. H. Freeman.
#' 
#' NIST/SEMATECH. 2012. *e-Handbook of Statistical Methods*. 
#' http://www.itl.nist.gov/div898/handbook/. 
#'
#' @export
onevar.test <- function(x, s2 = 1, conf.level = 0.95, 
                        alternative = c("two.sided", "less", "greater")) {
  
  # Input validation with informative error messages
  if (!is.numeric(x)) {
    stop("Input 'x' must be a numeric vector", call. = FALSE)
  }
  
  if (any(is.na(x))) {
    stop("Missing values (NA) are not allowed in the input data", call. = FALSE)
  }
  
  if (length(x) < 2) {
    stop("Sample size must be at least 2", call. = FALSE)
  }
  
  if (s2 <= 0) {
    stop("Hypothesized variance must be positive", call. = FALSE)
  }
  
  if (conf.level <= 0 || conf.level >= 1) {
    stop("Confidence level must be between 0 and 1", call. = FALSE)
  }
  
  alternative <- match.arg(alternative)
  
  # Calculate test statistics
  n <- length(x)
  df <- n - 1
  sample_var <- var(x)
  ts <- df * sample_var / s2
  
  # Calculate p-values using chi-square distribution
  p_upper <- pchisq(ts, df = df, lower.tail = FALSE)
  p_lower <- pchisq(ts, df = df, lower.tail = TRUE)
  
  # Calculate confidence intervals
  alpha <- 1 - conf.level
  
  if (alternative == "two.sided") {
    pval <- 2 * min(p_upper, p_lower)
    lcl <- df * sample_var / qchisq(1 - alpha/2, df = df)
    ucl <- df * sample_var / qchisq(alpha/2, df = df)
  } else if (alternative == "less") {
    pval <- p_lower
    lcl <- 0
    ucl <- df * sample_var / qchisq(alpha, df = df)
  } else { # "greater"
    pval <- p_upper
    lcl <- df * sample_var / qchisq(1 - alpha, df = df)
    ucl <- Inf
  }
  
  # Prepare results in htest format
  result <- list(
    statistic = c(X2 = ts),
    parameter = c(df = df),
    p.value = pval,
    conf.int = c(lcl, ucl),
    estimate = c(variance = sample_var),
    null.value = c(variance = s2),
    alternative = alternative,
    method = "One-Sample Variance Test (Chi-Square Test)",
    data.name = deparse(substitute(x))
  )
  
  attr(result$conf.int, "conf.level") <- conf.level
  class(result) <- "htest"
  
  return(result)
}
