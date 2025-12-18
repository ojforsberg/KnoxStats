#' @title 
#' Wald Test for Proportions
#'
#' @description
#' Performs one-sample and two-sample Wald tests for proportions, including 
#' confidence intervals. This test is appropriate for large sample sizes 
#' (typically when both successes and failures are at least 5).
#'
#' @param x Numeric vector of successes. For one-sample test: a single number. 
#'   For two-sample test: a vector of length 2 (c(successes1, successes2)).
#' @param n Numeric vector of sample sizes. For one-sample test: a single number. 
#'   For two-sample test: a vector of length 2 (c(n1, n2)).
#' @param p For one-sample test: hypothesized population proportion (default = 0.5).
#' @param pdiff For two-sample test: hypothesized difference in proportions 
#'   (default = 0, testing equality of proportions).
#' @param conf.level Confidence level for the interval (default = 0.95).
#' @param alternative Character string specifying the alternative hypothesis:
#'   \code{"two.sided"} (default), \code{"less"}, or \code{"greater"}.
#'
#' @return
#' A list with class \code{"htest"} containing the following components:
#' \item{statistic}{The value of the z-statistic.}
#' \item{p.value}{The p-value for the test.}
#' \item{conf.int}{A confidence interval for the proportion or difference.}
#' \item{estimate}{The sample proportion(s).}
#' \item{null.value}{The hypothesized value of the proportion or difference.}
#' \item{alternative}{A character string describing the alternative hypothesis.}
#' \item{method}{A character string indicating the type of test performed.}
#' \item{data.name}{A character string describing the data.}
#'
#' @details
#' The Wald test is a statistical test for proportions based on the normal 
#' approximation to the binomial distribution. It works well with large sample 
#' sizes but can be inaccurate with small samples. The test statistic follows 
#' approximately a standard normal distribution.
#' 
#' \strong{One-sample test:} Tests whether a population proportion equals a 
#' specified value (e.g., is the proportion of left-handed students different 
#' from 0.10?).
#' 
#' \strong{Two-sample test:} Tests whether two population proportions are equal 
#' (e.g., do male and female students have different passing rates?).
#' 
#' \strong{Sample size warning:} The function issues a warning if any category 
#' has fewer than 5 successes or failures, as the normal approximation may not 
#' be accurate. In such cases, consider using exact tests like Fisher's exact test.
#'
#' @section Assumptions:
#' 1. Observations are independent.
#' 2. Sample size is sufficiently large (np ≥ 5 and n(1-p) ≥ 5 for each group).
#' 3. Data come from a binomial process (each trial has two possible outcomes).
#'
#' @examples
#' # ONE-SAMPLE EXAMPLE:
#' # Test if the proportion of heads from 100 coin flips is different from 0.5
#' # Suppose we observed 60 heads in 100 flips
#' wald.test(x = 60, n = 100, p = 0.5)
#' 
#' # Test if less than 20% of students are left-handed
#' # In a sample of 200 students, 30 were left-handed
#' wald.test(x = 30, n = 200, p = 0.2, alternative = "less")
#' 
#' # Get a 90% confidence interval for the proportion
#' wald.test(x = 30, n = 200, conf.level = 0.90)
#' 
#' # TWO-SAMPLE EXAMPLE:
#' # Test if passing rates differ between two teaching methods
#' # Method A: 45 passed out of 60
#' # Method B: 35 passed out of 55
#' wald.test(x = c(45, 35), n = c(60, 55))
#' 
#' # Test if proportion A is greater than proportion B
#' wald.test(x = c(45, 35), n = c(60, 55), alternative = "greater")
#' 
#' # Test with a specific hypothesized difference
#' wald.test(x = c(45, 35), n = c(60, 55), pdiff = 0.1, alternative = "two.sided")
#'
#' @importFrom stats pnorm qnorm
#'
#' @seealso
#' \code{\link{prop.test}} for the more commonly used score test (Wilson interval),
#' \code{\link{binom.test}} for exact binomial test (better for small samples),
#' \code{\link{fisher.test}} for exact test of independence in 2x2 tables.
#'
#' @references
#' Agresti, Alan, and Brent A. Coull. 1998. "Approximate Is Better Than 'Exact' 
#' for Interval Estimation of Binomial Proportions." \emph{The American Statistician} 
#' 52 (2): 119–26.
#' 
#'
#' @export
wald.test <- function(x, n, p = 0.5, pdiff = 0, conf.level = 0.95, 
                      alternative = c("two.sided", "less", "greater")) {
  
  # Validate alternative hypothesis
  alternative <- match.arg(alternative)
  
  # Handle case where x has length 2 but n has length 1 (assume equal n)
  if (length(x) == 2 && length(n) == 1) {
    n <- c(n, n)
  }
  
  # Create descriptive data name
  dn <- paste(deparse(substitute(x)), "out of", deparse(substitute(n)))
  
  # ONE-SAMPLE TEST
  if (length(x) == 1) {
    x <- x[1]
    n <- n[1]
    
    # Check sample size adequacy
    if (x < 5 || n - x < 5) {
      warning("The sample size may not be large enough for accurate results. ",
              "Consider using binom.test() for small samples.")
    }
    
    # Set test information
    test <- "One-Sample Wald Test for Proportions"
    nv <- "proportion"
    
    # Calculate statistics
    phat <- x / n
    se_wald <- sqrt(p * (1 - p) / n)  # Standard error under null
    ts <- (phat - p) / se_wald  # Test statistic
    
    # Calculate p-value based on alternative hypothesis
    if (alternative == "two.sided") {
      pval <- 2 * stats::pnorm(-abs(ts))
      alternativeHypothesis <- paste("true proportion is not equal to", p)
      c <- (1 - conf.level) / 2
    } else if (alternative == "less") {
      pval <- stats::pnorm(ts)
      alternativeHypothesis <- paste("true proportion is less than", p)
      c <- 1 - conf.level
    } else {  # "greater"
      pval <- 1 - stats::pnorm(ts)
      alternativeHypothesis <- paste("true proportion is greater than", p)
      c <- 1 - conf.level
    }
    
    # Confidence interval (using sample proportion for SE)
    se_ci <- sqrt(phat * (1 - phat) / n)
    E <- -stats::qnorm(c) * se_ci
    lcl <- max(0, phat - E)  # Bound at 0
    ucl <- min(1, phat + E)  # Bound at 1
    
    # Prepare estimates
    est <- phat
    names(est) <- "sample proportion"
    expectedValue <- p
    
  } else if (length(x) == 2) {  # TWO-SAMPLE TEST
    x1 <- x[1]
    x2 <- x[2]
    n1 <- n[1]
    n2 <- n[2]
    
    # Check sample size adequacy
    if (x1 < 5 || n1 - x1 < 5 || x2 < 5 || n2 - x2 < 5) {
      warning("At least one sample size may not be large enough for accurate results.")
    }
    
    # Set test information
    test <- "Two-Sample Wald Test for Proportions"
    nv <- "difference in proportions"
    
    # Calculate sample proportions
    phat1 <- x1 / n1
    phat2 <- x2 / n2
    
    # Pooled proportion for test statistic (under null of equality)
    p_pool <- (x1 + x2) / (n1 + n2)
    se_test <- sqrt(p_pool * (1 - p_pool) * (1/n1 + 1/n2))
    ts <- ((phat1 - phat2) - pdiff) / se_test
    
    # Calculate p-value
    if (alternative == "two.sided") {
      pval <- 2 * stats::pnorm(-abs(ts))
      alternativeHypothesis <- paste("true difference in proportions is not equal to", pdiff)
      c <- (1 - conf.level) / 2
    } else if (alternative == "less") {
      pval <- stats::pnorm(ts)
      alternativeHypothesis <- paste("true difference in proportions is less than", pdiff)
      c <- 1 - conf.level
    } else {  # "greater"
      pval <- 1 - stats::pnorm(ts)
      alternativeHypothesis <- paste("true difference in proportions is greater than", pdiff)
      c <- 1 - conf.level
    }
    
    # Confidence interval (using individual proportions for SE)
    se_ci <- sqrt(phat1 * (1 - phat1)/n1 + phat2 * (1 - phat2)/n2)
    E <- -stats::qnorm(c) * se_ci
    lcl <- (phat1 - phat2) - E
    ucl <- (phat1 - phat2) + E
    
    # Prepare estimates
    est <- c(phat1, phat2)
    names(est) <- c("prop in group 1", "prop in group 2")
    expectedValue <- pdiff
    
  } else {
    stop("x must have length 1 (one-sample) or 2 (two-sample)")
  }
  
  # Create results object
  results <- list(
    statistic = ts,
    parameter = c(df = Inf),  # Normal approximation, infinite degrees of freedom
    p.value = pval,
    conf.int = c(lcl, ucl),
    estimate = est,
    null.value = expectedValue,
    alternative = alternativeHypothesis,
    method = test,
    data.name = dn
  )
  
  attr(results$conf.int, "conf.level") <- conf.level
  names(results$statistic) <- "z"
  class(results) <- "htest"
  
  return(results)
}
