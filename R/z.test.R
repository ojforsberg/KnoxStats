#' @title 
#' Z-Test for Means with Known Variances
#' 
#' @description
#' Performs a one-sample or two-sample z-test for means when population variances
#' are known. This is appropriate when you know the true population standard 
#' deviation(s) and want to test whether a sample mean (or difference between 
#' two sample means) equals a specified value.
#' 
#' @param x Numeric vector of data values for the first sample.
#' @param y Optional numeric vector for the second sample. If provided, a 
#'   two-sample test is performed; otherwise, a one-sample test.
#' @param sigmax Known population standard deviation for the first sample (x).
#' @param sigmay Known population standard deviation for the second sample (y).
#'   Defaults to `sigmax`.
#' @param mu The hypothesized value of the mean (for one-sample) or difference 
#'   in means (for two-sample). Defaults to 0.
#' @param alternative Character string specifying the alternative hypothesis:
#'   `"two.sided"` (default), `"less"`, or `"greater"`.
#' @param conf.level Confidence level for the interval. Defaults to 0.95.
#' 
#' @return
#' An object of class `"htest"` containing the following components:
#' \item{statistic}{The z-test statistic}
#' \item{p.value}{The p-value for the test}
#' \item{conf.int}{Confidence interval for the mean or difference in means}
#' \item{estimate}{Estimated mean(s)}
#' \item{null.value}{The specified hypothesized value}
#' \item{alternative}{The alternative hypothesis}
#' \item{method}{A character string describing the test}
#' \item{data.name}{A character string describing the data}
#' 
#' @details
#' The z-test is used when population standard deviations are known, which is 
#' rare in practice but useful for theoretical understanding or when dealing 
#' with standardized tests. The test statistic follows a standard normal 
#' distribution under the null hypothesis.
#' 
#' For a one-sample test: z = (x̄ - μ₀) / (σ/√n)
#' 
#' For a two-sample test: z = ((x̄ - ȳ) - μ₀) / √(σ₁²/n₁ + σ₂²/n₂)
#' 
#' The function automatically detects whether to perform a one-sample or 
#' two-sample test based on whether `y` is provided.
#' 
#' @examples
#' # One-sample z-test
#' # Known population standard deviation = 15
#' test_scores <- c(78, 85, 92, 88, 76, 95, 89, 84, 91, 87)
#' result1 <- z.test(test_scores, sigmax = 15, mu = 85)
#' print(result1)
#' 
#' # Two-sample z-test with equal variances
#' group1 <- c(23, 25, 28, 22, 26, 24, 27)
#' group2 <- c(21, 19, 24, 18, 22, 20, 23)
#' result2 <- z.test(group1, group2, sigmax = 4, sigmay = 4)
#' print(result2)
#' 
#' # Two-sample z-test with unequal variances
#' result3 <- z.test(group1, group2, sigmax = 4, sigmay = 6)
#' print(result3)
#' 
#' # One-sided test (less than)
#' result4 <- z.test(test_scores, sigmax = 15, mu = 90, 
#'                   alternative = "less")
#' print(result4)
#' 
#' # Different confidence level
#' result5 <- z.test(test_scores, sigmax = 15, mu = 85, 
#'                   conf.level = 0.99)
#' print(result5)
#' 
#' @references
#' Devore, Jay L. 2015. *Probability and Statistics for Engineering and the 
#' Sciences*. 9th ed. Boston: Cengage Learning.
#' 
#' Moore, David S., George P. McCabe, and Bruce A. Craig. 2017. *Introduction to 
#' the Practice of Statistics*. 9th ed. New York: W.H. Freeman.
#' 
#' @seealso
#' \code{\link{t.test}} for the t-test when population variances are unknown,
#' \code{\link{prop.test}} for proportion tests,
#' \code{\link{pnorm}} for the standard normal distribution function
#' 
#' @importFrom stats pnorm qnorm
#' @export
z.test <- function(x, y = NULL, 
                   sigmax, sigmay = sigmax, mu = 0,
                   alternative = c("two.sided", "less", "greater"), 
                   conf.level = 0.95) {
  
  # Input validation
  alternative <- match.arg(alternative)
  
  if (sigmax <= 0 || sigmay <= 0) {
    stop("Standard deviations must be positive")
  }
  
  if (conf.level <= 0 || conf.level >= 1) {
    stop("Confidence level must be between 0 and 1")
  }
  
  # Perform the test
  if (is.null(y)) {
    # One-sample test
    method <- "One-sample z-test"
    se <- sigmax / sqrt(length(x))
    xbar <- mean(x)
    z <- (xbar - mu) / se
    est <- xbar
    pest <- xbar
    dn <- deparse(substitute(x))
    names(est) <- paste("mean of", dn)
    nv <- "mean"
    data.names <- dn
  } else {
    # Two-sample test
    se <- sqrt((sigmax^2 / length(x)) + (sigmay^2 / length(y)))
    xbar <- mean(x)
    ybar <- mean(y)
    z <- ((xbar - ybar) - mu) / se
    est <- c(xbar, ybar)
    pest <- xbar - ybar
    names(est)[1] <- paste("mean of", deparse(substitute(x)))
    names(est)[2] <- paste("mean of", deparse(substitute(y)))
    dn <- paste(deparse(substitute(x)), "(σ=", sigmax, ") and ", 
                deparse(substitute(y)), "(σ=", sigmay, ")", sep = "")
    nv <- "difference in means"
    data.names <- dn
    
    if (sigmax != sigmay) {
      method <- "Two-sample z-test, unequal variances"
    } else {
      method <- "Two-sample z-test, equal variances"
    }
  }
  
  # Calculate p-value and confidence interval
  alpha <- 1 - conf.level
  
  if (alternative == "two.sided") {
    p <- 2 * pnorm(-abs(z))
    ci <- pest + c(-1, 1) * qnorm(1 - alpha/2) * se
  } else if (alternative == "less") {
    p <- pnorm(z)
    ci <- c(-Inf, pest + qnorm(1 - alpha) * se)
  } else {  # alternative == "greater"
    p <- 1 - pnorm(z)
    ci <- c(pest - qnorm(1 - alpha) * se, Inf)
  }
  
  # Create results object
  results <- list(
    statistic = c(z = z),
    parameter = c(df = Inf),
    p.value = p,
    conf.int = ci,
    estimate = est,
    null.value = c("mean" = mu),
    alternative = alternative,
    method = method,
    data.name = data.names,
    sigmax = sigmax,
    sigmay = sigmay
  )
  
  attr(results$conf.int, "conf.level") <- conf.level
  class(results) <- "htest"
  
  return(results)
}
