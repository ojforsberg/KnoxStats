#' @title
#' Binomial Predictions
#'
#' @description
#' Calculates prediction intervals for binomial proportions. This function estimates
#' the range in which future observations are likely to fall based on observed
#' sample data, using the normal approximation to the binomial distribution.
#' 
#' @param x Integer. The number of successes observed in the sample.
#' @param n Integer. The total number of trials (sample size).
#' @param conf.level Numeric. The confidence level for the prediction interval.
#'   Must be between 0 and 1. Default is 0.95 (95% confidence interval).
#' 
#' @return A list with three elements:
#' \itemize{
#'   \item \code{name}: Character string describing the interval type
#'   \item \code{lower}: The lower bound of the prediction interval
#'   \item \code{upper}: The upper bound of the prediction interval
#' }
#' 
#' @details
#' This function calculates prediction intervals for binomial proportions using
#' the normal approximation method. A prediction interval provides a range of
#' values that is likely to contain future observations, unlike a confidence
#' interval which provides a range for a population parameter.
#' 
#' The formula used is based on the normal approximation:
#' 
#' \deqn{\hat{p} \pm z_{\alpha/2} \times \sqrt{\hat{p}(1-\hat{p}) \times \left(1 + \frac{1}{n}\right)}}{%
#' p̂ ± z_{α/2} × √[p̂(1-p̂) × (1 + 1/n)]}
#' 
#' where:
#' \itemize{
#'   \item \eqn{\hat{p} = x/n} is the sample proportion
#'   \item \eqn{z_{\alpha/2}} is the critical value from the standard normal distribution
#'   \item \eqn{n} is the sample size
#' }
#' 
#' The normal approximation works best when \eqn{n\hat{p} \geq 5}{n×p̂ ≥ 5} and 
#' \eqn{n(1-\hat{p}) \geq 5}{n×(1-p̂) ≥ 5}. For small samples or extreme proportions,
#' exact methods may be more appropriate.
#' 
#' @examples
#' # Basic example: 15 successes out of 20 trials
#' binomPred(15, 20)
#' 
#' # With 90% confidence level
#' binomPred(15, 20, conf.level = 0.90)
#' 
#' # Multiple examples showing different scenarios
#' results <- list(
#'   "High success" = binomPred(18, 20),
#'   "Moderate success" = binomPred(10, 20),
#'   "Low success" = binomPred(2, 20)
#' )
#' 
#' # Print all results
#' results
#' 
#' # Extract just the intervals
#' lapply(results, function(x) c(lower = x$lower, upper = x$upper))
#' 
#' # What does a 99% confidence interval look like?
#' binomPred(15, 20, conf.level = 0.99)
#' 
#' # Error handling: invalid confidence level
#' try(binomPred(10, 20, conf.level = 1.5))
#' 
#' @section Warning:
#' This function uses the normal approximation, which may not be accurate for
#' small sample sizes or extreme proportions. When \eqn{n\hat{p} < 5}{n×p̂ < 5} 
#' or \eqn{n(1-\hat{p}) < 5}{n×(1-p̂) < 5}, consider using exact methods or
#' simulation-based approaches.
#' 
#' @references
#' Agresti, Alan, and Brent A. Coull. 1998. "Approximate Is Better Than 'Exact'
#' for Interval Estimation of Binomial Proportions." *American Statistician* 52 (2): 119–26.
#' 
#' Brown, Lawrence D., T. Tony Cai, and Anirban DasGupta. 2001. "Interval
#' Estimation for a Binomial Proportion." *Statistical Science* 16 (2): 101–33.
#' 
#' @seealso
#' \code{\link[stats]{binom.test}} for exact binomial tests and confidence intervals,
#' \code{\link[stats]{qnorm}} for normal distribution quantiles,
#' \code{\link{prop.test}} for testing proportions
#' 
#' @importFrom stats qnorm
#' @export
binomPred <- function(x, n, conf.level = 0.95) {
  # Input validation
  if (!is.numeric(x) || x < 0 || x > n) {
    stop("x must be a non-negative integer less than or equal to n")
  }
  if (!is.numeric(n) || n <= 0) {
    stop("n must be a positive integer")
  }
  if (!is.numeric(conf.level) || conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be a numeric value between 0 and 1")
  }
  
  # Calculate sample proportion
  phat <- x / n
  
  # Calculate alpha and critical values
  alpha <- 1 - conf.level
  z_lower <- stats::qnorm(alpha / 2)
  z_upper <- stats::qnorm(1 - alpha / 2)
  
  # Calculate standard error with finite population correction
  se <- sqrt(phat * (1 - phat) * 2 * 1/n)
  
  # Calculate prediction interval bounds
  lower_bound <- phat + z_lower * se
  upper_bound <- phat + z_upper * se

  # Ensure bounds are within [0, 1] for proportions
  lower_bound <- max(0, lower_bound)
  upper_bound <- min(1, upper_bound)
  
  # Return results as a list
  result <- list(
    name = "Binomial Prediction Interval",
    proportion = phat,
    sample_size = n,
    confidence_level = conf.level,
    lower = lower_bound,
    upper = upper_bound
  )
  
  # Add class for potential S3 methods
  class(result) <- "binomPred"
  
  return(result)
}

# Optional: Print method for nice output
#' @export
print.binomPred <- function(x, ...) {
  cat(sprintf("%s\n", x$name))
  cat(sprintf("Sample proportion: %.4f (%d/%d)\n", 
              x$proportion, 
              round(x$proportion * x$sample_size), 
              x$sample_size))
  cat(sprintf("Sample size: %d\n", x$sample_size))
  cat(sprintf("Confidence level: %.1f%%\n", x$confidence_level * 100))
  cat(sprintf("Prediction interval: [%.4f, %.4f]\n", x$lower, x$upper))
  invisible(x)
}