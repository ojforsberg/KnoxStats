#' @title 
#' Heteroskedasticity-Consistent Standard Errors
#'
#' @description
#' Calculates regression coefficient estimates with heteroskedasticity-consistent 
#' standard errors using the Huber-White sandwich estimator.
#'
#' @param model A fitted linear regression model object from \code{\link{lm}}.
#'
#' @return 
#' A matrix similar to \code{\link{summary.lm}}'s coefficients table, but with
#' heteroskedasticity-consistent standard errors, t-values, and p-values.
#' 
#' @details
#' This function computes heteroskedasticity-consistent (HC) standard errors,
#' also known as "robust standard errors" or "White-Huber standard errors."
#' These are particularly useful when the assumption of homoskedasticity
#' (constant variance of errors) is violated in regression analysis.
#' 
#' \strong{Why use this function?} In ordinary least squares (OLS) regression,
#' we assume that the variance of the errors is constant across all observations
#' (homoskedasticity). When this assumption is violated (heteroskedasticity),
#' the standard errors from \code{summary(lm())} may be incorrect, leading to
#' invalid hypothesis tests and confidence intervals.
#' 
#' \strong{What it does:} This function adjusts the standard errors to be valid
#' even when heteroskedasticity is present, while keeping the original OLS
#' coefficient estimates unchanged.
#' 
#' \strong{The adjustment:} The function uses the Huber-White sandwich estimator:
#' \deqn{Var(\hat{\beta}) = (X'X)^{-1} X' \Omega X (X'X)^{-1}}
#' where \eqn{\Omega} is a diagonal matrix of squared residuals.
#'
#' @examples
#' # Load a dataset with potential heteroskedasticity
#' data(mtcars)
#' 
#' # Fit a linear regression model
#' model <- lm(mpg ~ wt + hp + qsec, data = mtcars)
#' 
#' # Compare standard summary vs. heteroskedasticity-consistent summary
#' cat("Standard OLS summary:\n")
#' print(summary(model)$coefficients)
#' 
#' cat("\n\nHeteroskedasticity-consistent summary:\n")
#' print(summaryHCE(model))
#' 
#' # Visual check for heteroskedasticity
#' par(mfrow = c(2, 2))
#' plot(model, which = 1:3)
#' 
#' # Test with simulated heteroskedastic data
#' set.seed(123)
#' n <- 100
#' x <- rnorm(n)
#' y <- 2 + 3*x + x*rnorm(n)  # Heteroskedastic error structure
#' sim_model <- lm(y ~ x)
#' 
#' cat("\n\nSimulated heteroskedastic data:\n")
#' cat("Standard errors:\n")
#' print(summary(sim_model)$coefficients[, "Std. Error"])
#' cat("\nRobust standard errors:\n")
#' print(summaryHCE(sim_model)[, "stderr"])
#'
#' @references
#' Huber, Peter J. 1967. "The Behavior of Maximum Likelihood Estimates under
#' Nonstandard Conditions." In *Proceedings of the Fifth Berkeley Symposium on
#' Mathematical Statistics and Probability*, vol. 1, 221–33. Berkeley: University
#' of California Press.
#' 
#' White, Halbert. 1980. "A Heteroskedasticity-Consistent Covariance Matrix
#' Estimator and a Direct Test for Heteroskedasticity." *Econometrica* 48 (4): 
#' 817–38. \doi{10.2307/1912934}.
#' 
#' Wooldridge, Jeffrey M. 2019. *Introductory Econometrics: A Modern Approach*.
#' 7th ed. Boston: Cengage Learning. (See chapter 8 for heteroskedasticity).
#'
#' @seealso
#' \code{\link{lm}} for fitting linear models,
#' \code{\link{summary.lm}} for standard regression summaries,
#' \code{\link{sandwich::vcovHC}} from the \pkg{sandwich} package for more 
#' advanced heteroskedasticity-consistent covariance matrices,
#' \code{\link{lmtest::coeftest}} from the \pkg{lmtest} package for testing
#' coefficients with robust standard errors,
#' \code{\link{ncvTest}} from the \pkg{car} package for testing for 
#' heteroskedasticity.
#'
#' @importFrom stats model.matrix residuals
#' @export
summaryHCE <- function(model) {
  if (!inherits(model, "lm")) {
    stop("Input must be an 'lm' object from the lm() function")
  }
  
  # Get basic summary and components
  s <- summary(model)
  X <- stats::model.matrix(model)
  u2 <- stats::residuals(model)^2
  
  # Calculate heteroskedasticity-consistent covariance matrix
  n <- nrow(X)
  k <- ncol(X)
  Du <- diag(u2)
  XpDX <- t(X) %*% Du %*% X
  XpXi <- solve(t(X) %*% X)
  varcovar <- XpXi %*% XpDX %*% XpXi
  
  # Calculate adjusted standard errors
  dfadj <- sqrt(n) / sqrt(n - k)
  stderr <- dfadj * sqrt(diag(varcovar))
  
  # Calculate t-statistics and p-values
  t_stat <- model$coefficients / stderr
  p_value <- 2 * pnorm(-abs(t_stat))
  
  # Create results matrix
  results <- cbind(
    Estimate = model$coefficients,
    `HC Std. Error` = stderr,
    `HC t value` = t_stat,
    `HC Pr(>|t|)` = p_value
  )
  
  # Round p-values for readability (optional)
  results[, 4] <- round(results[, 4], 6)
  
  # Preserve row names
  rownames(results) <- rownames(s$coefficients)
  
  # Add a note about the method
  attr(results, "method") <- "Huber-White heteroskedasticity-consistent standard errors"
  attr(results, "model") <- deparse(substitute(model))
  
  return(results)
}
