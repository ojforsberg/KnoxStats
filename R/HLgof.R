#' @title 
#' Hosmer-Lemeshow Goodness-of-Fit Test
#' 
#' @description 
#' Performs the Hosmer-Lemeshow goodness-of-fit test for logistic regression models.
#' This test assesses whether the observed event rates in subgroups match the 
#' expected event rates predicted by the model.
#' 
#' @param y A numeric vector of observed binary outcomes (0 or 1).
#' @param yhat A numeric vector of predicted probabilities from a logistic 
#'   regression model (values between 0 and 1).
#' @param g Integer specifying the number of groups to create based on predicted 
#'   probabilities (default is 10). Must be at least 3.
#' 
#' @return An object of class \code{"htest"} containing:
#' \item{statistic}{The Hosmer-Lemeshow chi-square test statistic (H)}
#' \item{parameter}{Degrees of freedom (g - 2)}
#' \item{p.value}{The p-value for the test}
#' \item{method}{A character string describing the test}
#' \item{data.name}{A character string giving the name of the data}
#' \item{alternative}{A character string describing the alternative hypothesis}
#' 
#' @details 
#' The Hosmer-Lemeshow test is commonly used to assess the calibration of 
#' logistic regression models. The test works by:
#' 
#' 1. Sorting observations by their predicted probabilities (\code{yhat})
#' 2. Dividing them into \code{g} groups (usually 10) based on quantiles
#' 3. Comparing observed vs. expected outcomes in each group using a chi-square test
#' 
#' A small p-value (typically < 0.05) suggests that the model doesn't fit the 
#' data well. However, the test has limitations:
#' 
#' \itemize{
#'   \item It's sensitive to the choice of \code{g} (number of groups)
#'   \item It assumes no group has fewer than 5 observations
#'   \item It works best with large sample sizes
#'   \item It may have low power to detect certain types of misfit
#' }
#' 
#' The test statistic follows a chi-square distribution with \code{g - 2} degrees 
#' of freedom under the null hypothesis that the model fits the data.
#' 
#' @examples
#' # Simulate some data for a logistic regression example
#' set.seed(123)
#' n <- 200
#' x1 <- rnorm(n)
#' x2 <- rnorm(n)
#' 
#' # True logistic model
#' true_prob <- plogis(0.5 + 0.8*x1 - 0.4*x2)
#' y <- rbinom(n, 1, true_prob)
#' 
#' # Fit a logistic regression model
#' model <- glm(y ~ x1 + x2, family = binomial)
#' 
#' # Get predicted probabilities
#' predicted_probs <- predict(model, type = "response")
#' 
#' # Perform Hosmer-Lemeshow test with default (10 groups)
#' HLgof(y, predicted_probs)
#' 
#' # Try with fewer groups (e.g., 5)
#' HLgof(y, predicted_probs, g = 5)
#' 
#' # Example with poorly fitting model
#' bad_model <- glm(y ~ 1, family = binomial)  # Null model
#' bad_probs <- predict(bad_model, type = "response")
#' HLgof(y, bad_probs)
#' 
#' # Note: The test may warn about small bin sizes
#' # with too many groups for small datasets
#' small_data_y <- y[1:30]
#' small_data_probs <- predicted_probs[1:30]
#' \dontrun{
#' HLgof(small_data_y, small_data_probs, g = 10)  # May produce warning
#' }
#' 
#' @references 
#' Hosmer, David W., and Stanley Lemeshow. 1980. "Goodness-of-Fit Tests for 
#' the Multiple Logistic Regression Model." *Communications in Statistics: 
#' Theory and Methods* 9 (10): 1043–69. 
#' 
#' Hosmer, David W., Stanley Lemeshow, and Rodney X. Sturdivant. 2013. *Applied 
#' Logistic Regression*. 3rd ed. Hoboken, NJ: John Wiley & Sons.
#' 
#' Paul, Pranab, Tathagata Banerjee, and Arnab Kumar Maity. 2021. "A Critical 
#' Review of the Hosmer-Lemeshow Test." *International Journal of Epidemiology* 
#' 50 (4): 1077–84. doi:10.1093/ije/dyab049.
#' 
#' @seealso 
#' \code{\link[stats]{glm}} for fitting logistic regression models,
#' \code{\link[stats]{pchisq}} for the chi-square distribution function,
#' \code{\link[stats]{quantile}} for creating the groups,
#' \code{\link[stats]{xtabs}} for creating contingency tables
#' 
#' @importFrom stats quantile pchisq xtabs
#' @export
HLgof <- function(y, yhat, g = 10) {
  # Input validation
  if (!is.numeric(y) || !all(y %in% c(0, 1))) {
    stop("'y' must be a numeric vector of 0s and 1s")
  }
  
  if (!is.numeric(yhat) || any(yhat < 0 | yhat > 1)) {
    stop("'yhat' must be a numeric vector of probabilities between 0 and 1")
  }
  
  if (length(y) != length(yhat)) {
    stop("'y' and 'yhat' must have the same length")
  }
  
  if (g < 3) {
    stop("Number of groups 'g' must be at least 3")
  }
  
  # Create result structure
  ret <- list(
    method = "Hosmer and Lemeshow Goodness of Fit test",
    data.name = deparse(substitute(y)),
    alternative = "Model does not fit the data well"
  )
  
  # Create groups based on predicted probabilities
  endpts <- quantile(yhat, probs = seq(0, 1, length.out = g + 1))
  bins <- cut(yhat, breaks = endpts, include.lowest = TRUE)
  
  # Check for small bins
  bin_counts <- table(bins)
  if (any(bin_counts < 5)) {
    warning(
      "The HL statistic assumes no bin has fewer than five observations.\n",
      "Please reduce the number of groups (g).\n",
      "Smallest bin has ", min(bin_counts), " observations."
    )
  }
  
  # Calculate observed and expected frequencies
  ob <- xtabs(cbind(`Observed 0` = 1 - y, `Observed 1` = y) ~ bins)
  ex <- xtabs(cbind(`Expected 0` = 1 - yhat, `Expected 1` = yhat) ~ bins)
  
  # Calculate test statistic
  H <- sum((ob - ex)^2 / ex)
  
  if (H < 0) {
    warning("Negative test statistic detected. Check that y and yhat are valid probabilities.")
  }
  
  # Calculate p-value
  p <- pchisq(H, df = g - 2, lower.tail = FALSE)
  
  # Format output for htest class
  ret$statistic <- c(H = H)
  ret$parameter <- c(df = g - 2)
  ret$p.value <- p
  
  # Additional information for user
  ret$observed <- ob
  ret$expected <- ex
  ret$groups <- bins
  
  class(ret) <- "htest"
  return(ret)
}
