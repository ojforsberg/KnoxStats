#' @title 
#' Model Fit Statistics
#'
#' @description
#' A collection of functions to calculate various information criteria for
#' statistical model selection. These criteria help balance model fit with
#' complexity to prevent overfitting.
#'
#' @details
#' Information criteria are tools for selecting among statistical models.
#' They all follow the same basic form: -2*log-likelihood + penalty for complexity.
#' The penalty term varies between criteria, with some being more conservative
#' (penalizing complex models more heavily) than others.
#'
#' When comparing models using these criteria:
#' \itemize{
#'   \item{Lower values indicate better models (better fit with less complexity)}
#'   \item{Compare models fit to the same data}
#'   \item{Differences > 2-4 are usually considered meaningful}
#'   \item{No single criterion is always best - consider several}
#' }
#'
#' @param model A fitted statistical model object (e.g., from \code{lm()},
#'   \code{glm()}) that has methods for \code{logLik()} and \code{nobs()}.
#'
#' @return A numeric value representing the information criterion for the model.
#'
#' @examples
#' # Fit a simple linear regression model
#' data(mtcars)
#' model1 <- lm(mpg ~ wt, data = mtcars)
#' model2 <- lm(mpg ~ wt + hp, data = mtcars)
#'
#' # Calculate AIC for both models
#' AIC2(model1)
#' AIC2(model2)
#'
#' # Calculate SBC (BIC) - more conservative
#' SBC(model1)
#' SBC(model2)
#'
#' # For small samples, use AICc
#' AICc(model1)
#'
#' # Get all fit statistics at once
#' \dontrun{
#' model.fit(model1)
#' }
#'
#' # Compare two models
#' \dontrun{
#' cat("Model 1 (mpg ~ wt):\n")
#' model.fit(model1)
#' cat("Model 2 (mpg ~ wt + hp):\n")
#' model.fit(model2)
#' }
#'
#' @references
#' Akaike, Hirotugu. 1974. "A New Look at the Statistical Model Identification."
#' \emph{IEEE Transactions on Automatic Control} 19 (6): 716–23.
#'
#' Schwarz, Gideon. 1978. "Estimating the Dimension of a Model."
#' \emph{The Annals of Statistics} 6 (2): 461–64.
#'
#' Burnham, Kenneth P., and David R. Anderson. 2002. \emph{Model Selection
#' and Multimodel Inference: A Practical Information-Theoretic Approach}.
#' 2nd ed. New York: Springer.
#'
#' Hurvich, Clifford M., and Chih-Ling Tsai. 1989. "Regression and Time Series
#' Model Selection in Small Samples." \emph{Biometrika} 76 (2): 297–307.
#'
#' @seealso
#' \code{\link{AIC}} for R's built-in AIC function,
#' \code{\link{BIC}} for R's built-in BIC function,
#' \code{\link{logLik}} for extracting log-likelihood values,
#' \code{\link{nobs}} for getting number of observations
#'
#' @rdname model_fit_statistics
#' @name model_fit_statistics
NULL

#' @title Akaike Information Criterion (AIC)
#' @rdname AIC2
#' @export
#' @description
#' Calculates the Akaike Information Criterion (AIC) for a model.
#' AIC = -2*log-likelihood + 2*k, where k is the number of parameters.
#' Lower AIC values indicate better models.
AIC2 <- function(model) {
  k   <- attr(logLik(model), "df")
  lnl <- as.numeric(logLik(model))
  ret <- -2 * lnl + 2 * k
  return(ret)
}

#' @title Schwarz Bayesian Criterion (SBC/BIC)
#' @rdname SBC
#' @export
#' @description
#' Calculates the Schwarz Bayesian Criterion (SBC), also known as the
#' Bayesian Information Criterion (BIC). SBC = -2*log-likelihood + log(n)*k,
#' where n is sample size and k is number of parameters. This criterion
#' penalizes complex models more heavily than AIC, especially with larger samples.
SBC <- function(model) {
  k   <- attr(logLik(model), "df")
  lnl <- as.numeric(logLik(model))
  n   <- nobs(model)
  ret <- -2 * lnl + log(n) * k
  return(ret)
}

#' @title Corrected Akaike Information Criterion (AICc)
#' @rdname AICc
#' @export
#' @description
#' Calculates the corrected Akaike Information Criterion (AICc) for small samples.
#' AICc = AIC + 2*k*(k+1)/(n-k-1). This correction reduces bias when the
#' sample size is small relative to the number of parameters.
AICc <- function(model) {
  k   <- attr(logLik(model), "df")
  n   <- nobs(model)
  ret <- AIC(model) + 2 * k * (k + 1) / (n - k - 1)
  return(ret)
}

#' @title Corrected Schwarz Bayesian Criterion (SBCc)
#' @rdname SBCc
#' @export
#' @description
#' Calculates the corrected Schwarz Bayesian Criterion (SBCc) for small samples.
#' This is a small-sample correction to SBC similar to the AICc correction.
SBCc <- function(model) {
  k   <- attr(logLik(model), "df")
  n   <- nobs(model)
  ret <- SBC(model) + 2 * k * (k + 1) / (n - k - 1)
  return(ret)
}

#' @title Quasi-Akaike Information Criterion (QAIC)
#' @rdname QAIC
#' @export
#' @description
#' Calculates the Quasi-Akaike Information Criterion (QAIC) for models with
#' overdispersion, such as some generalized linear models. QAIC adjusts AIC
#' for extra variation not captured by the model.
QAIC <- function(model) {
  k   <- attr(logLik(model), "df")
  lnl <- as.numeric(logLik(model))
  # Calculate dispersion parameter
  chat <- model$deviance / model$df.residual
  ret <- -2 * lnl / chat + 2 * k
  return(ret)
}

#' @title Corrected Quasi-Akaike Information Criterion (QAICc)
#' @rdname QAICc
#' @export
#' @description
#' Calculates the corrected Quasi-Akaike Information Criterion (QAICc) for
#' small samples with overdispersion.
QAICc <- function(model) {
  k   <- attr(logLik(model), "df")
  n   <- nobs(model)
  ret <- QAIC(model) + 2 * k * (k + 1) / (n - k - 1)
  return(ret)
}

#' @title Mallows' Cp Statistic
#' @rdname Cp
#' @export
#' @description
#' Calculates Mallows' Cp statistic for linear regression model selection.
#' Cp = SSE/s² + 2*k - n, where SSE is sum of squared errors, s² is error
#' variance, k is number of parameters, and n is sample size. Used primarily
#' for selecting among nested linear models.
Cp <- function(model) {
  if (!inherits(model, "lm")) {
    warning("Cp statistic is designed for linear regression models")
  }
  k   <- attr(logLik(model), "df")
  n   <- nobs(model)
  SSE <- sum(model$residuals^2)
  # Estimate error variance from the model
  s2  <- summary(model)$sigma^2
  ret <- SSE / s2 + 2 * k - n
  return(ret)
}

#' @title Print Model Fit Statistics
#' @rdname model.fit
#' @export
#' @description
#' Prints a comprehensive table of model fit statistics for easy comparison.
#' This function displays multiple information criteria at once, making it
#' convenient for model selection.
#'
#' @examples
#' \dontrun{
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' model.fit(model)
#' }
model.fit <- function(model) {
  cat("\nModel Fit Statistics\n")
  cat(rep("-", 30), "\n\n", sep = "")

  cat("Basic Fit:\n")
  cat(sprintf("\t-2*log(Likelihood) = %.2f\n\n", -2 * as.numeric(logLik(model))))

  cat("Information Criteria:\n")
  cat(sprintf("\tAIC    = %.2f\n", AIC(model)))
  cat(sprintf("\tAICc   = %.2f (corrected for small samples)\n", AICc(model)))
  cat(sprintf("\tSBC    = %.2f (also called BIC)\n", SBC(model)))
  cat(sprintf("\tSBCc   = %.2f (corrected SBC)\n", SBCc(model)))

  # Only show QAIC for appropriate models
  if (inherits(model, "glm")) {
    cat("\nFor Overdispersed Models:\n")
    cat(sprintf("\tQAIC   = %.2f\n", QAIC(model)))
    cat(sprintf("\tQAICc  = %.2f\n", QAICc(model)))
  }

  # Only show Cp for linear models
  if (inherits(model, "lm")) {
    cat("\nFor Linear Regression:\n")
    cat(sprintf("\tCp     = %.2f\n", Cp(model)))
  }

  cat("\n", rep("-", 30), "\n", sep = "")
  cat("Note: Lower values indicate better models\n")
  cat("      (better fit with less complexity)\n")
}
