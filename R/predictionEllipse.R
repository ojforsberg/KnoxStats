#' @title 
#' Prediction Ellipse for Multivariate Linear Regression
#'
#' @description
#' Creates a prediction ellipse (confidence region) for a bivariate response 
#' from a multivariate linear regression model. The ellipse shows where future 
#' observations are likely to fall with a specified confidence level.
#'
#' @param mod An object of class `lm` (linear model) from a multivariate 
#'   regression with at least 2 response variables. Must be created using 
#'   \code{\link{lm}} with a matrix response.
#' @param newdata A data frame containing the predictor values for which to 
#'   create the prediction ellipse. Must have the same predictor variables as 
#'   the original model.
#' @param conf.level Confidence level for the prediction ellipse (default = 0.95).
#'   Must be between 0 and 1.
#' @param ... Additional graphical parameters passed to \code{\link{plot}}.
#'
#' @return
#' Invisibly returns a matrix of (x, y) coordinates that define the ellipse 
#' boundary. Primarily used for its side effect of creating a plot.
#'
#' @details
#' This function creates a prediction ellipse for bivariate response data 
#' from a multivariate linear regression. Unlike confidence intervals for 
#' single variables, this ellipse accounts for the correlation between two 
#' response variables.
#'
#' The ellipse is centered at the predicted values for the given predictor 
#' values, and its size and shape are determined by:
#' \enumerate{
#'   \item{The prediction uncertainty (how well we can predict)}
#'   \item{The correlation between the two response variables}
#'   \item{The sample size and model complexity}
#'   \item{The specified confidence level}
#' }
#'
#' The mathematical basis is Hotelling's T-squared distribution, which is the 
#' multivariate extension of the t-distribution. The red point in the center 
#' shows the predicted values for the given predictor combination.
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic usage with simulated data
#' # Create a multivariate response (two outcome variables)
#' set.seed(123)
#' n <- 50
#' x <- rnorm(n)
#' y1 <- 2 + 1.5*x + rnorm(n, sd = 0.5)
#' y2 <- 1 + 0.8*x + rnorm(n, sd = 0.7)
#' 
#' # Response matrix (both y1 and y2 together)
#' Y <- cbind(y1, y2)
#' 
#' # Fit multivariate regression
#' mod <- lm(Y ~ x)
#' 
#' # Create prediction ellipse for x = 0.5
#' newdata <- data.frame(x = 0.5)
#' predictionEllipse(mod, newdata)
#' 
#' # Example 2: With multiple predictors
#' data <- data.frame(
#'   x1 = rnorm(40),
#'   x2 = rnorm(40),
#'   y1 = rnorm(40),
#'   y2 = rnorm(40)
#' )
#' Y <- cbind(data$y1, data$y2)
#' mod2 <- lm(Y ~ x1 + x2, data = data)
#' 
#' # Predict for specific values
#' newdata2 <- data.frame(x1 = 0, x2 = 0)
#' predictionEllipse(mod2, newdata2, conf.level = 0.90,
#'                   main = "90% Prediction Ellipse",
#'                   col = "blue", lwd = 2)
#' 
#' # Example 3: Add observed data points
#' plot(data$y1, data$y2, pch = 16, col = "gray",
#'      xlab = "Response 1", ylab = "Response 2",
#'      main = "Data with Prediction Ellipse")
#' predictionEllipse(mod2, newdata2, add = TRUE)
#' points(newdata2$x1, newdata2$x2, pch = 19, col = "red", cex = 1.5)
#' }
#'
#' @references
#' Fox, John, and Sanford Weisberg. 2019. *An R Companion to Applied Regression*. 
#' 3rd ed. Thousand Oaks, CA: Sage. https://socialsciences.mcmaster.ca/jfox/Books/Companion/.
#'
#' Johnson, Richard A., and Dean W. Wichern. 2007. *Applied Multivariate 
#' Statistical Analysis*. 6th ed. Upper Saddle River, NJ: Pearson Prentice Hall.
#'
#' Hotelling, Harold. 1931. "The Generalization of Student's Ratio." *Annals of 
#' Mathematical Statistics* 2 (3): 360–78. https://doi.org/10.1214/aoms/1177732979.
#'
#' @seealso
#' \code{\link{lm}} for fitting linear models (with matrix responses),
#' \code{\link[car]{ellipse}} for generating ellipse coordinates,
#' \code{\link{predict.lm}} for making predictions from linear models,
#' \code{\link{confint}} for confidence intervals in univariate regression
#'
#' @importFrom car ellipse
#' @importFrom graphics plot points
#' @importFrom stats model.frame model.matrix predict qf resid terms delete.response
#' @export
predictionEllipse <- function(mod, newdata, conf.level = 0.95, ...) {
  # Validate inputs with student-friendly error messages
  if (!inherits(mod, "lm")) {
    stop("The 'mod' argument must be a linear model object created by lm()")
  }
  
  # Check if response is a matrix (for multivariate regression)
  response <- mod$model[[1]]
  if (!is.matrix(response)) {
    stop("Response must be a matrix. Use cbind() for multiple responses.\n",
         "Example: lm(cbind(y1, y2) ~ x1 + x2, data = your_data)")
  }
  
  if (ncol(response) < 2) {
    stop("This function requires at least 2 response variables.\n",
         "For single response variables, use predict() with interval='prediction'")
  }
  
  if (missing(newdata)) {
    stop("You must provide newdata with predictor values.\n",
         "Example: newdata = data.frame(predictor1 = 5, predictor2 = 10)")
  }
  
  if (conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be between 0 and 1 (e.g., 0.95 for 95%)")
  }
  
  # Get response variable names
  resp_names <- colnames(mod$coefficients)
  if (is.null(resp_names)) {
    resp_names <- c("Response 1", "Response 2")
  }
  
  # Create informative title
  title <- sprintf("%.0f%% Prediction Ellipse for %s and %s", 
                   conf.level * 100, resp_names[1], resp_names[2])
  
  # Get predictions for the new data
  predictions <- predict(mod, newdata)
  center <- c(predictions[1, 1], predictions[1, 2])
  
  # Calculate error covariance matrix
  X <- model.matrix(mod)
  Y <- mod$model[[1]]
  n <- nrow(Y)
  m <- ncol(Y)
  p <- ncol(X) - 1  # number of predictors (excluding intercept)
  
  # Residual covariance matrix
  residuals_matrix <- resid(mod)
  S <- crossprod(residuals_matrix) / (n - p - 1)
  
  # Calculate the radius based on Hotelling's T-squared
  # This accounts for both estimation error and inherent variability
  model_terms <- terms(mod)
  model_terms_no_response <- delete.response(model_terms)
  mf <- model.frame(model_terms_no_response, newdata, 
                    na.action = na.pass, xlev = mod$xlevels)
  z0 <- model.matrix(model_terms_no_response, mf, 
                     contrasts.arg = mod$contrasts)
  
  # F-statistic for multivariate prediction
  f_critical <- qf(conf.level, df1 = m, df2 = n - p - m)
  radius <- sqrt((m * (n - p - 1) / (n - p - m)) * f_critical * 
                  z0 %*% solve(t(X) %*% X) %*% t(z0))
  
  # Generate ellipse points (using car package)
  ellipse_points <- car::ellipse(center = center, shape = S, 
                                 radius = c(radius), draw = FALSE)
  
  # Create the plot
  plot(ellipse_points, type = "l", xlab = resp_names[1], 
       ylab = resp_names[2], main = title, ...)
  
  # Add the center point
  points(x = center[1], y = center[2], pch = 19, col = "red", cex = 1.2)
  
  # Add invisible return value for advanced users
  invisible(ellipse_points)
}
