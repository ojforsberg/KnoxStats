#' @title
#' Working-Hotelling Confidence Bands for Simple Linear Regression
#'
#' @description
#' Creates Working-Hotelling confidence bands for simple linear regression models.
#' These bands provide a confidence region for the entire regression line rather
#' than just individual predictions.
#'
#' @param mod A simple linear regression model object created by \code{\link{lm}}.
#' @param newdata Optional data frame containing the predictor values for which
#'   to calculate confidence bands. If not provided, uses the original predictor
#'   values from the model.
#'
#' @return
#' A list with two components:
#' \item{lcb}{Lower confidence bounds for each predictor value}
#' \item{ucb}{Upper confidence bounds for each predictor value}
#'
#' @details
#' The Working-Hotelling procedure creates simultaneous confidence bands for the
#' entire regression line in simple linear regression. Unlike prediction intervals
#' for individual points, these bands account for the fact that we're making
#' inferences about the entire line simultaneously.
#'
#' The confidence bands are calculated using the formula:
#' \deqn{\hat{y} \pm \sqrt{2F_{2,n-2,\alpha}} \cdot \sqrt{MSE\left(\frac{1}{n} + \frac{(x-\bar{x})^2}{S_{xx}}\right)}}
#' where \eqn{F_{2,n-2,\alpha}} is the critical value from the F-distribution,
#' MSE is the mean squared error, \eqn{n} is the sample size, \eqn{\bar{x}} is
#' the mean of the predictor, and \eqn{S_{xx}} is the sum of squared deviations
#' of the predictor.
#'
#' The bands are wider than individual confidence intervals but guarantee that
#' the entire true regression line lies within them with the specified
#' confidence level (95% by default).
#'
#' @examples
#' # Simple linear regression example
#' # Create sample data
#' set.seed(123)
#' x <- 1:20
#' y <- 2 + 1.5*x + rnorm(20, 0, 3)
#' data <- data.frame(x = x, y = y)
#'
#' # Fit linear model
#' model <- lm(y ~ x, data = data)
#'
#' # Calculate Working-Hotelling confidence bands for original x values
#' wh_bands <- predictWH(model)
#' head(wh_bands$lcb)  # Lower bounds
#' head(wh_bands$ucb)  # Upper bounds
#'
#' # Plot the results with confidence bands
#' plot(data$x, data$y, main = "Working-Hotelling Confidence Bands",
#'      xlab = "Predictor", ylab = "Response", pch = 19)
#' abline(model, col = "blue", lwd = 2)
#' lines(data$x, wh_bands$lcb, col = "red", lty = 2)
#' lines(data$x, wh_bands$ucb, col = "red", lty = 2)
#' legend("topleft", legend = c("Data", "Regression Line", "WH Bands"),
#'        col = c("black", "blue", "red"), pch = c(19, NA, NA),
#'        lty = c(NA, 1, 2), lwd = c(NA, 2, 1))
#'
#' # Calculate bands for specific new x values
#' new_x <- data.frame(x = c(5, 10, 15))
#' new_bands <- predictWH(model, newdata = new_x)
#' new_bands
#'
#' # Compare with regular confidence intervals
#' regular_ci <- predict(model, newdata = new_x, interval = "confidence")
#' comparison <- data.frame(
#'   x = new_x$x,
#'   WH_lower = new_bands$lcb,
#'   Regular_lower = regular_ci[, "lwr"],
#'   WH_upper = new_bands$ucb,
#'   Regular_upper = regular_ci[, "upr"]
#' )
#' comparison
#'
#' @references
#' Working, Holbrook, and Hotelling, Harold. 1929. "Applications of the Theory
#' of Error to the Interpretation of Trends." *Journal of the American
#' Statistical Association* 24 (165A): 73–85.
#'
#' Kutner, Michael H., Christopher J. Nachtsheim, John Neter, and William Li.
#' 2005. *Applied Linear Statistical Models*. 5th ed. Boston: McGraw-Hill Irwin.
#'
#' @seealso
#' \code{\link{lm}} for fitting linear models, \code{\link{predict.lm}} for
#' individual predictions and confidence intervals, \code{\link{qf}} for the
#' F-distribution quantile function.
#'
#' @importFrom stats qf
#' @export
predictWH <- function(mod, newdata = NA) {
  
  # Input validation
  if (!inherits(mod, "lm")) {
    stop("The model must be a linear regression model created by lm().")
  }
  
  # Check for simple linear regression (one predictor plus intercept)
  if (length(mod$coefficients) != 2) {
    stop("This function only works with simple linear regression (one predictor).")
  }
  
  # Extract model components
  n <- nrow(mod$model)
  b0 <- as.numeric(mod$coefficients[1])
  b1 <- as.numeric(mod$coefficients[2])
  
  # Calculate MSE
  MSE <- sum(mod$residuals^2) / (n - 2)
  
  # Extract predictor variable
  predictor_name <- attr(mod$terms, "term.labels")[1]
  x <- mod$model[[predictor_name]]
  mux <- mean(x)
  Sxx <- sum((x - mux)^2)
  
  # Handle newdata
  if (identical(newdata, NA) || is.null(newdata)) {
    newdata <- data.frame(x = x)
    names(newdata) <- predictor_name
    newx <- x
  } else if (is.list(newdata)) {
    if (!is.data.frame(newdata)) {
      newdata <- as.data.frame(newdata)
    }
    if (!predictor_name %in% names(newdata)) {
      stop("newdata must contain the predictor variable: ", predictor_name)
    }
    newx <- newdata[[predictor_name]]
  } else {
    stop("newdata must be a data frame, list, or NULL")
  }
  
  # Calculate Working-Hotelling critical value (95% confidence)
  wh_critical <- sqrt(2 * qf(0.95, 2, n - 2))
  
  # Calculate confidence bands
  se <- sqrt(MSE * (1/n + (newx - mux)^2 / Sxx))
  predictions <- b0 + b1 * newx
  lcb <- predictions - wh_critical * se
  ucb <- predictions + wh_critical * se
  
  # Return results
  return(list(
    lcb = as.numeric(lcb),
    ucb = as.numeric(ucb),
    x_values = newx,
    predictor = predictor_name,
    confidence_level = 0.95
  ))
}

