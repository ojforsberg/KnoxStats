#' @title
#' VIF-Adjusted Regression Summary
#'
#' @description
#' Provides regression coefficient estimates adjusted for multicollinearity using
#' Variance Inflation Factors (VIF). This function calculates standard errors,
#' t-values, and p-values that account for inflated variance due to correlated
#' predictor variables.
#'
#' @param mod A linear regression model object created by \code{\link{lm}}.
#' @param showVIF Logical. If \code{TRUE}, includes VIF column in output.
#'   Default is \code{FALSE}.
#'
#' @return
#' A matrix containing adjusted regression results with columns for:
#' \itemize{
#'   \item{Estimate: Coefficient estimates (unchanged from original model)}
#'   \item{Std. Error: Standard errors adjusted for multicollinearity}
#'   \item{t value: t-statistics calculated using adjusted standard errors}
#'   \item{Pr(>|t|): p-values calculated using adjusted t-statistics}
#'   \item{VIF: Variance Inflation Factors (optional, if \code{showVIF = TRUE})}
#' }
#'
#' @details
#' When predictor variables in a regression model are highly correlated
#' (multicollinear), the standard errors of their coefficients become inflated,
#' making it harder to detect statistically significant relationships.
#'
#' This function addresses this issue by:
#' \enumerate{
#'   \item Calculating Variance Inflation Factors (VIF) for each predictor
#'   \item Adjusting standard errors by dividing them by the square root of VIF
#'   \item Recalculating t-statistics and p-values using these adjusted standard errors
#' }
#'
#' The adjusted results provide a more accurate picture of each predictor's
#' individual contribution when multicollinearity is present. Note that the
#' coefficient estimates themselves remain unchanged—only their standard errors
#' and associated statistics are adjusted.
#'
#' A common rule of thumb is that VIF values above 5-10 indicate problematic
#' multicollinearity that may require attention.
#'
#' @examples
#' # Load required package
#' library(car)
#'
#' # Example 1: Simple regression with correlated predictors
#' # Create data with correlated predictors
#' set.seed(123)
#' x1 <- rnorm(100)
#' x2 <- x1 + rnorm(100, sd = 0.5)  # x2 is correlated with x1
#' y <- 2 + 3*x1 + 4*x2 + rnorm(100)
#'
#' # Fit linear model
#' model <- lm(y ~ x1 + x2)
#'
#' # Get VIF-adjusted summary
#' summaryVIFA(model)
#'
#' # Show results with VIF column
#' summaryVIFA(model, showVIF = TRUE)
#'
#' # Example 2: Compare with standard summary
#' # Standard output (may show inflated standard errors)
#' summary(model)
#'
#' # VIF-adjusted output
#' summaryVIFA(model, showVIF = TRUE)
#'
#' # Example 3: Model with uncorrelated predictors
#' x3 <- rnorm(100)
#' x4 <- rnorm(100)
#' y2 <- 1 + 2*x3 + 3*x4 + rnorm(100)
#' model2 <- lm(y2 ~ x3 + x4)
#'
#' # VIF should be close to 1 for uncorrelated predictors
#' summaryVIFA(model2, showVIF = TRUE)
#'
#' @references
#' \itemize{
#'   \item Fox, John, and Sanford Weisberg. 2019. *An R Companion to Applied
#'   Regression*. 3rd ed. Thousand Oaks, CA: Sage.
#'
#'   \item Kutner, Michael H., Christopher J. Nachtsheim, John Neter, and
#'   William Li. 2005. *Applied Linear Statistical Models*. 5th ed.
#'   New York: McGraw-Hill.
#'
#'   \item Belsley, David A., Edwin Kuh, and Roy E. Welsch. 2004. *Regression
#'   Diagnostics: Identifying Influential Data and Sources of Collinearity*.
#'   Hoboken, NJ: Wiley.
#' }
#'
#' @seealso
#' \code{\link{lm}} for creating linear models,
#' \code{\link{summary.lm}} for standard regression summaries,
#' \code{\link[car]{vif}} for calculating Variance Inflation Factors,
#' \code{\link{pt}} for the t-distribution function used in p-value calculation
#'
#' @importFrom car vif
#' @importFrom stats pt
#' @export
summaryVIFA <- function(mod, showVIF = FALSE) {
  
  # Input validation
  if (!inherits(mod, "lm")) {
    stop("Input must be a linear model object created by lm()")
  }
  
  # Calculate VIF and get model summary
  vv <- car::vif(mod)
  ss <- summary(mod)
  nvars <- length(vv)
  
  # Initialize results matrix
  results <- matrix(NA, nrow = nvars + 1, ncol = 5)
  rownames(results) <- c("(Intercept)", attr(ss$terms, "term.labels"))
  colnames(results) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "VIF")
  
  # Fill intercept row (no adjustment needed)
  results[1, 1:4] <- ss$coefficients[1, ]
  results[1, 5] <- NA
  
  # Process each predictor
  for (i in 1:nvars) {
    row_idx <- i + 1
    
    # Calculate adjusted statistics
    sif <- sqrt(vv[i])  # Standard error inflation factor
    ase <- ss$coefficients[row_idx, 2] / sif  # Adjusted standard error
    ats <- ss$coefficients[row_idx, 1] / ase  # Adjusted t-statistic
    apv <- 2 * (1 - pt(abs(ats), df = ss$df[2]))  # Adjusted p-value
    
    # Fill results matrix
    results[row_idx, 1] <- ss$coefficients[row_idx, 1]  # Original estimate
    results[row_idx, 2] <- ase  # Adjusted standard error
    results[row_idx, 3] <- ats  # Adjusted t-statistic
    results[row_idx, 4] <- apv  # Adjusted p-value
    results[row_idx, 5] <- vv[i]  # VIF
  }
  
  # Return results with or without VIF column based on showVIF argument
  if (showVIF) {
    return(results)
  } else {
    return(results[, 1:4])
  }
}

