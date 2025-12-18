#' @title 
#' Link and Inverse Link Functions
#'
#' @description
#' A collection of link functions and their inverses commonly used in 
#' generalized linear models (GLMs). These functions transform between 
#' the response scale (0-1 for binomial family) and the linear predictor scale.
#'
#' @param x Numeric vector. For link functions: values on the probability 
#'   scale (typically between 0 and 1). For inverse link functions: values 
#'   on the linear predictor scale (typically real numbers).
#'
#' @return
#' Numeric vector of transformed values:
#' - Link functions: transform from probability to linear predictor scale
#' - Inverse link functions: transform from linear predictor to probability scale
#'
#' @details
#' These functions implement various link functions for binomial GLMs:
#' \describe{
#'   \item{\code{logit}}{Logit (log-odds) transformation, canonical link for 
#'         binomial family. Maps (0,1) to (-∞, ∞).}
#'   \item{\code{probit}}{Inverse standard normal CDF. Similar to logit but 
#'         with heavier tails.}
#'   \item{\code{cauchit}}{Cauchy link function. Robust to outliers but 
#'         has infinite variance.}
#'   \item{\code{cloglog}}{Complementary log-log link. Asymmetric link 
#'         function for binary data where one outcome is rare.}
#'   \item{\code{loglog}}{Log-log link. Asymmetric link function, the 
#'         complement of cloglog.}
#' }
#' 
#' All inverse functions are available as \code{<link>.inv} (e.g., 
#' \code{logit.inv}) or as separate functions (\code{logistic} = \code{logit.inv}).
#'
#' @section Mathematical Definitions:
#' \describe{
#'   \item{Logit}{logit(p) = log(p/(1-p))}
#'   \item{Probit}{probit(p) = Φ^(-1)(p) where Φ is standard normal CDF}
#'   \item{Cauchit}{cauchit(p) = tan(π(p - 0.5))}
#'   \item{Cloglog}{cloglog(p) = log(-log(1-p))}
#'   \item{Loglog}{loglog(p) = -log(-log(p))}
#' }
#'
#' @examples
#' # Basic usage of link functions
#' p <- seq(0.1, 0.9, by = 0.1)
#' 
#' # Logit and inverse
#' eta <- logit(p)
#' p_recovered <- logit.inv(eta)
#' all.equal(p, p_recovered)  # Should be TRUE
#' 
#' # Alternative name for logit inverse
#' logistic(0)  # Returns 0.5
#' 
#' # Probit transformation
#' eta_probit <- probit(p)
#' probit.inv(eta_probit)  # Recovers original probabilities
#' 
#' # Compare different link functions
#' p <- 0.75
#' c(
#'   logit = logit(p),
#'   probit = probit(p),
#'   cauchit = cauchit(p),
#'   cloglog = cloglog(p),
#'   loglog = loglog(p)
#' )
#' 
#' # Visualize link functions
#' \dontrun{
#' p <- seq(0.01, 0.99, length.out = 100)
#' plot(p, logit(p), type = "l", col = "red", ylim = c(-5, 5),
#'      main = "Link Functions", xlab = "Probability", ylab = "Linear Predictor")
#' lines(p, probit(p), col = "blue")
#' lines(p, cauchit(p), col = "green")
#' lines(p, cloglog(p), col = "orange")
#' lines(p, loglog(p), col = "purple")
#' legend("topleft", legend = c("logit", "probit", "cauchit", "cloglog", "loglog"),
#'        col = c("red", "blue", "green", "orange", "purple"), lty = 1)
#' }
#'
#' @seealso
#' \code{\link[stats]{family}} for GLM families in base R,
#' \code{\link[stats]{make.link}} for creating link functions in R,
#' \code{\link{qnorm}}, \code{\link{pnorm}} for normal quantile/probability functions
#'
#' @references
#' McCullagh, P., & Nelder, J. A. (1989). *Generalized Linear Models* 
#' (2nd ed.). Chapman and Hall.
#' 
#' Agresti, A. (2012). *Categorical Data Analysis* (3rd ed.). Wiley.
#' 
#' Fox, J. (2015). *Applied Regression Analysis and Generalized Linear Models* 
#' (3rd ed.). Sage Publications.
#'
#' @name link-functions
NULL

#' @rdname link-functions
#' @export
logit <- function(x) {
  if (any(x <= 0 | x >= 1, na.rm = TRUE)) {
    warning("logit: x values should be in (0,1)")
  }
  log(x / (1 - x))
}

#' @rdname link-functions
#' @export
logit.inv <- function(x) {
  result <- exp(x) / (1 + exp(x))
  # Handle extreme values more robustly
  result[is.infinite(x) & x > 0] <- 1
  result[is.infinite(x) & x < 0] <- 0
  result
}

#' @rdname link-functions
#' @export
logistic <- logit.inv

#' @rdname link-functions
#' @export
probit <- function(x) {
  if (any(x < 0 | x > 1, na.rm = TRUE)) {
    warning("probit: x values should be in [0,1]")
  }
  qnorm(x)
}

#' @rdname link-functions
#' @export
probit.inv <- function(x) {
  pnorm(x)
}

#' @rdname link-functions
#' @export
cauchit <- function(x) {
  if (any(x < 0 | x > 1, na.rm = TRUE)) {
    warning("cauchit: x values should be in [0,1]")
  }
  tan(pi * (x - 0.5))
}

#' @rdname link-functions
#' @export
cauchit.inv <- function(x) {
  0.5 + atan(x) / pi
}

#' @rdname link-functions
#' @export
cloglog <- function(x) {
  if (any(x < 0 | x > 1, na.rm = TRUE)) {
    warning("cloglog: x values should be in [0,1]")
  }
  log(-log(1 - x))
}

#' @rdname link-functions
#' @export
cloglog.inv <- function(x) {
  1 - exp(-exp(x))
}

#' @rdname link-functions
#' @export
loglog <- function(x) {
  if (any(x < 0 | x > 1, na.rm = TRUE)) {
    warning("loglog: x values should be in [0,1]")
  }
  -log(-log(x))
}

#' @rdname link-functions
#' @export
loglog.inv <- function(x) {
  exp(-exp(-x))
}
