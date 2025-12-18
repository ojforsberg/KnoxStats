#' @title 
#' Create GLM Link Functions
#' 
#' @description
#' This function creates link functions for use in generalized linear models (GLMs).
#' It returns a list containing the link function, its inverse, derivative, and
#' validation function for a specified link name.
#' 
#' @param link A character string specifying the link function. Supported links are:
#'   \describe{
#'     \item{\code{"logit"}}{Logit link for binomial models}
#'     \item{\code{"probit"}}{Probit (inverse normal) link}
#'     \item{\code{"cauchit"}}{Cauchit (inverse Cauchy) link}
#'     \item{\code{"cloglog"}}{Complementary log-log link}
#'     \item{\code{"identity"}}{Identity link for normal models}
#'     \item{\code{"log"}}{Log link for Poisson and other count models}
#'     \item{\code{"sqrt"}}{Square root link}
#'     \item{\code{"1/mu^2"}}{Inverse square link}
#'     \item{\code{"inverse"}}{Inverse link}
#'     \item{\code{"loglog"}}{Log-log link}
#'   }
#' 
#' @return
#' A list of class \code{"link-glm"} containing the following components:
#' \describe{
#'   \item{\code{linkfun}}{The link function: transforms the mean \code{mu} to the linear predictor \code{eta}}
#'   \item{\code{linkinv}}{The inverse link function: transforms the linear predictor \code{eta} back to the mean \code{mu}}
#'   \item{\code{mu.eta}}{Function giving the derivative \code{d mu / d eta}}
#'   \item{\code{valideta}}{Function indicating valid values for \code{eta}}
#'   \item{\code{name}}{Character string with the name of the link}
#' }
#' 
#' @details
#' Link functions are essential components of generalized linear models (GLMs) that
#' connect the mean of the response variable to the linear predictor. Each link
#' function has specific properties that make it suitable for different types of data:
#' 
#' \itemize{
#'   \item \strong{Logit}: Default for binomial GLMs (logistic regression). Suitable for 
#'         binary or proportional data bounded between 0 and 1.
#'   \item \strong{Probit}: Often used in bioassay and economics. Assumes normally 
#'         distributed latent variable.
#'   \item \strong{Cauchit}: Robust alternative to logit and probit, with heavier tails.
#'   \item \strong{Complementary log-log}: Asymmetric link for binary data where events 
#'         are rare or common.
#'   \item \strong{Identity}: Direct relationship between mean and linear predictor.
#'   \item \strong{Log}: Ensures positive means, used for count data (Poisson regression).
#'   \item \strong{Square root}: Variance-stabilizing link for count data.
#'   \item \strong{Inverse}: Used for gamma and inverse Gaussian models.
#'   \item \strong{Log-log}: Alternative asymmetric link for binary data.
#' }
#' 
#' The function includes numerical stability protections (e.g., bounding values 
#' away from machine precision limits) to prevent computational issues during 
#' model fitting.
#'
#' @examples
#' # Create different link functions
#' logit_link <- make.link("logit")
#' probit_link <- make.link("probit")
#' log_link <- make.link("log")
#' 
#' # Use the link function components
#' mu <- seq(0.1, 0.9, 0.1)
#' eta <- logit_link$linkfun(mu)  # Transform to linear predictor
#' mu_back <- logit_link$linkinv(eta)  # Transform back
#' 
#' # Check derivative at specific points
#' deriv <- logit_link$mu.eta(c(-2, 0, 2))
#' 
#' # Verify valid eta values
#' logit_link$valideta(c(-5, 0, 5))    # TRUE for all values
#' sqrt_link <- make.link("sqrt")
#' sqrt_link$valideta(c(-1, 0, 1))     # FALSE for non-positive values
#' 
#' # Create a custom GLM family with specific link
#' poisson_family <- poisson(link = make.link("log"))
#' 
#' @references
#' \itemize{
#'   \item McCullagh, P., & Nelder, J. A. (1989). Generalized Linear Models (2nd ed.).
#'         Chapman and Hall.
#'   \item Dobson, A. J., & Barnett, A. G. (2018). An Introduction to Generalized 
#'         Linear Models (4th ed.). Chapman and Hall/CRC.
#'   \item Fox, J. (2015). Applied Regression Analysis and Generalized Linear Models 
#'         (3rd ed.). Sage Publications.
#'   \item R Core Team (2023). R: A Language and Environment for Statistical Computing.
#'         R Foundation for Statistical Computing, Vienna, Austria.
#'         \url{https://www.R-project.org/}
#' }
#' 
#' @seealso
#' \code{\link{family}}, \code{\link{glm}}, \code{\link{binomial}}, 
#' \code{\link{poisson}}, \code{\link{Gamma}}
#' 
#' @export
make.link <- function(link) {
  # Validate input
  if (!is.character(link) || length(link) != 1) {
    stop("'link' must be a single character string")
  }
  
  # Define link functions using a more organized approach
  switch(link,
    logit = {
      linkfun <- function(mu) qlogis(mu)  # Use qlogis instead of C call
      linkinv <- function(eta) plogis(eta)  # Use plogis instead of C call
      mu.eta <- function(eta) {
        # More stable calculation of derivative for logit
        p <- plogis(eta)
        p * (1 - p)
      }
      valideta <- function(eta) TRUE
    },
    
    probit = {
      linkfun <- function(mu) qnorm(mu)
      linkinv <- function(eta) {
        # Numerical stability: bound away from extremes
        thresh <- -qnorm(.Machine$double.eps)
        eta <- pmin(pmax(eta, -thresh), thresh)
        pnorm(eta)
      }
      mu.eta <- function(eta) {
        # Ensure minimum value for stability
        pmax(dnorm(eta), .Machine$double.eps)
      }
      valideta <- function(eta) TRUE
    },
    
    cauchit = {
      linkfun <- function(mu) qcauchy(mu)
      linkinv <- function(eta) {
        thresh <- -qcauchy(.Machine$double.eps)
        eta <- pmin(pmax(eta, -thresh), thresh)
        pcauchy(eta)
      }
      mu.eta <- function(eta) {
        pmax(dcauchy(eta), .Machine$double.eps)
      }
      valideta <- function(eta) TRUE
    },
    
    cloglog = {
      linkfun <- function(mu) log(-log(1 - mu))
      linkinv <- function(eta) {
        # Bounded between machine epsilon and 1-epsilon
        pmax(pmin(-expm1(-exp(eta)), 1 - .Machine$double.eps), .Machine$double.eps)
      }
      mu.eta <- function(eta) {
        # Numerical stability for large eta
        eta <- pmin(eta, 700)
        pmax(exp(eta) * exp(-exp(eta)), .Machine$double.eps)
      }
      valideta <- function(eta) TRUE
    },
    
    identity = {
      linkfun <- function(mu) mu
      linkinv <- function(eta) eta
      mu.eta <- function(eta) rep.int(1, length(eta))
      valideta <- function(eta) TRUE
    },
    
    log = {
      linkfun <- function(mu) log(mu)
      linkinv <- function(eta) {
        pmax(exp(eta), .Machine$double.eps)
      }
      mu.eta <- function(eta) {
        pmax(exp(eta), .Machine$double.eps)
      }
      valideta <- function(eta) TRUE
    },
    
    sqrt = {
      linkfun <- function(mu) sqrt(mu)
      linkinv <- function(eta) eta^2
      mu.eta <- function(eta) 2 * eta
      valideta <- function(eta) all(is.finite(eta) & eta >= 0)
    },
    
    `1/mu^2` = {
      linkfun <- function(mu) 1 / mu^2
      linkinv <- function(eta) 1 / sqrt(eta)
      mu.eta <- function(eta) -1 / (2 * eta^1.5)
      valideta <- function(eta) all(is.finite(eta) & eta > 0)
    },
    
    inverse = {
      linkfun <- function(mu) 1 / mu
      linkinv <- function(eta) 1 / eta
      mu.eta <- function(eta) -1 / (eta^2)
      valideta <- function(eta) all(is.finite(eta) & eta != 0)
    },
    
    loglog = {
      linkfun <- function(mu) -log(-log(mu))
      linkinv <- function(eta) exp(-exp(-eta))
      mu.eta <- function(eta) exp(-exp(-eta) - eta)
      valideta <- function(eta) all(is.finite(eta))
    },
    
    # If link not recognized
    stop(sQuote(link), " link not recognized. See ?make.link for supported links.")
  )
  
  # Return link object
  structure(
    list(
      linkfun = linkfun,
      linkinv = linkinv,
      mu.eta = mu.eta,
      valideta = valideta,
      name = link
    ),
    class = "link-glm"
  )
}
