#' @title
#' Calculate Statistical Intervals
#'
#' @description
#' Computes observation, confidence, or prediction intervals for a numeric vector.
#'
#' @param x A numeric vector of data values.
#' @param type Type of interval to compute. Options are:
#'   \itemize{
#'     \item{"observation" or "o": Nonparametric interval based on sample quantiles}
#'     \item{"confidence" or "c": Confidence interval for the population mean}
#'     \item{"prediction" or "p": Prediction interval for a single future observation}
#'   }
#'   The matching is case-insensitive.
#' @param level Confidence level for the interval, between 0 and 1 (default: 0.95).
#'
#' @return A list with three components:
#'   \item{upper}{Upper bound of the interval}
#'   \item{lower}{Lower bound of the interval}
#'   \item{level}{Confidence level used}
#'
#' @details
#' This function calculates three types of statistical intervals:
#'
#' \strong{Observation interval} (type = "observation" or "o"):
#' A nonparametric interval based on sample quantiles. For a level of 0.95,
#' it uses the 2.5th and 97.5th percentiles of the data. This interval contains
#' approximately the specified proportion of the observed data.
#'
#' \strong{Confidence interval} (type = "confidence" or "c"):
#' The standard t-interval for the population mean, calculated as:
#' \deqn{\bar{x} \pm t_{\alpha/2, n-1} \times \frac{s}{\sqrt{n}}}
#' where \eqn{\bar{x}} is the sample mean, \eqn{s} is the sample standard deviation,
#' \eqn{n} is the sample size, and \eqn{t_{\alpha/2, n-1}} is the critical value
#' from the t-distribution with \eqn{n-1} degrees of freedom.
#'
#' \strong{Prediction interval} (type = "prediction" or "p"):
#' An interval for a single future observation from the same population, calculated as:
#' \deqn{\bar{x} \pm t_{\alpha/2, n-1} \times s \times \sqrt{1 + \frac{1}{n}}}
#' This interval is wider than the confidence interval because it accounts for
#' both the uncertainty in estimating the mean and the variability of individual
#' observations.
#'
#' @examples
#' # Sample data
#' set.seed(123)
#' data <- rnorm(50, mean = 100, sd = 15)
#'
#' # Observation interval (nonparametric)
#' interval(data, type = "observation")
#' interval(data, type = "o", level = 0.90)
#'
#' # Confidence interval for the mean
#' interval(data, type = "confidence")
#' interval(data, type = "c", level = 0.99)
#'
#' # Prediction interval for a future observation
#' interval(data, type = "prediction")
#' interval(data, type = "p", level = 0.90)
#'
#' # Compare all three intervals on the same data
#' obs_int <- interval(data, type = "observation")
#' conf_int <- interval(data, type = "confidence")
#' pred_int <- interval(data, type = "prediction")
#'
#' cat("Observation interval: [", obs_int$lower, ",",
#' 
#' 
#' @export
interval <- function(x, type="observation", level=0.95) {

type = tolower(type)
n = length(x)
a2 = (1-level)/2

if(type=="o" || type=="observation") {
	ub = quantile(x,1-a2)
	lb = quantile(x,a2)
}

if(type=="c" || type=="confidence") {
	ub = mean(x)-qt(a2, df=n-1)*sd(x)/sqrt(n)
	lb = mean(x)+qt(a2, df=n-1)*sd(x)/sqrt(n)
}

if(type=="p" || type=="prediction") {
	ub = mean(x)-qt(a2, df=n-1)*sd(x)*sqrt(1+1/n)
	lb = mean(x)+qt(a2, df=n-1)*sd(x)*sqrt(1+1/n)
}

res = list(upper=as.numeric(ub), lower=as.numeric(lb), level=level)
return(res)
}