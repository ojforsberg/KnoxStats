#' @title 
#' Multiple Means Calculator
#'
#' @description
#' Calculates arithmetic, geometric, and harmonic means with optional trimming
#' of extreme values. Handles missing values and validates input parameters.
#'
#' @param x A numeric vector of values.
#' @param type Type of mean to calculate. Options are:
#'   \itemize{
#'     \item{\code{"arithmetic"}: Arithmetic mean (default when non-positive values present)}
#'     \item{\code{"geometric"}: Geometric mean}
#'     \item{\code{"harmonic"}: Harmonic mean}
#'     \item{\code{"all"}: Returns all three means (default)}
#'   }
#' @param trim Proportion of observations to trim from each end before 
#'   calculating the mean. Must be between 0 and 0.5. Default is 0 (no trimming).
#' @param na.rm Logical. Should missing values be removed? Default is \code{FALSE}.
#'
#' @return
#' A list containing:
#' \itemize{
#'   \item{When \code{type = "all"}: A list with components \code{Arithmetic},
#'     \code{Geometric}, \code{Harmonic}, and \code{trim}}
#'   \item{Otherwise: A list with components \code{Mean} (the requested mean)
#'     and \code{trim}}
#' }
#' 
#' @details
#' This function calculates three types of means:
#' \itemize{
#'   \item{\strong{Arithmetic mean}: The sum of values divided by the count,
#'     optionally with trimming. Uses \code{\link[base]{mean}} for calculation.}
#'   \item{\strong{Geometric mean}: The nth root of the product of n values,
#'     calculated as \code{exp(mean(log(x)))}. Only valid for positive values.}
#'   \item{\strong{Harmonic mean}: The reciprocal of the arithmetic mean of
#'     reciprocals, calculated as \code{n / sum(1/x)}. Only valid for positive,
#'     non-zero values.}
#' }
#' 
#' When the input contains non-positive values (≤ 0), only the arithmetic mean
#' can be calculated. In such cases, the function will:
#' \enumerate{
#'   \item{Issue a warning}
#'   \item{Override the \code{type} parameter to \code{"arithmetic"}}
#'   \item{Return only the arithmetic mean}
#' }
#' 
#' The \code{trim} parameter removes a proportion of observations from each end
#' of the sorted data before calculation. Trimming is useful for reducing the
#' influence of outliers. The function validates and adjusts the trim value:
#' \itemize{
#'   \item{Negative values are set to 0}
#'   \item{Values > 0.5 are set to 0.5}
#'   \item{Integer values between 1 and n/2 are interpreted as counts and
#'     converted to proportions}
#' }
#'
#' @examples
#' # Basic examples
#' x <- c(1, 2, 3, 4, 5)
#' 
#' # Get all three means
#' means(x, type = "all")
#' 
#' # Get only arithmetic mean with 20% trimming
#' means(x, type = "arithmetic", trim = 0.2)
#' 
#' # Get only geometric mean
#' means(x, type = "geometric")
#' 
#' # Handle missing values
#' y <- c(1, 2, NA, 4, 5)
#' means(y, na.rm = TRUE)
#' 
#' # Non-positive values force arithmetic mean only
#' z <- c(-1, 2, 3, 4, 5)
#' means(z, type = "all")  # Warning issued
#' 
#' # Trimmed mean examples
#' data <- c(1, 2, 3, 4, 100)  # 100 is an outlier
#' means(data, type = "arithmetic")  # Influenced by outlier
#' means(data, type = "arithmetic", trim = 0.2)  # More robust
#' 
#' # Real-world example: Growth rates
#' growth_rates <- c(1.05, 1.08, 1.03, 1.06, 1.04)
#' means(growth_rates, type = "geometric")  # Appropriate for growth rates
#'
#' @seealso
#' \code{\link[base]{mean}} for the base R mean function,
#' \code{\link[stats]{weighted.mean}} for weighted means,
#' \code{\link[psych]{geometric.mean}} and \code{\link[psych]{harmonic.mean}}
#'   in the psych package for alternative implementations,
#' \code{\link[base]{trimws}} for the concept of trimming
#'
#' @references
#' Wikipedia contributors, "Pythagorean means," Wikipedia, The Free Encyclopedia, 
#' https://en.wikipedia.org/w/index.php?title=Pythagorean_means&oldid=1327929429 
#' (accessed December 18, 2025).
#'
#'
#' @export
means <- function(x, type = "all", trim = 0, na.rm = FALSE) {
  
  # Input validation
  if (!is.numeric(x)) {
    stop("Input 'x' must be a numeric vector", call. = FALSE)
  }
  
  if (!is.character(type) || length(type) != 1) {
    stop("'type' must be a single character string", call. = FALSE)
  }
  
  valid_types <- c("arithmetic", "geometric", "harmonic", "all")
  if (!type %in% valid_types) {
    stop("'type' must be one of: ", 
         paste(valid_types, collapse = ", "), 
         call. = FALSE)
  }
  
  if (!is.numeric(trim) || length(trim) != 1) {
    stop("'trim' must be a single numeric value", call. = FALSE)
  }
  
  # Handle missing values
  if (na.rm) {
    x <- x[!is.na(x)]
  } else if (any(is.na(x))) {
    stop("Missing values present. Use na.rm = TRUE to remove them.", call. = FALSE)
  }
  
  # Check for non-positive values if geometric or harmonic mean requested
  if (any(x <= 0) && type != "arithmetic") {
    warning("Variable contains non-positive values. ",
            "Only arithmetic mean will be returned.", 
            call. = FALSE)
    type <- "arithmetic"
  }
  
  n <- length(x)
  
  # Validate and adjust trim parameter
  if (trim < 0) {
    trim <- 0
  } else if (trim > 0.5 && trim < 1.0) {
    trim <- 0.5
  } else if (trim >= 1 && trim <= n / 2) {
    # Interpret as count of observations to trim
    trim <- trim / n
  } else if (trim > n / 2) {
    trim <- 0.5
  }
  
  # Apply trimming
  if (trim > 0) {
    x_sorted <- sort(x)
    cut_no <- floor(n * trim)
    if (cut_no > 0) {
      x <- x_sorted[(cut_no + 1):(n - cut_no)]
      n <- length(x)  # Update n after trimming
    }
  }
  
  # Calculate means
  if (type == "arithmetic" || type == "all") {
    ma <- mean(x)
  }
  
  if (type == "geometric" || type == "all") {
    mg <- exp(mean(log(x)))
  }
  
  if (type == "harmonic" || type == "all") {
    mh <- n / sum(1 / x)
  }
  
  # Return results
  if (type == "all") {
    return(list(
      Arithmetic = ma,
      Geometric = mg,
      Harmonic = mh,
      trim = trim,
      n_used = n
    ))
  } else {
    mean_value <- switch(type,
      "arithmetic" = ma,
      "geometric" = mg,
      "harmonic" = mh
    )
    return(list(
      Mean = mean_value,
      trim = trim,
      n_used = n
    ))
  }
}
