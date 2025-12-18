#' @title
#' Runs Test for Randomness
#'
#' @description
#' Performs a nonparametric runs test to determine if a sequence of observations 
#' is random. This test helps identify non-random patterns, trends, or clustering 
#' in your data.
#'
#' @param e A numeric vector or time series containing the residuals or values 
#'          to test for randomness.
#' @param orderBy A vector used to order \code{e} before testing. Defaults to 
#'               the original order of \code{e}.
#' @param alternative The alternative hypothesis. Must be one of \code{"two.sided"}
#'                   (default), \code{"positive.correlated"}, or 
#'                   \code{"negative.correlated"}.
#'
#' @return
#' A list of class \code{"htest"} containing:
#' \itemize{
#'   \item \code{statistic}: The standardized runs statistic
#'   \item \code{p.value}: The p-value for the test
#'   \item \code{alternative}: A character string describing the alternative hypothesis
#'   \item \code{method}: A character string indicating the test performed
#'   \item \code{data.name}: A character string giving the names of the data
#' }
#'
#' @details
#' The runs test (also called the Wald-Wolfowitz test) examines a sequence of 
#' binary data (typically residuals coded as above/below median or mean) to test 
#' the null hypothesis that the sequence is random. A "run" is a consecutive 
#' sequence of identical values. If there are too few or too many runs compared 
#' to what we'd expect by chance, it suggests the data may not be random.
#'
#' This function is particularly useful in regression analysis for checking if 
#' residuals are randomly distributed. If residuals are not random, it may 
#' indicate problems with your model such as:
#' \itemize{
#'   \item Non-linear relationships not captured by the model
#'   \item Omitted variables
#'   \item Time-dependent patterns (in time series data)
#' }
#'
#' The test converts continuous data to binary by comparing each value to the 
#' median (default) or mean, then counts the number of runs in the resulting 
#' binary sequence.
#'
#' @examples
#' \dontrun{
#' # Load required package
#' library(lawstat)
#' 
#' # Example 1: Testing random numbers
#' set.seed(123)
#' random_data <- rnorm(50)
#' 
#' # Test if random_data appears random
#' result1 <- runs.test(random_data)
#' result1
#' 
#' # Example 2: Testing regression residuals for randomness
#' # Create some data with a linear relationship
#' x <- 1:20
#' y <- 2*x + rnorm(20)
#' 
#' # Fit a linear model
#' model <- lm(y ~ x)
#' residuals <- resid(model)
#' 
#' # Test if residuals are random
#' result2 <- runs.test(residuals, orderBy = x)
#' result2
#' 
#' # Example 3: Testing with clear pattern (should reject randomness)
#' pattern_data <- rep(c(1, -1), each = 10)  # Clear pattern: 10 ones, then 10 negative ones
#' result3 <- runs.test(pattern_data)
#' result3
#' 
#' # Example 4: One-sided test for positive correlation
#' trend_data <- 1:50 + rnorm(50, sd = 2)
#' result4 <- runs.test(trend_data, alternative = "positive.correlated")
#' result4
#' }
#'
#' @references
#' Wald, Abraham, and Jacob Wolfowitz. 1940. "On a Test Whether Two Samples Are 
#' from the Same Population." *Annals of Mathematical Statistics* 11 (2): 147–62.
#' 
#' Gibbons, Jean Dickinson, and Subhabrata Chakraborti. 2011. *Nonparametric 
#' Statistical Inference*. 5th ed. Boca Raton, FL: Chapman & Hall/CRC.
#' 
#' National Institute of Standards and Technology. 2012. *Engineering Statistics 
#' Handbook*. Gaithersburg, MD: NIST. https://www.itl.nist.gov/div898/handbook/.
#'
#' @seealso
#' \code{\link[lawstat]{runs.test}} for the underlying implementation,
#' \code{\link{lm}} for fitting linear models,
#' \code{\link{resid}} for extracting model residuals,
#' \code{\link{sample}} for generating random data
#'
#' @importFrom lawstat runs.test
#' @export
runs.test <- function(e, orderBy = e, alternative = "two.sided") {
  
  # Validate input
  if (!is.vector(e) || !is.numeric(e)) {
    stop("Argument 'e' must be a numeric vector")
  }
  
  if (length(e) < 3) {
    stop("Need at least 3 observations to perform runs test")
  }
  
  # Ensure alternative is valid
  valid_alternatives <- c("two.sided", "positive.correlated", "negative.correlated")
  if (!alternative %in% valid_alternatives) {
    stop("'alternative' must be one of: ", 
         paste(valid_alternatives, collapse = ", "))
  }
  
  # Order the data if specified
  if (length(orderBy) != length(e)) {
    stop("'e' and 'orderBy' must have the same length")
  }
  
  ordered_e <- e[order(orderBy)]
  
  # Perform the runs test
  result <- lawstat::runs.test(ordered_e, alternative = alternative)
  
  # Create informative data name
  e_name <- deparse(substitute(e))
  order_name <- deparse(substitute(orderBy))
  result$data.name <- paste(e_name, ", ordered by ", order_name, sep = "")
  
  return(result)
}
