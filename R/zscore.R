#' @title 
#' Calculate Z-Scores
#'
#' @description
#' Computes standardized z-scores for a numeric vector. Z-scores indicate how many 
#' standard deviations each value is from the mean, allowing comparison of data 
#' from different distributions or data sets.
#'
#' @param x A numeric vector for which to calculate z-scores.
#' @param na.rm Logical. Should missing values be removed? Default is `TRUE`.
#' @param names Optional character vector of names to assign to the output.
#'   If provided, this vector should have the same length as `x`. 
#'   Default is `NA` (no names).
#'
#' @return A numeric vector of z-scores with the same length as `x`. 
#'   If `names` is provided, the output will have just those names.
#'
#' @details
#' Z-scores (also called standard scores) transform data to have a mean of 0 
#' and a standard deviation of 1. This standardization process allows you to:
#' 
#' * Compare values from different data sets
#' * Identify outliers (typically values with |z-score| > 3)
#' * Understand how unusual a particular observation is
#' * Prepare data for certain statistical analyses
#' 
#' The formula used is: 
#' \deqn{z = \frac{x - \bar{x}}{s}}
#' where \eqn{\bar{x}} is the sample mean and \eqn{s} is the sample standard deviation.
#'
#' @section Warning:
#' When all values in `x` are identical, the standard deviation will be 0, 
#' resulting in division by zero. The function will return `NaN` for all values 
#' in this case with a warning message.
#'
#' @examples
#' # Basic example with exam scores
#' exam_scores <- c(85, 92, 78, 96, 82, 88, 75, 98, 81, 90)
#' zscore(exam_scores)
#' 
#' # With names for each score
#' student_names <- paste0("Student_", LETTERS[1:10])
#' zscore(exam_scores, names = student_names)
#' 
#' # Handling missing values
#' scores_with_na <- c(85, 92, NA, 96, 82, 88, NA, 98, 81, 90)
#' zscore(scores_with_na)
#' zscore(scores_with_na, na.rm = FALSE)
#' 
#' # Identifying outliers
#' data <- c(12, 15, 14, 13, 15, 100, 12, 14, 13)  # 100 is an outlier
#' z_scores <- zscore(data)
#' which(abs(z_scores) > 3)  # Identify position of outlier
#' 
#' # Comparing two different datasets
#' math_scores <- c(85, 92, 78, 96, 82)
#' english_scores <- c(88, 85, 92, 79, 84)
#' 
#' # Convert both to z-scores for comparison
#' math_z <- zscore(math_scores)
#' english_z <- zscore(english_scores)
#' 
#' # Now we can compare performance across subjects
#' cbind(math_scores, math_z, english_scores, english_z)
#'
#' @seealso
#' * \code{\link{scale}} for a base R alternative that can handle matrices
#' * \code{\link{mean}} and \code{\link{sd}} for the component functions
#' * \code{\link{pnorm}} for converting z-scores to probabilities
#' * \code{\link{qqnorm}} for checking if data follows a normal distribution
#'
#' @references
#' Devore, Jay L. 2011. *Probability and Statistics for Engineering and the Sciences*. 
#' 8th ed. Boston: Cengage Learning.
#' 
#' Moore, David S., George P. McCabe, and Bruce A. Craig. 2017. *Introduction to the 
#' Practice of Statistics*. 9th ed. New York: W. H. Freeman.
#'
#' Agresti, Alan, and Christine Franklin. 2013. *Statistics: The Art and Science of 
#' Learning from Data*. 3rd ed. Boston: Pearson.
#'
#' @export
zscore <- function(x, na.rm = TRUE, names = NULL) {
  # Input validation
  if (!is.numeric(x)) {
    stop("Input 'x' must be a numeric vector")
  }
  
  # Calculate mean and standard deviation
  m <- mean(x, na.rm = na.rm)
  s <- sd(x, na.rm = na.rm)
  
  # Check for zero standard deviation
  if (s == 0 && length(x) > 1) {
    warning("Standard deviation is 0. All values are identical, returning NaN.")
    result <- rep(NaN, length(x))
  } else {
    # Calculate z-scores
    result <- (x - m) / s
  }
  
  # Handle NA values when na.rm = FALSE
  if (!na.rm && any(is.na(x))) {
    warning("NAs present in data and na.rm = FALSE. Some z-scores will be NA.")
  }
  
  # Assign names if provided
  if (!is.null(names)) {
    if (length(names) != length(x)) {
      warning("Length of 'names' does not match length of 'x'. Names not assigned.")
    } else {
      names(result) <- names
    }
  }
  
  return(result)
}
