#' @title
#' Set Reference Level for a Factor
#'
#' @description
#' Changes the reference (base) level of a factor variable, reordering the levels
#' so that a specified level comes first. This is useful for statistical modeling
#' when you want to control which group is used as the reference category in 
#' regression models.
#' 
#' Arrrrrgh! Writing documentation is soooooo boring!
#'
#' @param var A factor variable whose reference level you want to change.
#' @param base A character string specifying the level that should become the 
#'   new reference (first) level of the factor.
#' @param data A data frame containing the factor variable (optional). If not 
#'   provided, `var` should be a factor vector.
#'
#' @return
#' A factor with reordered levels where `base` is the first level. All other 
#'   levels follow in their original order.
#'
#' @details
#' In statistical modeling, factors (categorical variables) need a reference level
#' against which other levels are compared. By default, R uses alphabetical order
#' to determine the reference level. This function allows you to specify which 
#' level should be the reference.
#'
#' For example, if you have a factor "Treatment" with levels "Placebo", "Low", 
#' and "High", R would use "High" as the reference (alphabetically). If you want 
#' "Placebo" as the reference instead, this function will reorder the levels to 
#' "Placebo", "High", "Low".
#'
#' The function checks that the specified `base` level actually exists in the 
#' factor. If it doesn't, an error message will explain the problem.
#'
#' @examples
#' # Create a sample factor
#' treatment <- factor(c("Placebo", "Drug", "Placebo", "Drug", "Placebo"))
#' levels(treatment)  # Shows: "Drug" "Placebo" (alphabetical order)
#'
#' # Set "Placebo" as the reference level
#' treatment_ref <- set.base(treatment, base = "Placebo")
#' levels(treatment_ref)  # Now: "Placebo" "Drug"
#'
#' # Using with a data frame
#' patients <- data.frame(
#'   id = 1:6,
#'   treatment = factor(c("A", "B", "C", "A", "B", "C")),
#'   response = c(10, 15, 12, 11, 14, 13)
#' )
#'
#' # Set "B" as reference level
#' patients$treatment <- set.base(patients$treatment, base = "B", data = patients)
#' levels(patients$treatment)  # Now: "B" "A" "C"
#'
#' # Error example - trying to use a non-existent level
#' try(set.base(treatment, base = "Control"))  # Will produce an error message
#'
#' # Real statistical application
#' # In regression, changing reference level affects interpretation:
#' summary(lm(response ~ treatment, data = patients))
#'
#' @seealso
#' \code{\link{factor}} for creating factors,
#' \code{\link{relevel}} for a base R function that does similar reordering,
#' \code{\link{C}} for setting contrasts directly,
#' \code{\link{lm}} for linear models where reference levels matter
#'
#' @references
#' Fox, John. 2015. *Applied Regression Analysis and Generalized Linear Models*. 
#' 3rd ed. Thousand Oaks, CA: Sage.
#'
#' Wickham, Hadley, and Garrett Grolemund. 2016. *R for Data Science: Import, 
#' Tidy, Transform, Visualize, and Model Data*. Sebastopol, CA: O'Reilly Media.
#'
#' @importFrom stats relevel
#' @export
set.base <- function(var, base, data = NULL) {
  
  # Get the factor from data or use var directly
  if (!is.null(data)) {
    var_name <- deparse(substitute(var))
    if (!var_name %in% names(data)) {
      stop("Variable '", var_name, "' not found in the data frame.")
    }
    factor_var <- data[[var_name]]
  } else {
    factor_var <- var
  }
  
  # Input validation
  if (!is.factor(factor_var)) {
    stop("'var' must be a factor variable.")
  }
  
  if (!is.character(base) || length(base) != 1) {
    stop("'base' must be a single character string.")
  }
  
  # Check if base exists in levels
  current_levels <- levels(factor_var)
  if (!base %in% current_levels) {
    stop("The base level '", base, "' is not a current factor level.\n",
         "Current levels are: ", paste(current_levels, collapse = ", "))
  }
  
  # Reorder levels: base first, then others in original order
  new_levels <- c(base, setdiff(current_levels, base))
  
  # Create the new factor
  result <- factor(factor_var, 
                   levels = new_levels, 
                   ordered = is.ordered(factor_var))
  
  return(result)
}

