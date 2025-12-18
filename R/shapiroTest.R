#' @title 
#' Shapiro-Wilk Normality Test
#'
#' @description
#' Performs the Shapiro-Wilk test for normality on one or more groups of data.
#' This test checks whether a sample comes from a normally distributed population.
#' The function can handle both ungrouped data and grouped data (via a formula).
#' For multiple groups, p-values are adjusted using the Bonferroni correction.
#'
#' @param y A numeric vector of data values to test for normality.
#' @param g An optional grouping variable (factor or vector). If provided, 
#'          the test is performed separately for each group.
#' @param formula A formula of the form \code{response ~ group} where 
#'               \code{response} is a numeric vector and \code{group} is a 
#'               factor indicating group membership.
#' @param na.rm Logical indicating whether to remove missing values. 
#'             Default is \code{TRUE}.
#' @param ... Additional arguments (currently not used).
#'
#' @return
#' A list with the following components:
#' \itemize{
#'   \item \code{Title}: Name of the test
#'   \item \code{adjustment}: Type of adjustment used for multiple comparisons 
#'         (if applicable)
#'   \item \code{results}: A data frame containing the test results with columns:
#'     \itemize{
#'       \item \code{Level}: Group identifier (for grouped tests)
#'       \item \code{p.value}: P-value for the Shapiro-Wilk test
#'     }
#' }
#' For single groups, returns the standard \code{shapiro.test} output.
#'
#' @details
#' The Shapiro-Wilk test is one of the most powerful tests for detecting 
#' departures from normality. It tests the null hypothesis that the data 
#' come from a normally distributed population.
#'
#' \strong{Key points for students:}
#' \itemize{
#'   \item This test works best with sample sizes between 3 and 5,000.
#'   \item Groups with fewer than 3 observations or more than 5,000 are 
#'         automatically excluded with a warning.
#'   \item When testing multiple groups, the Bonferroni correction is applied 
#'         to control the family-wise error rate (the probability of making 
#'         at least one Type I error across all tests).
#'   \item A p-value less than 0.05 suggests the data are not normally 
#'         distributed.
#' }
#'
#' @examples
#' # Example 1: Test single ungrouped data
#' set.seed(123)
#' normal_data <- rnorm(50, mean = 100, sd = 15)
#' non_normal_data <- rexp(50, rate = 0.5)
#' 
#' shapiroTest(normal_data)     # Should give high p-value
#' shapiroTest(non_normal_data) # Should give low p-value
#' 
#' # Example 2: Test multiple groups using grouping variable
#' group_var <- rep(c("A", "B", "C"), each = 20)
#' values <- c(rnorm(20, 100, 10), rnorm(20, 110, 15), rexp(20, 0.1))
#' 
#' shapiroTest(y = values, g = group_var)
#' 
#' # Example 3: Test using formula syntax
#' data_df <- data.frame(
#'   score = c(rnorm(15, 85, 5), rnorm(15, 90, 10)),
#'   group = rep(c("Control", "Treatment"), each = 15)
#' )
#' 
#' shapiroTest(score ~ group, data = data_df)
#' 
#' # Example 4: Real dataset example
#' # Test if Sepal.Length in iris dataset is normally distributed
#' if (requireNamespace("datasets", quietly = TRUE)) {
#'   data(iris)
#'   shapiroTest(iris$Sepal.Length)
#'   
#'   # Test normality for each species
#'   shapiroTest(Sepal.Length ~ Species, data = iris)
#' }
#'
#' @importFrom stats shapiro.test aggregate model.frame
#' @importFrom utils head
#' 
#' @seealso
#' \code{\link[stats]{shapiro.test}} for the base R Shapiro-Wilk test,
#' \code{\link[stats]{qqnorm}} for visual normality checking with Q-Q plots,
#' \code{\link[stats]{aggregate}} for data aggregation,
#' \code{\link[stats]{p.adjust}} for other multiple testing adjustments
#'
#' @references
#' Shapiro, S. S., and M. B. Wilk. 1965. "An Analysis of Variance Test for 
#' Normality (Complete Samples)." *Biometrika* 52 (3/4): 591–611.
#' 
#' Field, Andy, Jeremy Miles, and Zoë Field. 2012. *Discovering Statistics 
#' Using R*. London: Sage Publications. (Chapter 5 covers normality tests)
#' 
#' McDonald, John H. 2014. *Handbook of Biological Statistics*. 3rd ed. 
#' Baltimore, Maryland: Sparky House Publishing. 
#' (Chapter 4.1 covers normality testing)
#'
#' @export
#' @export shapiroTest.default
#' @export shapiroTest.formula
shapiroTest <- function(y, ...) {
  UseMethod("shapiroTest")
}

#' @rdname shapiroTest
#' @method shapiroTest default
#' @export
shapiroTest.default <- function(y, g = NA, ...) {
  res <- list(Title = "Shapiro-Wilk Test")
  
  # Single group test
  if (is.na(g[1])) {
    return(shapiro.test(y))
  }
  
  # Multiple groups test
  lev <- unique(g)
  group_sizes <- table(g)
  
  # Check for groups with invalid sample sizes
  invalid_groups <- names(group_sizes)[group_sizes < 3 | group_sizes > 5000]
  
  # Warning for invalid groups
  if (length(invalid_groups) > 0) {
    warning(
      "The following groups have fewer than 3 or more than 5000 observations:\n",
      paste("  ", invalid_groups, collapse = "\n"),
      "\nThese groups have been excluded from analysis."
    )
    valid_groups <- setdiff(lev, invalid_groups)
  } else {
    valid_groups <- lev
  }
  
  # Apply Bonferroni correction
  adj <- length(valid_groups)
  res$adjustment <- paste("Bonferroni correction (k =", adj, ")")
  
  # Helper function to perform test with adjustment
  perform_test <- function(data_subset) {
    p_val <- shapiro.test(data_subset)$p.value * adj
    return(min(p_val, 1))  # Cap at 1
  }
  
  # Calculate p-values for each valid group
  results <- data.frame(
    Level = character(length(valid_groups)),
    p.value = numeric(length(valid_groups)),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(valid_groups)) {
    group <- valid_groups[i]
    group_data <- y[g == group]
    results$Level[i] <- as.character(group)
    results$p.value[i] <- perform_test(group_data)
  }
  
  res$results <- results
  class(res) <- "shapiroTest"
  return(res)
}

#' @rdname shapiroTest
#' @method shapiroTest formula
#' @export
shapiroTest.formula <- function(formula, data = NULL, na.rm = TRUE, ...) {
  # Validate formula
  if (missing(formula) || length(formula) != 3) {
    stop("Formula must be of the form: response ~ group")
  }
  
  # Extract data using model.frame
  mf <- model.frame(formula, data = data, na.action = if (na.rm) na.omit else na.pass)
  
  if (ncol(mf) != 2) {
    stop("Formula should have exactly one response and one grouping variable")
  }
  
  # Extract components and perform test
  response <- mf[[1]]
  group <- mf[[2]]
  
  return(shapiroTest.default(y = response, g = group))
}

# Print method for nice output
#' @export
print.shapiroTest <- function(x, ...) {
  cat(x$Title, "\n")
  if (!is.null(x$adjustment)) {
    cat("Adjustment:", x$adjustment, "\n\n")
  }
  
  if (!is.null(x$results)) {
    print(x$results, row.names = FALSE)
  } else {
    print(x[[1]])  # Print the standard shapiro.test output
  }
  invisible(x)
}
