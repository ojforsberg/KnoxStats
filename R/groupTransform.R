#' @title 
#' Transform from Separate Groups to a Data Frame
#'
#' @description
#' The groupTransform function transforms a list of group data into a data frame 
#' suitable for statistical analysis. 
#' 
#' @param groups A list of data vectors (e.g., list(group1_data, group2_data, ...))
#' @param names A character vector of group names corresponding to each list element
#' 
#' @return A data frame with two columns:
#' \itemize{
#'   \item \code{gv} - Factor variable containing group names
#'   \item \code{xv} - Numeric vector containing the measurements
#' }
#' 
#' @details
#' This function is particularly useful for preparing data for ANOVA, t-tests, 
#' or other statistical analyses that require data in "long" format. The function
#' takes separate group vectors stored in a list and combines them into a single
#' data frame with appropriate group labels. The resulting data frame follows the
#' data principle where each observation is a row and each variable is a column.
#' 
#' The group names are converted to factors, which is the appropriate data type
#' for categorical grouping variables in most statistical modeling functions in R.
#' The order of factor levels follows the order of names provided.
#' 
#' @examples
#' # Example 1: Basic usage with three groups
#' group1 <- c(23, 25, 26, 27, 28)
#' group2 <- c(30, 32, 34, 35, 36)
#' group3 <- c(40, 42, 43, 45, 46)
#' 
#' groups_list <- list(group1, group2, group3)
#' group_names <- c("Control", "Treatment_A", "Treatment_B")
#' 
#' df <- groupTransform(groups_list, group_names)
#' head(df)
#' str(df)
#' 
#' # Example 2: Using the result with statistical functions
#' # One-way ANOVA
#' anova_result <- aov(xv ~ gv, data = df)
#' summary(anova_result)
#' 
#' # Boxplot visualization
#' boxplot(xv ~ gv, data = df, 
#'         main = "Group Comparisons",
#'         xlab = "Group", 
#'         ylab = "Measurement")
#' 
#' # Example 3: Handling groups with different sample sizes
#' groups_unequal <- list(
#'   c(10, 12, 14),
#'   c(20, 22, 24, 26, 28),
#'   c(30, 32)
#' )
#' 
#' df_unequal <- groupTransform(groups_unequal, 
#'                             c("Small", "Medium", "Large"))
#' table(df_unequal$gv)  # Check group sizes
#'
#' @references
#' Wickham, H. (2014). Tidy data. \emph{Journal of Statistical Software}, 59(10), 1-23.
#' 
#' R Core Team (2023). R: A Language and Environment for Statistical Computing.
#' R Foundation for Statistical Computing, Vienna, Austria.
#' 
#' @seealso
#' \code{\link{data.frame}}, \code{\link{factor}}, \code{\link{rep}}, 
#' \code{\link{unlist}} for the underlying functions used in the transformation.
#' 
#' \code{\link{aov}}, \code{\link{lm}} for statistical analyses that commonly
#' use data in this format.
#'

#' 
#' @export

groupTransform <- function(groups, names) {
  data.frame(
    gv = factor(rep(names, sapply(groups, length))),
    xv = unlist(groups)
  )
}