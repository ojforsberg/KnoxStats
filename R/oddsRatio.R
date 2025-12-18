#' @title 
#' Odds Ratio Test for 2×2 Contingency Tables
#' 
#' @description
#' Performs a hypothesis test for the odds ratio in a 2×2 contingency table. 
#' This test determines whether there is a significant association between 
#' two categorical variables by comparing the odds of an event occurring 
#' in two different groups.
#' 
#' @param t A 2×2 matrix or table containing frequency counts. Rows typically 
#'   represent exposure groups (e.g., treatment vs. control), and columns 
#'   represent outcomes (e.g., success vs. failure).
#' @param alternative A character string specifying the alternative hypothesis:
#'   \code{"two.sided"} (default), \code{"less"}, or \code{"greater"}. 
#'   This determines whether the test looks for any difference (\code{"two.sided"}), 
#'   a smaller odds ratio (\code{"less"}), or a larger odds ratio (\code{"greater"}).
#' @param conf.level The confidence level for the confidence interval (default = 0.95). 
#'   Must be between 0 and 1.
#' @param mu The hypothesized value of the odds ratio under the null hypothesis 
#'   (default = 1, indicating no association). Must be positive.
#' 
#' @return An object of class \code{"htest"} containing:
#' \itemize{
#'   \item \code{statistic}: The test statistic (Z-score)
#'   \item \code{parameter}: NULL (not used in this test)
#'   \item \code{p.value}: The p-value for the test
#'   \item \code{conf.int}: Confidence interval for the odds ratio
#'   \item \code{estimate}: The estimated odds ratio
#'   \item \code{null.value}: The hypothesized odds ratio
#'   \item \code{alternative}: The alternative hypothesis
#'   \item \code{method}: The name of the test
#'   \item \code{data.name}: The name of the data
#' }
#' 
#' @details
#' The odds ratio measures the strength of association between two binary variables. 
#' It compares the odds of an event occurring in one group to the odds of it 
#' occurring in another group.
#' 
#' \deqn{OR = \frac{a \times d}{b \times c}}
#' where the 2×2 table has the structure:
#' \tabular{cccc}{
#'           \tab Success \tab Failure \tab Total\cr
#' Group 1  \tab a       \tab b       \tab a+b\cr
#' Group 2  \tab c       \tab d       \tab c+d
#' }
#' 
#' The test uses the log odds ratio, which is approximately normally distributed 
#' for large samples. The standard error is calculated using:
#' \deqn{SE(\log(OR)) = \sqrt{\frac{1}{a} + \frac{1}{b} + \frac{1}{c} + \frac{1}{d}}}
#' 
#' A p-value < 0.05 typically indicates a statistically significant association 
#' between the variables.
#' 
#' @examples
#' # Example 1: Vaccine efficacy study
#' # A study comparing infection rates between vaccinated and unvaccinated groups
#' vaccine_data <- matrix(c(20, 80, 100, 400), nrow = 2, 
#'                       dimnames = list(Treatment = c("Vaccine", "Placebo"),
#'                                       Outcome = c("Infected", "Not Infected")))
#' vaccine_data
#' 
#' # Two-sided test (default)
#' oddsRatio(vaccine_data)
#' 
#' # Test with 90% confidence interval
#' oddsRatio(vaccine_data, conf.level = 0.90)
#' 
#' # One-sided test: Is the odds ratio less than 1?
#' oddsRatio(vaccine_data, alternative = "less")
#' 
#' # Example 2: Smoking and lung cancer (classic example)
#' cancer_data <- matrix(c(688, 650, 21, 59), nrow = 2,
#'                      dimnames = list(Smoking = c("Yes", "No"),
#'                                      Cancer = c("Yes", "No")))
#' cancer_data
#' 
#' oddsRatio(cancer_data)
#' 
#' # Example 3: Test a specific null hypothesis
#' # Is the odds ratio different from 2?
#' oddsRatio(vaccine_data, mu = 2)
#' 
#' @references
#' Agresti, Alan. 2018. *An Introduction to Categorical Data Analysis*. 3rd ed. 
#' Hoboken, NJ: Wiley.
#' 
#' Fleiss, Joseph L., Bruce Levin, and Myunghee Cho Paik. 2003. *Statistical Methods 
#' for Rates and Proportions*. 3rd ed. Hoboken, NJ: Wiley.
#' 
#' Szumilas, Magdalena. 2010. "Explaining Odds Ratios." *Journal of the Canadian 
#' Academy of Child and Adolescent Psychiatry* 19 (3): 227–29.
#' 
#' @seealso
#' \code{\link{fisher.test}} for an exact test for 2×2 tables,
#' \code{\link{chisq.test}} for the chi-squared test of independence,
#' \code{\link{prop.test}} for comparing proportions,
#' \code{\link{epitools::oddsratio}} for alternative implementations
#' 
#' @importFrom stats pnorm qnorm
#' @export
oddsRatio <- function(t, alternative = c("two.sided", "less", "greater"), 
                      conf.level = 0.95, mu = 1) {
  
  # Input validation
  if (!is.matrix(t) || !all(dim(t) == c(2, 2))) {
    stop("Input must be a 2x2 matrix or table")
  }
  if (any(t <= 0)) {
    stop("All cell counts must be positive for valid odds ratio calculation")
  }
  if (mu <= 0) {
    stop("The null hypothesis value (mu) must be positive")
  }
  if (conf.level <= 0 || conf.level >= 1) {
    stop("Confidence level must be between 0 and 1")
  }
  
  alternative <- match.arg(alternative)
  
  # Calculate odds ratio and its properties
  OR <- (t[1, 1] * t[2, 2]) / (t[1, 2] * t[2, 1])
  lOR <- log(OR)
  se <- sqrt(sum(1 / t))  # Standard error of log odds ratio
  
  # Test statistic
  TS <- (lOR - log(mu)) / se
  
  # Initialize result structure
  res <- list(
    method = "Odds Ratio Test",
    data.name = deparse(substitute(t)),
    null.value = mu,
    alternative = alternative,
    conf.level = conf.level
  )
  names(res$null.value) <- "odds ratio"
  
  # Calculate p-value and confidence intervals based on alternative
  if (alternative == "two.sided") {
    res$p.value <- 2 * pnorm(-abs(TS))
    alpha <- (1 - conf.level) / 2
    z_critical <- qnorm(1 - alpha)
    lcl <- exp(lOR - z_critical * se)
    ucl <- exp(lOR + z_critical * se)
  } else if (alternative == "less") {
    res$p.value <- pnorm(TS)
    z_critical <- qnorm(conf.level)
    lcl <- 0
    ucl <- exp(lOR + z_critical * se)
  } else {  # alternative == "greater"
    res$p.value <- pnorm(TS, lower.tail = FALSE)
    z_critical <- qnorm(conf.level)
    lcl <- exp(lOR - z_critical * se)
    ucl <- Inf
  }
  
  # Complete result object
  res$statistic <- TS
  names(res$statistic) <- "Z"
  res$estimate <- OR
  names(res$estimate) <- "odds ratio"
  res$conf.int <- c(lcl, ucl)
  attr(res$conf.int, "conf.level") <- conf.level
  
  class(res) <- "htest"
  return(res)
}
