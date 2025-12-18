#' @title
#' Bayes Law Calculator
#'
#' @description
#' `bayesLaw` performs basic calculations using Bayes Law
#'
#' 
#' @param conmat Confusion matrix (2x2) for the test
#' @param pwild Incidence in the population
#' @param sensitivity True positive rate for the test
#' @param specificity True negative rate for the test
#' 
#' @return The function returns the probability that the patient is diseased, given the disease prevalence in the population and either the confusion matrix or the sensitivity and specificity.
#' 
#' @details
#' This function calculates the posterior probability of disease given a positive test result
#' (positive predictive value) using Bayes' theorem. The calculation requires:
#' 
#' \deqn{P(Disease|Positive) = \frac{P(Positive|Disease) \times P(Disease)}{P(Positive)}}
#' 
#' where \eqn{P(Positive)} can be expanded as:
#' \deqn{P(Positive) = P(Positive|Disease) \times P(Disease) + P(Positive|No Disease) \times P(No Disease)}
#' 
#' The function can accept input in two different formats:
#' \enumerate{
#'   \item A 2x2 confusion matrix along with population incidence
#'   \item Sensitivity and specificity values along with population incidence
#' }
#' 
#' When using a confusion matrix, the function will extract sensitivity and specificity 
#' from the matrix. The confusion matrix should be structured as:
#' \preformatted{
#'           Predicted
#' Actual    Positive Negative
#'   Positive TP      FN
#'   Negative FP      TN
#' }
#' 
#' Note: All probabilities should be provided as values between 0 and 1.
#' 
#' @examples
#' # Using sensitivity and specificity
#' bayesLaw(sensitivity = 0.95, specificity = 0.90, pwild = 0.01)
#' 
#' # Using a confusion matrix
#' conf_matrix <- matrix(c(95, 5, 10, 90), nrow = 2, byrow = TRUE,
#'                      dimnames = list(c("Diseased", "Healthy"),
#'                                      c("Test+", "Test-")))
#' bayesLaw(conmat = conf_matrix, pwild = 0.01)
#' 
#' @seealso
#' For more information on Bayes' theorem: \url{https://en.wikipedia.org/wiki/Bayes\%27_theorem}
#' 
#'
#' @export
bayesLaw <- function(
	conmat=NULL, 
	pwild=NULL, 
	sensitivity=NULL, 
	specificity=NULL
	) {

pdis = acc = fpr = fnr = NULL

## Have a table?
if( !is.null(conmat) ) {
  # Parse the table
  TP = conmat[1,1]
  FN = conmat[1,2]
  FP = conmat[2,1]
  TN = conmat[2,2]
    
  sensitivity = TP/(TP+FN)
  specificity = TN/(FP+TN)
  acc  = (TP+TN)/sum(conmat)
} 


## No table
if( !is.null(sensitivity) & !is.null(specificity) ) {
    acc = NULL
}

# Calculate some accuracy statistics
  fpr  = 1 - specificity
  fnr  = 1 - sensitivity

# Prepare to use Bayes Law
if( !is.null(pwild) ) {

  # Calculate P[D|+]
    num = sensitivity*pwild
    den = num+( (fpr)*(1-pwild) )
    pdis = num/den
}

# Prepare output as a list
  res = list(title="Bayes Law Calculator")
  res$Sensitivity = sensitivity
  res$Specificity = specificity
  res$FPR = fpr
  res$FNR = fnr
  res$DiseasePrevalence = pwild
  res$Accuracy = acc
  res$ConfusionMatrix=conmat
  res$ProbDiseased = pdis

# Return output
return(res)
}