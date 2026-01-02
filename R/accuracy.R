#' @title
#' Classification Model Evaluation Metrics
#'
#' @description
#' A collection of functions to evaluate the performance of binary classification models.
#' These functions calculate common metrics like accuracy, precision, recall, F-scores,
#' and correlation coefficients from confusion matrices or logistic regression models.
#'
#' @param data A data frame (optional, currently not used in \code{accuracy}).
#' @param y A vector of true binary outcomes (0/1 or FALSE/TRUE). Defaults to \code{model$y}.
#' @param model A logistic regression model object from \code{glm()} with \code{family = binomial}.
#' @param t A threshold for converting probabilities to binary predictions (default = 0.5).
#'          For \code{Fscore()}, a confusion matrix or a threshold value.
#' @param rate Logical; if \code{TRUE} (default), returns accuracy as a proportion.
#'             If \code{FALSE}, returns the raw count of correct predictions.
#' @param beta Numeric weight for balancing precision and recall in F-score (default = 1).
#' @param confusion A 2x2 confusion matrix where rows are actual classes and columns are 
#'                  predicted classes. Format: \code{matrix(c(TP, FP, FN, TN), nrow = 2)}.
#'
#' @return
#' \itemize{
#'   \item \code{accuracy()}: Accuracy as a proportion (if \code{rate = TRUE}) or count
#'   \item \code{fnr()}: False negative rate
#'   \item \code{fpr()}: False positive rate
#'   \item \code{Fscore()}: F-score (F-beta score)
#'   \item \code{F1score()}: F1-score (harmonic mean of precision and recall)
#'   \item \code{MCC()}: Matthews Correlation Coefficient (-1 to +1)
#'   \item \code{phiCoef()}: Phi coefficient (similar to correlation)
#'   \item \code{precision()}: Precision (positive predictive value)
#'   \item \code{recall()}: Recall (sensitivity, true positive rate)
#' }
#'
#' @details
#' These functions help evaluate binary classification models where outcomes are
#' either positive (1/TRUE) or negative (0/FALSE). Understanding these metrics is
#' crucial for model selection and improvement.
#'
#' \strong{Key concepts:}
#' \itemize{
#'   \item \strong{True Positives (TP)}: Correctly predicted positives
#'   \item \strong{False Positives (FP)}: Negative cases predicted as positive (Type I error)
#'   \item \strong{False Negatives (FN)}: Positive cases predicted as negative (Type II error)
#'   \item \strong{True Negatives (TN)}: Correctly predicted negatives
#' }
#'
#' Most functions accept a 2x2 confusion matrix as input. The \code{accuracy()} function
#' works directly with logistic regression models from \code{glm()}.
#'
#' @section Thresholds in Classification:
#' In logistic regression, predicted probabilities are converted to binary predictions
#' using a threshold (default = 0.500). Changing this threshold affects all metrics.
#'
#' @examples
#' # Load required package
#' if (requireNamespace("stats", quietly = TRUE)) {
#'   # Create sample data
#'   set.seed(370)
#'   n <- 100
#'   x <- rnorm(n)
#'   y <- rbinom(n, 1, plogis(0.5 + 1.2*x))
#'   data <- data.frame(x = x, y = y)
#'   
#'   # Fit logistic regression model
#'   model <- glm(y ~ x, data = data, family = binomial)
#'   
#'   # Calculate accuracy with default threshold
#'   accuracy(model = model)
#'   
#'   # Calculate accuracy with different threshold
#'   accuracy(model = model, t = 0.6)
#'   
#'   # Create confusion matrix
#'   pred <- ifelse(model$fitted.values > 0.500, 1, 0)
#'   conf_matrix <- matrix(c(
#'     sum(pred == 1 & y == 1),  # TP
#'     sum(pred == 1 & y == 0),  # FP
#'     sum(pred == 0 & y == 1),  # FN
#'     sum(pred == 0 & y == 0)   # TN
#'   ), nrow = 2, byrow = TRUE)
#'   
#'   # Calculate various metrics
#'   precision(conf_matrix)
#'   recall(conf_matrix)
#'   F1score(conf_matrix)
#'   MCC(conf_matrix)
#'   
#'   # Compare F-scores with different beta values
#'   Fscore(conf_matrix, beta = 1)   # Equal weight (F1)
#'   Fscore(conf_matrix, beta = 0.5) # Emphasizes precision
#'   Fscore(conf_matrix, beta = 2)   # Emphasizes recall
#'   
#'   # Error rates
#'   fpr(conf_matrix)  # False positive rate
#'   fnr(conf_matrix)  # False negative rate
#' }
#'
#' @references
#' \itemize{
#'   \item James, Gareth, et al. 2021. \emph{An Introduction to Statistical Learning: With Applications in R}. 2nd ed. Springer.
#'   \item Kuhn, Max, and Kjell Johnson. 2019. \emph{Feature Engineering and Selection: A Practical Approach for Predictive Models}. CRC Press.
#'   \item Powers, David M. W. 2011. "Evaluation: From Precision, Recall and F-Measure to ROC, Informedness, Markedness and Correlation." \emph{Journal of Machine Learning Technologies} 2 (1): 37–63.
#'   \item Chicco, Davide, and Giuseppe Jurman. 2020. "The Advantages of the Matthews Correlation Coefficient (MCC) over F1 Score and Accuracy in Binary Classification Evaluation." \emph{BMC Genomics} 21 (1): 1–13.
#' }
#'
#' @seealso
#' \code{\link[stats]{glm}} for fitting logistic regression models
#' \code{\link[base]{table}} for creating confusion matrices
#' \code{\link[caret]{confusionMatrix}} from the caret package for comprehensive metrics
#'
#' @importFrom stats glm fitted.values
#' @export
accuracy <- function(data = NULL, y = model$y, model, t = 0.500, rate = TRUE) {
  if (!inherits(model, "glm")) {
    stop("The 'model' argument must be a glm object (logistic regression)")
  }
  
  pr.value <- model$fitted.values
  predictions <- pr.value > t
  reality <- y
  correct <- sum(predictions == reality, na.rm = TRUE)
  
  if (rate) {
    correct <- correct / length(reality)
  }
  
  return(correct)
}

#' @rdname accuracy
#' @export
Fscore <- function(confusion, beta = 1) {
  if (!is.matrix(confusion) || !all(dim(confusion) == c(2, 2))) {
    stop("Input must be a 2x2 confusion matrix")
  }
  
  prec <- precision(confusion)
  rec <- recall(confusion)
  
  if (prec + rec == 0) {
    return(0)
  }
  
  numerator <- (1 + beta^2) * prec * rec
  denominator <- (beta^2 * prec + rec)
  
  return(numerator / denominator)
}

#' @rdname accuracy
#' @export
F1score <- function(confusion) {
  if (!is.matrix(confusion) || !all(dim(confusion) == c(2, 2))) {
    stop("Input must be a 2x2 confusion matrix")
  }
  
  # Renaming for clarity
  TP <- confusion[1, 1]
  FP <- confusion[1, 2]
  FN <- confusion[2, 1]
  
  if (TP + FP == 0 || TP + FN == 0) {
    return(0)
  }
  
  return(2 * TP / (2 * TP + FP + FN))
}

#' @rdname accuracy
#' @export
MCC <- function(confusion) {
  if (!is.matrix(confusion) || !all(dim(confusion) == c(2, 2))) {
    stop("Input must be a 2x2 confusion matrix")
  }
  
  TP <- confusion[1, 1]
  FP <- confusion[1, 2]
  FN <- confusion[2, 1]
  TN <- confusion[2, 2]
  
  numerator <- TP * TN - FP * FN
  
  if (numerator == 0) {
    return(0)
  }
  
  denominator <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  
  return(numerator / denominator)
}

#' @rdname accuracy
#' @export
phiCoef <- function(confusion) {
  MCC(confusion)  # Phi coefficient is mathematically equivalent to MCC
}

#' @rdname accuracy
#' @export
precision <- function(confusion) {
  if (!is.matrix(confusion) || !all(dim(confusion) == c(2, 2))) {
    stop("Input must be a 2x2 confusion matrix")
  }
  
  TP <- confusion[1, 1]
  FP <- confusion[1, 2]
  
  if (TP + FP == 0) {
    return(0)
  }
  
  return(TP / (TP + FP))
}

#' @rdname accuracy
#' @export
recall <- function(confusion) {
  if (!is.matrix(confusion) || !all(dim(confusion) == c(2, 2))) {
    stop("Input must be a 2x2 confusion matrix")
  }
  
  TP <- confusion[1, 1]
  FN <- confusion[2, 1]
  
  if (TP + FN == 0) {
    return(0)
  }
  
  return(TP / (TP + FN))
}

#' @rdname accuracy
#' @export
fnr <- function(confusion) {
  if (!is.matrix(confusion) || !all(dim(confusion) == c(2, 2))) {
    stop("Input must be a 2x2 confusion matrix")
  }
  
  TP <- confusion[1, 1]
  FN <- confusion[2, 1]
  
  if (TP + FN == 0) {
    return(0)
  }
  
  return(FN / (TP + FN))
}

#' @rdname accuracy
#' @export
fpr <- function(confusion) {
  if (!is.matrix(confusion) || !all(dim(confusion) == c(2, 2))) {
    stop("Input must be a 2x2 confusion matrix")
  }
  
  FP <- confusion[1, 2]
  TN <- confusion[2, 2]
  
  if (FP + TN == 0) {
    return(0)
  }
  
  return(FP / (FP + TN))
}



#' @rdname accuracy
#' @export
sensitivity <- function(confusion) {
  if (!is.matrix(confusion) || !all(dim(confusion) == c(2, 2))) {
    stop("Input must be a 2x2 confusion matrix")
  }
  
  TP <- confusion[1, 1]
  FN <- confusion[2, 1]
  
  if (TP + FN == 0) {
    return(0)
  }
  
  return(TP / (TP + FN))
}


#' @rdname accuracy
#' @export
specificity <- function(confusion) {
  if (!is.matrix(confusion) || !all(dim(confusion) == c(2, 2))) {
    stop("Input must be a 2x2 confusion matrix")
  }
  
  FP <- confusion[1, 2]
  TN <- confusion[2, 2]
  
  if (FP + TN == 0) {
    return(0)
  }
  
  return(1 - FP / (FP + TN))
}

