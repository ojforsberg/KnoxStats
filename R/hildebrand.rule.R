#' @title 
#' The Hildebrand Rule
#' 
#' @description
#' Calculates the Hildebrand ratio and uses it to determine if the data are "sufficiently" skewed and in which direction.
#' 
#' @param x Numeric vector
#' @param ... Additional parameters passed to the instances of this function
#' 
#' @return 
#' A data frame containing the group name, the Hildebrand ratio, and whether or not (and in which direction) the data are skewed.
#' 
#'#' @details 
#' The Hildebrand Rule is a simple test for assessing data skewness by comparing the mean and median. 
#' This implementation calculates the Hildebrand ratio as:
#' 
#' \deqn{H = \frac{\bar{x} - M}{s}}{H = (mean(x) - median(x)) / sd(x)}
#' 
#' Where \eqn{\bar{x}} is the sample mean, \eqn{M} is the median, and \eqn{s} is the sample standard deviation.
#' 
#' The function interprets the ratio as follows:
#' \itemize{
#'   \item \code{H > 0.2}: Data are "sufficiently" right-skewed (positively skewed)
#'   \item \code{H < -0.2}: Data are "sufficiently" left-skewed (negatively skewed)
#'   \item Otherwise: Data are not sufficiently skewed by this rule
#' }
#' 
#' The threshold of \eqn{\pm 0.2} is based on Hildebrand's original suggestion. Note that this is a 
#' rule-of-thumb rather than a formal statistical test. The function handles missing values by default 
#' via \code{na.rm = TRUE} in internal calculations.
#' 
#' @examples
#' # Right-skewed data
#' x_right <- rexp(100, rate = 1)
#' hildebrand.rule(x_right)
#' 
#' # Left-skewed data  
#' x_left <- -rexp(100, rate = 1)
#' hildebrand.rule(x_left)
#' 
#' # Symmetric data
#' x_sym <- rnorm(100, mean = 10, sd = 2)
#' hildebrand.rule(x_sym)
#' 
#' @references 
#' Hildebrand, D. K. (1986). Statistical Thinking for Behavioral Scientists. Boston: Duxbury Press.
#' 
#' 
#' @seealso 
#' \code{\link{skewness}} for other skewness measures, \code{\link{mean}}, \code{\link{median}}
#' 


#' @export
hildebrand.rule <- function(x, ...) {
UseMethod("hildebrand.rule")
}

#' @rdname hildebrand.rule
#' @export
hildebrand.rule.default = function(x, y=NA, rec=0, na.rm=TRUE) {
	if( is.na(y[1]) ) {
		r=(mean(x, na.rm=na.rm)-median(x, na.rm=na.rm) )/sd(x, na.rm=na.rm)
	} else {
		y = as.factor(y)
		r = data.frame(NA,NA,NA)
		colnames(r) = c("Group","Ratio","Skew")
		t = length(unique(y))
		for(k in 1:t) {
			r[k,1] = as.character(unique(y)[k])
			r[k,2] = hildebrand.rule.default( x[y==levels(y)[k]],rec=1 )
				r[k,3]="No Skew"
			if(r[k,2] > +0.20) r[k,3]="Positive Skew"
			if(r[k,2] < -0.20) r[k,3]="Negative Skew"
		}
}

	if( rec<1 & is.null(dim(r)) ) {
		r2 = data.frame("xx ",r,NA)
			r2[1,3]="No Skew"
		if(r > +0.20) r2[1,3]="Positive Skew"
		if(r < -0.20) r2[1,3]="Negative Skew"
		colnames(r2) = c("Group","Ratio","Skew")
		r=r2
	}
r
}

#' @rdname hildebrand.rule
#' @export
hildebrand.rule.formula = function(formula, na.rm=TRUE) {
  if (missing(formula) || (length(formula) != 3L)) 
      stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)

  if (is.matrix(eval(m$data, parent.frame()))) 
      m$data <- as.data.frame(data)
  m[[1L]] <- quote(model.frame)

  mf <- eval(m, parent.frame())
  if (length(mf) != 2L) 
      stop("'formula' should be of the form response ~ group")

  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  y <- do.call("hildebrand.rule.default", as.list(mf))

y
}