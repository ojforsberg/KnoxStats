#' @title 
#' Overlay a Probability Distribution on a Histogram
#' 
#' @description
#' Creates a histogram of data with an overlaid probability density curve.
#' This visualization helps compare empirical data distribution with theoretical
#' probability distributions. The function estimates distribution parameters
#' from the data using method of moments or maximum likelihood.
#' 
#' @param x A numeric vector of data values.
#' @param dist Character string specifying the distribution to overlay.
#'   Options include: "norm" (normal), "exp" (exponential), "t" (Student's t),
#'   "chisq" (chi-square), "gamma" (gamma), "cauchy" (Cauchy), or "unif" (uniform).
#'   Default is "norm".
#' @param na.rm Logical indicating whether to remove missing values. Default is TRUE.
#' @param breaks Method to determine histogram breaks. Can be a number, vector,
#'   or character string like "Sturges" or "Scott". Default is "Sturges".
#' @param from,to Numeric values for the density curve range. If NA, calculated
#'   automatically from data.
#' @param length Number of points for the density curve. Default is 10000.
#' @param xlim,ylim Limits for x and y axes. If NA, calculated automatically.
#' @param col Color for the density curve border. Default is "dodgerblue".
#' @param fill Color for the density curve fill. Default is "aliceblue".
#' @param rug Logical indicating whether to add a rug plot. Default is FALSE.
#' @param onTop Logical indicating whether the density should be plotted on top
#'   of the histogram. Default is FALSE.
#' @param yaxs Character indicating y-axis style. "i" for internal, "r" for regular.
#'   Default is "i".
#' @param las Orientation of axis labels (0=parallel, 1=horizontal, 2=perpendicular,
#'   3=vertical). Default is 1.
#' @param xlab Label for x-axis. Default is "The Variable".
#' @param ylab Label for y-axis. Default is empty.
#' @param main Main title for the plot. Default is empty.
#' @param maxDF Maximum degrees of freedom to consider for t and chi-square
#'   distributions. Default is 1000.
#' @param ... Additional graphical parameters passed to plotting functions.
#' 
#' @return
#' A named list containing the estimated parameters for the selected distribution.
#' 
#' @details
#' This function helps visualize how well different probability distributions
#' fit your data. It creates a histogram (showing the empirical distribution)
#' and overlays a smooth curve showing the theoretical probability density
#' function.
#' 
#' The function estimates distribution parameters from your data:
#' \itemize{
#'   \item For normal distribution: mean and standard deviation
#'   \item For exponential distribution: rate parameter (1/mean)
#'   \item For t-distribution: degrees of freedom (estimated by maximum likelihood)
#'   \item For chi-square distribution: degrees of freedom (estimated by maximum likelihood)
#'   \item For gamma distribution: shape and rate parameters (method of moments)
#'   \item For Cauchy distribution: location (median) and scale (IQR/2)
#'   \item For uniform distribution: minimum and maximum values
#' }
#' 
#' The histogram uses a special styling with white separator lines in bars
#' when counts are low, making it easier to see individual observations.
#' 
#' @examples
#' # Example 1: Normal distribution overlay
#' set.seed(123)
#' normal_data <- rnorm(100, mean = 50, sd = 10)
#' overlay(normal_data, dist = "norm", 
#'         main = "Normal Distribution Fit",
#'         xlab = "Test Scores")
#' 
#' # Example 2: Exponential distribution
#' exp_data <- rexp(50, rate = 0.5)
#' overlay(exp_data, dist = "exp",
#'         col = "darkred", fill = "pink",
#'         main = "Exponential Distribution Fit")
#' 
#' # Example 3: With rug plot and custom colors
#' t_data <- rt(80, df = 5)
#' overlay(t_data, dist = "t",
#'         rug = TRUE, col = "darkgreen", fill = "lightgreen",
#'         xlab = "t-distributed Values")
#' 
#' # Example 4: Compare multiple distributions
#' par(mfrow = c(2, 2))
#' overlay(normal_data, dist = "norm", main = "Normal")
#' overlay(normal_data, dist = "t", main = "t-distribution")
#' overlay(exp_data, dist = "exp", main = "Exponential")
#' overlay(exp_data, dist = "gamma", main = "Gamma")
#' par(mfrow = c(1, 1))
#' 
#' # Example 5: Custom histogram breaks
#' overlay(normal_data, breaks = 20, 
#'         col = "purple", fill = "lavender",
#'         main = "Custom Breaks (n=20)")
#' 
#' @seealso
#' \code{\link{hist}} for creating histograms,
#' \code{\link{dnorm}}, \code{\link{dexp}}, \code{\link{dt}} for density functions,
#' \code{\link{rug}} for adding rug plots,
#' \code{\link{par}} for graphical parameters
#' 
#' @references
#' Rice, John A. 2007. *Mathematical Statistics and Data Analysis*. 3rd ed. 
#' Belmont, CA: Thomson Brooks/Cole.
#' 
#' Devore, Jay L. 2011. *Probability and Statistics for Engineering and the 
#' Sciences*. 8th ed. Boston, MA: Cengage Learning.
#' 
#' Wilks, Daniel S. 2011. *Statistical Methods in the Atmospheric Sciences*. 
#' 3rd ed. Amsterdam: Academic Press.
#' 
#' @export
overlay <- function(x, dist = "norm", na.rm = TRUE, breaks = "Sturges", 
                   from = NA, to = NA, length = 1e4, xlim = NA, ylim = NA,
                   col = "dodgerblue", fill = "aliceblue", 
                   rug = FALSE, onTop = FALSE, yaxs = "i", las = 1,
                   xlab = "The Variable", ylab = "", main = "", 
                   maxDF = 1000, ...) {
  
  # Input validation
  if (!is.numeric(x)) {
    stop("Input 'x' must be a numeric vector.")
  }
  
  dist <- tolower(dist)
  valid_dists <- c("norm", "exp", "t", "chisq", "gamma", "cauchy", "unif")
  if (!dist %in% valid_dists) {
    stop("Invalid distribution. Choose from: ", 
         paste(valid_dists, collapse = ", "))
  }
  
  # Handle missing values
  num_na <- sum(is.na(x))
  if (na.rm && num_na > 0) {
    if (num_na == 1) {
      warning("1 missing value was removed from the data.")
    } else {
      warning(paste(num_na, "missing values were removed from the data."))
    }
    x <- x[!is.na(x)]
  }
  
  if (!na.rm && num_na > 0) {
    stop(paste("Data contains", num_na, "missing values. ",
               "Set na.rm = TRUE or remove NAs manually."))
  }
  
  if (length(x) < 2) {
    stop("Need at least 2 non-missing values to create a plot.")
  }
  
  # Helper function to plot enhanced histogram
  plot_enhanced_hist <- function(h, col = "black", fill = "white") {
    # Plot histogram bars
    for (i in seq_along(h$density)) {
      rect(h$breaks[i], 0, h$breaks[i + 1], h$density[i], 
           border = col, col = "grey90")
      
      # Add white separator lines for small counts
      if (h$counts[i] > 1 && h$counts[i] <= 50) {
        mult <- h$density[i] / h$counts[i]
        line_margin <- 0.15 * (h$breaks[i + 1] - h$breaks[i])
        
        for (k in 1:(h$counts[i] - 1)) {
          segments(h$breaks[i] + line_margin, k * mult, 
                   h$breaks[i + 1] - line_margin, k * mult,
                   col = "white", lwd = 2)
        }
      }
    }
  }
  
  # Function to calculate density and parameters
  calculate_density <- function(dist, x, from, to, length, maxDF) {
    xx <- seq(from, to, length.out = length)
    params <- list()
    
    switch(dist,
      "norm" = {
        mu <- mean(x)
        sigma <- sd(x)
        density <- dnorm(xx, mean = mu, sd = sigma)
        params <- list(mean = mu, sd = sigma)
      },
      
      "exp" = {
        if (min(x) < 0) {
          stop("Cannot have negative values for exponential distribution.")
        }
        lambda <- 1 / mean(x)
        density <- dexp(xx, rate = lambda)
        params <- list(rate = lambda)
      },
      
      "t" = {
        # Estimate degrees of freedom using maximum likelihood
        log_lik <- numeric(maxDF)
        for (nu in 1:maxDF) {
          log_lik[nu] <- sum(dt(x, df = nu, log = TRUE))
        }
        df <- which.max(log_lik)
        if (df == maxDF) {
          warning("Estimated degrees of freedom at maximum boundary. ",
                  "Consider increasing maxDF.")
        }
        density <- dt(xx, df = df)
        params <- list(df = df)
      },
      
      "chisq" = {
        if (min(x) < 0) {
          stop("Cannot have negative values for chi-square distribution.")
        }
        log_lik <- numeric(maxDF)
        for (nu in 1:maxDF) {
          log_lik[nu] <- sum(dchisq(x, df = nu, log = TRUE))
        }
        df <- which.max(log_lik)
        if (df == maxDF) {
          warning("Estimated degrees of freedom at maximum boundary. ",
                  "Consider increasing maxDF.")
        }
        density <- dchisq(xx, df = df)
        params <- list(df = df)
      },
      
      "gamma" = {
        if (min(x) < 0) {
          stop("Cannot have negative values for gamma distribution.")
        }
        # Method of moments estimators
        shape <- mean(x)^2 / var(x)
        rate <- mean(x) / var(x)
        density <- dgamma(xx, shape = shape, rate = rate)
        params <- list(shape = shape, rate = rate)
      },
      
      "cauchy" = {
        location <- median(x)
        scale <- IQR(x) / 2
        density <- dcauchy(xx, location = location, scale = scale)
        params <- list(location = location, scale = scale)
      },
      
      "unif" = {
        a <- min(x)
        b <- max(x)
        density <- dunif(xx, min = a, max = b)
        params <- list(min = a, max = b)
      }
    )
    
    list(density = density, parameters = params)
  }
  
  # Create histogram object
  hist_obj <- hist(x, breaks = breaks, plot = FALSE)
  
  # Determine plotting range
  w <- diff(range(hist_obj$breaks))
  if (is.na(from)) from <- min(hist_obj$breaks) - 0.5 * w
  if (is.na(to)) to <- max(hist_obj$breaks) + 0.5 * w
  
  # Calculate density
  result <- calculate_density(dist, x, from, to, length, maxDF)
  xx_seq <- seq(from, to, length.out = length)
  yy <- result$density
  
  # Set plot limits
  if (is.na(xlim[1])) xlim <- c(from, to)
  if (is.na(ylim[1])) ylim <- c(0, max(hist_obj$density, yy))
  
  # Save current graphical parameters
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  # Set graphical parameters
  par(las = las, yaxt = "n", yaxs = yaxs, 
      mar = c(3.5, 0.5, 2, 0.5) + 0.5,
      font.lab = 2, cex.lab = 1.2, cex.axis = 1)
  
  # Create empty plot
  plot.new()
  plot.window(xlim = xlim, ylim = ylim)
  abline(h = 0, col = "gray50")
  
  # Plot density polygon
  polygon(x = c(xx_seq, rev(xx_seq)), 
          y = c(yy, rep(0, length(yy))), 
          col = fill, border = col)
  
  # Plot histogram
  plot_enhanced_hist(hist_obj)
  
  # Re-draw density on top if requested
  if (onTop) {
    if (fill != "" && !is.na(fill)) {
      polygon(x = c(xx_seq, rev(xx_seq)), 
              y = c(yy, rep(0, length(yy))), 
              col = fill, border = col)
    } else {
      lines(xx_seq, yy, col = col, lwd = 2)
    }
  } else {
    lines(xx_seq, yy, col = col, lwd = 2)
  }
  
  # Add axis and labels
  axis(1)
  title(xlab = xlab, ylab = ylab, main = main, line = 2.5)
  
  # Add rug plot if requested
  if (rug) {
    rug(jitter(x), col = "gray30")
  }
  
  # Return estimated parameters
  invisible(result$parameters)
}
