#' @title
#' Binomial Plot
#'
#' @description
#' `binomPlot` creates a basic plot of observed proportions and a confidence interval for them.
#'
#' 
#' @param x Number of successes (scalar or vector of length g)
#' @param n Number of trials (a vector of length g)
#' @param conf.level Confidence level, defaulting to 0.95
#' @param boxcol The color of the box representing the confidence interval 
#' @param border The color of the border around the confidence interval box
#' @param midlinecol The color of the segment representing the observed proportion
#' @param midlinety The line type for the segment representing the observed proportion
#' 
#' 
#' @return The function plots the observed proportion(s) and the confidence interval(s)
#' 
#' @details
#' This function produces a plot of observed binomial proportions (x/n) along with their 
#' corresponding confidence intervals. The confidence intervals are calculated using the 
#' normal approximation (Wald) method, with continuity correction applied when 
#' appropriate to improve accuracy, particularly for small sample sizes or proportions 
#' near 0 or 1.
#' 
#' Each proportion is visualized as a horizontal line segment (`midlinecol`) within 
#' a rectangular box that represents the confidence interval. The box is filled with 
#' `boxcol` and outlined with `border`. The x-axis represents the group index (if 
#' multiple groups are provided) or a single point for scalar inputs. 
#' 
#' If `x` and `n` are vectors of length g > 1, the function will plot each group's 
#' proportion and confidence interval side-by-side, spaced along the x-axis.
#' 
#' The confidence interval is computed as:
#' \deqn{\hat{p} \pm z_{\alpha/2} \sqrt{\frac{\hat{p}(1-\hat{p})}{n}}}{%
#'       p_hat ± z * sqrt(p_hat * (1 - p_hat) / n)}
#' where \eqn{\hat{p} = x/n}{p_hat = x/n} and z is the quantile from the standard 
#' normal distribution corresponding to the specified confidence level. Continuity 
#' correction adjusts the interval by \eqn{1/(2n)} when applied.
#' 
#' This visualization is useful for comparing multiple binomial proportions and 
#' assessing the precision of their estimates. The plot does not include axes labels 
#' or titles by default, allowing the user to customize the output as needed.
#' 
#' @examples
#' binomPlot(x=15, n=20)
#' 
#' binomPlot(x=c(15,22,45,6))
#' 
#' binomPlot(x=c(15,22,45,6), n=c(30,44,100,10))
#' 
#'
#' @export
binomPlot <- function( x, n=rep(sum(x),length(x)), 
        conf.level=0.95, 
        names=NULL, 
        xlim=NULL, ylim=NULL, xlab=NA, ylab="Proportion",
        boxcol=NULL, border=NA, midlinecol="black",midlinety=1,
        ... ) {
  g = length(x)
  if( length(n)!=g ) stop("The lengths of x and n must be the same");
  if(length(conf.level)>1) stop("The confidence level needs to be a single value.")
  if( conf.level<=0.50 || conf.level>=1.00) stop("The confidence level must be between 0.50 and 1.00, exclusive.")

  if( is.null(boxcol) ) {
        boxcol="honeydew2"
  }
  boxcol = rep(boxcol,g)

  ucl = numeric()
  lcl = numeric()

  for(i in 1:g) {
    tt=binom.test(x[i],n[i], conf.level=conf.level)
    lcl[i]=tt$conf.int[1]
    ucl[i]=tt$conf.int[2]
  }

  if(is.null(xlim)) xlim=c(0.5,g+0.5)
  if(is.null(ylim)) ylim=c(0,1) 

  plot.new()
  plot.window(xlim=xlim,ylim=ylim)

  if(!is.null(names)) mtext(side=1, at=1:g, text=names)
  if(is.null(names))  mtext(side=1, at=1:g, text=1:g)
  axis(2)
  title(xlab=xlab)
  title(ylab=ylab)

  for(i in 1:g) {
    thisColor = boxcol[i]
    rect(i-0.15,lcl[i],i+0.15,ucl[i], col=thisColor, border=border)
    segments(i-0.25,x[i]/n[i],i+0.25,x[i]/n[i], lwd=2, col=midlinecol, lty=midlinety)
  }

}