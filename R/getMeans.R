#' @title
#' Calculate Block Means of a Vector
#' 
#' @description
#' Divides a numeric vector into equal-sized blocks and calculates the mean of
#' each block. This is useful for data aggregation, smoothing, or downsampling.
#'
#' @param x A numeric vector to be aggregated.
#' @param block_size Number of consecutive elements to average together.
#'   Must be a positive integer less than or equal to the length of `x`.
#'
#' @return A numeric vector of block means. The length is `floor(length(x)/block_size)`.
#'
#' @details
#' The function divides the input vector into consecutive blocks of size 
#' `block_size`. If the length of `x` is not evenly divisible by `block_size`, 
#' excess elements at the beginning of the vector are discarded to create 
#' complete blocks. This ensures all block means are calculated from the 
#' same number of observations.
#'
#' @examples
#' # Basic usage
#' x <- 1:12
#' get_means(x, 3)  # Means of [1,2,3], [4,5,6], [7,8,9], [10,11,12]
#' 
#' # Downsample a time series
#' time_series <- sin(seq(0, 4*pi, length.out = 100))
#' smoothed <- get_means(time_series, 5)
#' 
#' # Compare with original length
#' length(time_series)  # 100
#' length(smoothed)     # 20
#' 
#' # Visualize the effect
#' plot(time_series, type = "l", col = "gray", 
#'      main = "Original vs Block-Averaged")
#' lines(rep(smoothed, each = 5), col = "red", lwd = 2)
#' legend("topright", legend = c("Original", "Block Mean (size=5)"),
#'        col = c("gray", "red"), lty = 1)
#'
#' @seealso
#' \code{\link{apply}} for applying functions over array margins,
#' \code{\link{rowMeans}} for alternative matrix row means,
#' \code{\link{filter}} for moving averages and other smoothing operations
#' 
#' @export
getMeans <- function(x, block_size = 1) {
  # Input validation
  if (!is.numeric(x)) {
    stop("Input 'x' must be a numeric vector", call. = FALSE)
  }
  
  if (!is.numeric(block_size) || length(block_size) != 1 || 
      block_size <= 0 || block_size != as.integer(block_size)) {
    stop("'block_size' must be a positive integer", call. = FALSE)
  }
  
  n <- length(x)
  
  if (block_size > n) {
    stop(sprintf(
      "block_size (%d) cannot be larger than vector length (%d)",
      block_size, n
    ), call. = FALSE)
  }
  
  if (block_size == 1) {
    return(x)  # No averaging needed
  }
  
  # Calculate number of complete blocks
  n_blocks <- floor(n / block_size)
  
  if (n_blocks == 0) {
    stop(sprintf(
      "block_size (%d) is too large to form any complete blocks from vector of length %d",
      block_size, n
    ), call. = FALSE)
  }
  
  # Keep only enough elements to form complete blocks
  # Discard from the beginning to be consistent with original behavior
  elements_needed <- n_blocks * block_size
  excess_elements <- n - elements_needed
  
  if (excess_elements > 0) {
    # Discard first 'excess_elements' to form complete blocks
    warning(sprintf(
      "Discarding first %d element(s) to form %d complete blocks of size %d",
      excess_elements, n_blocks, block_size
    ), call. = FALSE)
    
    x <- x[(excess_elements + 1):n]
  }
  
  # Reshape into matrix and calculate column means
  data_matrix <- matrix(x, nrow = block_size, ncol = n_blocks)
  colMeans(data_matrix)
}

