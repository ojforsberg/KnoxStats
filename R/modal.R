#' @title 
#' Calculate the Mode of a Dataset
#'
#' @description
#' Finds the mode(s) of a dataset, which is the value(s) that appear most 
#' frequently. Unlike the mean and median, a dataset can have zero, one, or 
#' multiple modes.
#'
#' @param x A vector (numeric, character, or factor) for which to find the mode.
#'
#' @return 
#' If there is a single mode, returns that value as a character string. 
#' If there are multiple modes, returns a character vector of all modes.
#' If there is no mode (all values appear exactly once), returns 
#' "There is no mode." as a character string.
#'
#' @details
#' The mode is a measure of central tendency that identifies the most 
#' frequently occurring value(s) in a dataset. This function can handle:
#' 
#' * **Unimodal datasets**: One value appears most frequently
#' * **Bimodal/multimodal datasets**: Multiple values tie for most frequent
#' * **No mode**: All values appear exactly once (equifrequent)
#' 
#' The function first creates a frequency table of the data, identifies 
#' which value(s) have the maximum frequency, and returns them. If all 
#' values have the same frequency (including the case where each value 
#' appears only once), it returns a message indicating there is no mode.
#'
#' @examples
#' # Single mode (unimodal)
#' scores <- c(85, 90, 85, 88, 92, 85, 90)
#' modal(scores)
#' 
#' # Two modes (bimodal)
#' colors <- c("red", "blue", "red", "blue", "green")
#' modal(colors)
#' 
#' # No mode (all unique)
#' unique_values <- c(1, 2, 3, 4, 5)
#' modal(unique_values)
#' 
#' # All values appear the same number of times (no mode)
#' equal_freq <- c("A", "B", "A", "B")
#' modal(equal_freq)
#' 
#' # Mode with factors
#' grade_factor <- factor(c("A", "B", "A", "C", "B", "A"))
#' modal(grade_factor)
#'
#' @seealso
#' \code{\link{mean}} for the arithmetic average,
#' \code{\link{median}} for the middle value,
#' \code{\link{table}} for creating frequency tables,
#' \code{\link{which.max}} for finding maximum positions
#'
#' @references
#' Wikipedia contributors. 2023. "Mode (Statistics)." *Wikipedia, The Free 
#' Encyclopedia*. https://en.wikipedia.org/wiki/Mode_(statistics).
#'
#' @export
modal <- function(x) {
  # Create frequency table
  freq_table <- table(x)
  
  # Find positions with maximum frequency
  max_positions <- which(freq_table == max(freq_table))
  
  # Check if there is a mode
  if (length(max_positions) == length(freq_table)) {
    # All values have the same frequency
    return("There is no mode.")
  } else {
    # Return the mode(s)
    return(names(freq_table[max_positions]))
  }
}
