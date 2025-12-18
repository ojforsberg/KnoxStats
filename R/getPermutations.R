#' @title
#' Permutation Generator
#'
#' @description
#' This produces *all* permutations of the numbers 1 through `n`.
#' 
#' @param n An integer
#' 
#' @return An n! x n matrix containing all permutations
#' 
#' @details
#' This function generates all permutations of the integers from 1 to `n` using a recursive algorithm. 
#' The algorithm works by:
#' 1. Generating all permutations of size `n-1`
#' 2. For each permutation, inserting the value `n` in all possible positions
#' 3. Adjusting values greater than or equal to the inserted value by incrementing them by 1
#' 
#' The time complexity is O(n!) and the space complexity is O(n!) as well, since all permutations are stored in memory.
#'
#' @examples
#' # All permutations of 1, 2, 3
#' permutations(3)
#' 
#' # All permutations of 1, 2, 3, 4
#' perm4 <- permutations(4)
#' dim(perm4)  # 24 rows, 4 columns
#' 
#' # First few permutations of 4 elements
#' head(permutations(4))
#' 
#' # Use permutations to shuffle data
#' set.seed(123)
#' data <- letters[1:4]
#' perm <- permutations(4)
#' data[perm[5, ]]  # Apply the 5th permutation to the data
#'
#' @references
#' Knuth, D. E. (1997). *The Art of Computer Programming, Volume 1 (3rd ed.): Fundamental Algorithms*. Addison-Wesley. pp. 1-2.
#' 
#' This algorithm is similar to the "Algorithm L (Lexicographic permutation generation)" described by Knuth, though implemented recursively rather than iteratively.
#' 
#' @seealso [utils::combn()] for combinations, [sample()] for random permutations
#' 

#' 
#' @export

getPermutations <- function(n) {
  if (n == 1) return(matrix(1))
  sp <- permutations(n - 1)
  p <- nrow(sp)
  A <- matrix(nrow = n * p, ncol = n)
  for (i in 1:n) {
    A[(i - 1) * p + 1:p, ] <- cbind(i, sp + (sp >= i))
  }
  return(A)
}