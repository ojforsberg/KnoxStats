#' @title
#' Drop Rows
#'
#' @description
#' This drops rows from a data frame.
#' 
#' @param df The data frame
#' @param row.names A vector of names of the rows to be dropped from the data frame.
#' 
#' @return A data frame identical to the original, but lacking the stated rows.
#' 
#' @details
#' This function removes specified rows from a data frame based on their row names.
#' It uses base R's row name indexing to subset the data frame, keeping only those
#' rows whose names are not in the \code{row.names} vector.
#' 
#' The function is particularly useful when working with data frames that have
#' meaningful row names (e.g., sample IDs, observation names) and you need to
#' exclude specific observations while maintaining the original structure of
#' the remaining data.
#' 
#' Note that this function operates on row names, not row indices. If your data
#' frame uses default integer row names (e.g., "1", "2", "3"), you can still use
#' this function by providing the row names as character strings.
#' 
#' 
#' @export

drop.rownames <- function( df, row.names ) {
 return( df[setdiff(rownames(df),row.names) ,] )
}

