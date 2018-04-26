#' @title Dimension Names
#' 
#' @description Get both row and column names \cr
#' Use \code{get_dim_names} to extract dimnames \cr
#' Use \code{get_row_names} to extract rownames \cr
#' Use \code{get_col_names} to extract colnames 
#' 
#' @param x a matrix or data frame
#' @export get_dim_names get_row_names get_col_names
#' @aliases get_dim_names get_row_names get_col_names
#' @keywords internal
get_dim_names <- function(x)
{
  list(row_names = get_row_names(x), 
       col_names = get_col_names(x))
}

get_row_names <- function(x)
{
  if (lacks_rownames(x)) row_names = 1:nrow(x)
  else row_names = rownames(x)
  # output
  row_names
}

get_col_names <- function(x)
{
  if (lacks_colnames(x)) col_names = 1:ncol(x)
  else col_names = colnames(x)
  # output
  col_names
}


#' @title Block Names
#' @description Get block names
#' @param blocks vector
#' @keywords internal
get_block_names <- function(blocks)
{
  if (lacks_names(blocks)) {
    block_names = paste("Block", 1:length(blocks), sep="")
  } else { 
    block_names = names(blocks)
  }
  # output
  block_names
}


#' @title All Labels
#' @description Get row names, column names, and block labels
#' @param x a matrix or data frame
#' @param blocks vector
#' @keywords internal
get_labels <- function(x, blocks = NULL)
{
  list(
    block_names = get_block_names(blocks),
    row_names = get_row_names(x),
    col_names = get_col_names(x)
  )
}

