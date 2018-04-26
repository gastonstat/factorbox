#' @title Extract Blocks
#' 
#' @description return the specified block(s) from a multi-block
#' 
#' @details
#' Use \code{get_block} to extract one single block from a
#' multi-block object
#' 
#' @param x a multiblock
#' @param i integer to indicate row block-dimension
#' @param j integer to indicate column block-dimension
#' @param k integer to indicate slice block-dimension (not implemented yet)
#' @return the specified block
#' @seealso \code{\link{rowblock}}, \code{\link{colblock}}, \code{\link{separate}}
#' @export
#' @examples
#' # blockvetcor
#' bv = blockvector(1:10, parts=c(5,5), dims=2)
#' # first block
#' get_block(bv, 1)
#' # second block
#' get_block(bv, 2)
#' 
#' # create a block-matrix with 6 blocks
#' A = blockmatrix(matrix(1:63, 7, 9), c(3,4), c(2,3,4))
#' 
#' # extract block A11
#' get_block(A, 1, 1)
#' 
#' # extract block A23
#' get_block(A, 2, 3)
get_block <- function(x, i, j, k) {
  UseMethod("get_block", x)
}


#' @S3method get_block default
get_block.default <- function(x, i, j, k) 
{
  if (!is.multiblock(x))
    stop("\n'get_block()' requires a multiblock object")
}


#' @S3method get_block blockvector
get_block.blockvector <- function(x, i, j, k) 
{
  if (missing(i))
    stop("\nIndex 'i' is missing")
  
  if (!is_positive_integer(i))
    stop("\nIndex 'i' must be a positive integer")

  if (length(i) > 1) {
    warning("\nonly the first element in 'i' will be used")
    i = i[1L]
  }
  if (nblocks(x) < i)
    stop("\nIndex 'i' exceeds number of blocks")
  
  # overall starting and ending indices
  start_end = from_to(parts(x))
  # extract block [i] from x
  xblock = x[start_end$from[i]:start_end$to[i]]
  # output as uniblock
  blockvector(xblock, parts = length(xblock), dims = 1L)
}


#' @S3method get_block blockmatrix
get_block.blockmatrix <- function(x, i, j, k) 
{
  if (missing(i))
    stop("\nIndex 'i' is missing")
  if (missing(j))
    stop("\nIndex 'j' is missing")
  
  if (!is_positive_integer(i))
    stop("\nIndex 'i' must be a positive integer")
  if (!is_positive_integer(j))
    stop("\nIndex 'j' must be a positive integer")

  if (length(i) > 1) {
    warning("\nonly the first element in 'i' will be used")
    i = i[1L]
  }
  if (length(j) > 1) {
    warning("\nonly the first element in 'j' will be used")
    j = j[1L]
  }
  
  if (brow(x) < i)
    stop("\nIndex 'i' exceeds number of row-blocks")
  if (bcol(x) < j)
    stop("\nIndex 'j' exceeds number of column-blocks")
  
  x_rparts = rowparts(x)
  x_cparts = colparts(x)
  
  rows_from_to = from_to(x_rparts)
  rows_from = rows_from_to$from
  rows_to = rows_from_to$to
  
  cols_from_to = from_to(x_cparts)
  cols_from = cols_from_to$from
  cols_to = cols_from_to$to
  
  # specified starting and ending indices
  irows = rows_from[i]:rows_to[i]
  jcols = cols_from[j]:cols_to[j]
  # extract block [i,j] from x
  xblock = x[irows, jcols]
  # output as uniblock
  blockmatrix(xblock, rowparts=nrow(xblock), colparts=ncol(xblock))
}
