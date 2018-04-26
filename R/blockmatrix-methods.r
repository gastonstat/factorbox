#' @title Number of Blocks by Rows/Columns of a Block-Matrix
#' 
#' @description return the number of rows present in a block-matrix
#' 
#' @details
#' Use \code{bdim} to get the blockdimension \cr
#' Use \code{brow} to get the number of blocks by rows \cr
#' Use \code{bcol} to get the number of blocks by columns
#' 
#' @param x a block-matrix (object of class \code{"blockmatrix"})
#' @return an integer indicating the number of blocks
#' @seealso \code{\link{blockdim}}
#' @name bdim
#' @aliases bdim brow bcol
#' @export bdim brow bcol
#' @examples
#' # create some block-matrices
#' A = blockmatrix(matrix(1:12, 3, 4), rowparts=3, colparts=c(2,2))
#' B = blockmatrix(matrix(1:20, 5, 4), rowparts=c(3,2), colparts=c(2,2))
#' 
#' # row-dimensions
#' brow(A)
#' brow(B)
#' 
#' # column-dimensions
#' bcol(A)
#' bcol(B)
bdim <- function(x) {
  if (!is.bmatrix(x))
    stop("\n'bdim()' requires a block-matrix")
  dims(x)
}

brow <- function(x) {
  if (!is.bmatrix(x))
    stop("\n'brow()' requires a block-matrix")
  dims(x)[1L]
}

bcol <- function(x) {
  if (!is.bmatrix(x))
    stop("\n'bcol()' requires a block-matrix")
  dims(x)[2L]
}



#' @title Row partitions of a block-matrix
#' 
#' @description
#' Returns row partitions of a block-matrix
#' 
#' @param x a block matrix
#' @name rowparts
#' @aliases rowparts rparts
#' @export rowparts rparts
rowparts <- function(x) {
  if (!is.bmatrix(x))
    stop("\n'rowparts()' requires a block-matrix")
  
  start_end = from_to(dims(x))
  from = start_end$from
  to = start_end$to
  
  aux_parts = parts(x)
  # row partitions
  rparts = aux_parts[from[1]:to[1]]
  rparts
}

rparts <- function(x) rowparts(x)


#' @title Column partitions of a block-matrix
#' 
#' @description
#' Return column partitions of a block-matrix
#' 
#' @param x a block matrix
#' @name colparts
#' @aliases colparts cparts
#' @export colparts cparts
colparts <- function(x) {
  if (!is.bmatrix(x))
    stop("\n'colparts()' requires a block-matrix")
  
  start_end = from_to(dims(x))
  from = start_end$from
  to = start_end$to
  
  aux_parts = parts(x)
  # column partitions
  cparts = aux_parts[from[2]:to[2]]
  cparts
}

cparts <- function(x) colparts(x)



#' @title Extract Row / Column Blocks
#' 
#' @description return the specified block(s) from a block-matrix
#' Use \code{rowblock} or \code{colblock} depending on whether you want
#' to get a particular row block or a particular column block (output
#' as block-matrix object)
#' 
#' @param x a multiblock
#' @param k integer to indicate row (or column) block-dimension
#' @return the specified row/column block
#' @seealso \code{\link{get_block}}, \code{\link{separate}}
#' @aliases rowblock colblock
#' @export rowblock colblock
#' @examples
#' # create a block-matrix with 6 blocks
#' A = blockmatrix(matrix(1:63, 7, 9), c(3,4), c(2,3,4))
#' 
#' # extract first row block
#' rowblock(A, 1)
#' 
#' # extract third column block
#' colblock(A, 3)
rowblock <- function(x, k) 
{
  if (!is.bmatrix(x))
    stop("\nrowblock() requires a blockmatrix")
  if (missing(k))
    stop("\nIndex 'k' is missing")
  if (length(k) != 1 || k < 0)
    stop("\nIndex 'k' must be a positive integer")
  if (brow(x) < k)
    stop("\nIndex 'k' grater than number of blocks in blockmatrix")
  
  rd = rparts(x)
  rows_to = cumsum(rd)
  rows_from = rows_to - rd + 1  
  irows = rows_from[k]:rows_to[k]
  ## extract block [k,] from x
  blockmatrix(x[irows,], colparts=cparts(x))
}


colblock <- function(x, k) 
{
  if (!is.bmatrix(x))
    stop("\ncolblock() requires a blockmatrix")
  if (missing(k))
    stop("\nIndex 'k' is missing")
  if (length(k) != 1 || k < 0)
    stop("\nIndex 'k' must be a positive integer")
  if (bcol(x) < k)
    stop("\nIndex 'k' grater than number of blocks in blockmatrix")
  
  cd = cparts(x)
  cols_to = cumsum(cd)
  cols_from = cols_to - cd + 1
  jcols = cols_from[k]:cols_to[k]
  ## extract block [,k] from x
  blockmatrix(x[,jcols], rowparts=rparts(x))
}



#' @title Test block-matrix structure
#' 
#' @description 
#' \code{is.rowblock} tests if its argument is one row block-matrix \cr
#' \code{is.colblock} tests if its argument is one column block-matrix \cr
#' \code{is.oneblock} tests if its argument is one single block-matrix \cr
#' \code{is.megablock} tests if its argument is a block-matrix with
#' more than one block by rows and columns
#' 
#' @param x an R object 
#' @aliases is.rowblock is.colblock is.oneblock is.megablock
#' @export is.rowblock is.colblock is.oneblock is.megablock
#' @examples
#' # create a regular matrix
#' a = matrix(runif(20), 5, 4)
#' 
#' # block-matrix with one single block 
#' A1 = blockmatrix(a, 5, 4) 
#' # block-matrix with one column block 
#' A2 = blockmatrix(a, c(2, 3), 4)
#' # block-matrix with one row block
#' A3 = blockmatrix(a, 5, c(2,2))
#' # block-matrix with 2 row-blocks and 2 col-blocks
#' A4 = blockmatrix(a, c(2, 3), c(2, 2))  
#' 
#' # test if row-block
#' is.rowblock(A1)  # TRUE
#' is.rowblock(A2)  # FALSE
#' is.rowblock(A3)  # TRUE
#' is.rowblock(A4)  # FALSE
#' 
#' # test if col-block
#' is.colblock(A1)  # TRUE
#' is.colblock(A2)  # TRUE
#' is.colblock(A3)  # FALSE
#' is.colblock(A4)  # FALSE
#' 
#' # test if one-block
#' is.oneblock(A1)  # TRUE
#' is.oneblock(A2)  # FALSE
#' is.oneblock(A3)  # FALSE
#' is.oneblock(A4)  # FALSE
#' 
#' # test if mega-block
#' is.megablock(A1)  # FALSE
#' is.megablock(A2)  # FALSE
#' is.megablock(A3)  # FALSE
#' is.megablock(A4)  # TRUE
is.rowblock <- function(x) {
  if (is.bmatrix(x)) {
    (brow(x) == 1)    
  } else FALSE
}

is.colblock <- function(x) {
  if (is.bmatrix(x)) {
    (bcol(x) == 1)    
  } else FALSE
}

is.oneblock <- function(x) {
  (is.rowblock(x) & is.colblock(x))
}

is.megablock <- function(x) {
  (brow(x) > 1 & bcol(x) > 1)  
}
