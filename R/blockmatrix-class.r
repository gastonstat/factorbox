#' @title Block Matrix
#' 
#' @description 
#' \code{blockmatrix} creates a block-matrix from the given set of values \cr
#' \code{as.bmatrix} attempts to turn its argument into a block-matrix \cr
#' \code{is.bmatrix} tests if its argument is a (strict) block-matrix
#' 
#' @details
#' When \code{data} is a blockvector, the return blockmatrix is a
#' column-block (i.e. vertical) matrix
#' 
#' @param data a matrix, a vector, or a blockvector 
#' @param rowparts vector of row partitions
#' @param colparts vector of column partitions
#' @param parts optional integer positive vector with the partitions
#' @param dims optional integer positive vector with the block structure
#' @return An object of class \code{"blockmatrix"}
#' @seealso \code{\link{blockvector}}, \code{\link{matrix}}
#' @export
#' @examples
#' # create a regular matrix
#' a = matrix(runif(20), 5, 4)
#' 
#' # convert 'a' into a block-matrix
#' A = blockmatrix(a, c(2, 3), c(2, 2))
#' A
#' 
#' # test if block-matrix
#' is.bmatrix(a)  # FALSE
#' is.bmatrix(A)  # TRUE
#' 
#' # generate a matrix
#' m = matrix(1:9, 3, 3)
#' 
#' # convert 'm' into a block-matrix (one block)
#' M = as.bmatrix(m, 3, 3)
#' M
blockmatrix <- function(data, rowparts = NULL, colparts = NULL,
                        parts = NULL, dims = NULL)
{
  if (is.vector(data))
    data = as.matrix(data)
  
  if (is.bvector(data))
    return(bvector_to_bmatrix(data))
  
  if (is_not_matrix(data))
    stop("\n'data' must be a matrix")
  
  # check row partitions
  if (!is.null(rowparts)) {
    check_row_partitions(rowparts, nrow(data))
  } else rowparts = nrow(data)
  
  # check column partitions
  if (!is.null(colparts)) {
    check_col_partitions(colparts, ncol(data))
  } else colparts = ncol(data)
  
  # blockdimension elements
  if (is.null(parts)) parts = c(rowparts, colparts)
  if (is.null(dims)) dims = c(length(rowparts), length(colparts))
  
  # output
  attr(data, "blockdimension") = blockdimension(parts, dims)
  class(data) = "blockmatrix"
  data
}

# from bvector to bmatrix
bvector_to_bmatrix <- function(x) {
  xparts = c(parts(x), 1)
  xdims = c(dims(x), 1)
  x = as.matrix(x)
  # output
  attr(x, "blockdimension") = blockdimension(xparts, xdims)
  class(x) = "blockmatrix"
  x
}


# check row partitions (private function)
check_row_partitions <- function(row_part, num_rows)
{
  if (!is.atomic(row_part) || any(row_part <= 0))
    stop("Invalid 'rowparts'")
  if (sum(row_part) != num_rows)
    stop("'rowparts' doesn't match number of rows in data")
  TRUE
}

# check column partitions (private function)
check_col_partitions <- function(col_part, num_cols)
{
  if (!is.atomic(col_part) || any(col_part <= 0))
    stop("Invalid 'colparts'")    
  if (sum(col_part) != num_cols)
    stop("'colparts' doesn't match number of columns in data")
  TRUE
}


#' @S3method print blockmatrix
print.blockmatrix <- function(x, ...) 
{
  cat('A block-matrix:', '\n')
  attr(x, "blockdimension") = NULL
  print(unclass(x))
}


#' @S3method t blockmatrix
t.blockmatrix <- function(x) {
  # transpose blockmatrix
  blockmatrix(t(unclass(x)), colparts(x), rowparts(x))
}


#' @rdname blockmatrix
#' @param x an R object
#' @aliases is.blockmatrix is.bmatrix
#' @export is.blockmatrix is.bmatrix
is.blockmatrix <- function(x) is(x, "blockmatrix")
is.bmatrix <- function(x) is.blockmatrix(x)


#' @rdname blockmatrix
#' @param ... additional arguments to be passed to \code{blockmatrix}
#' @aliases as.blockmatrix as.bmatrix
#' @export as.blockmatrix as.bmatrix
as.blockmatrix <- function(x, ...) blockmatrix(x, ...)
as.bmatrix <- function(x, ...) as.blockmatrix(x, ...)




