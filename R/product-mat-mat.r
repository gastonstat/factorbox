#' @title Matrix-Matrix extended product
#' 
#' @description 
#' Matrix (block-wise) product when multiplying multiblocks \cr
#' 
#' @details This product requires objects to have compatible
#' block-dimensions
#' 
#' @param x a numeric multiblock
#' @param y a numeric multiblock
#' @usage x \%mm\% y
#' @seealso \code{\link{hadhadprod}}, \code{\link{hadmatprod}}
#' @export
#' @rdname matmatprod
#' @examples
#' # vector
#' v = 1:5
#' v %mm% v
#' # same as:  v %*% v
#' v %*% v
#' 
#' # block vector
#' bv = blockvector(v, parts=c(2, 3))
#' bv %mm% bv
#' 
#' # matrix
#' a = matrix(1:20, 4, 5)
#' rownames(a) = paste("ind", 1:4, sep='')
#' colnames(a) = c("a1", "a2", "b1", "b2", "b3")
#' a %mm% t(a)
#' # same as 'a %*% t(a)'
#' a %*% t(a)
#' 
#' # block matrix
#' A = blockmatrix(a, c(2, 2), c(2, 3))
#' A %mm% t(A)
#' 
#' A %mm% bv
"%mm%" <- function(x, y) {
  matmatprod(x, y)
}


#' @rdname matmatprod
#' @S3method matmatprod default
#' @S3method matmatprod blockvector
#' @S3method matmatprod blockmatrix
matmatprod <- function(x, y) {
  UseMethod("matmatprod", x)
}

matmatprod.default <- function(x, y) {
  if (!is.numeric(x) | !is.numeric(y))
    stop("\n'%mm%' requires numeric arguments")
  if (!is.multiblock(x) & !is.multiblock(y))
    x %*% y
}

matmatprod.blockmatrix <- function(x, y) {
  if (!is.multiblock(y))
    stop("\ninvalid argument for '%mm%'")
  
  # block-matrix and block-vector
  if (is.bvector(y))
    return(matmatprod_bmatrix_bvector(x, y))

  # block-matrix and block-matrix
  if (is.bmatrix(y))
    return(matmatprod_bmatrix_bmatrix(x, y))
}

matmatprod.blockvector <- function(x, y) {
  # block-vector and block-vector
  if (!is.bvector(y)) {
    stop("\ninvalid argument for '%mm%'")
  } else {
    return(matmatprod_bvector_bvector(x, y))
  }
}




# private function
matmatprod_bmatrix_bmatrix <- function(x = NULL, y = NULL) 
{
  if (!identical(colparts(x), rowparts(y)))
    stop("\nnon-conformable arguments for '%mm%'")
  
  xy = x %*% y
  as.blockmatrix(xy, rowparts(x), colparts(y))  
}


# private function
matmatprod_bmatrix_bvector <- function(x = NULL, y = NULL) 
{  
  if (!identical(colparts(x), parts(y)))
    stop("\nnon-conformable arguments for '%mm%'")
  
  xy = as.numeric(x %*% y)
  blockvector(xy, rowparts(x))
}


# private function
matmatprod_bvector_bvector <- function(x = NULL, y = NULL) 
{  
  if (!same_parts(x, y))
    stop("\nnon-conformable arguments for '%mm%'")
  
  sum(x * y)
}
