#' @title Hadamard-Matrix extended product
#' 
#' @description 
#' Block-Matrix product for multiblocks \cr
#' 
#' @details When multiplying multiblock objects, this product 
#' requires objects to have compatible block-dimensions. \cr
#' When multiplying blockmatrices, this product requires a rowblock
#' (i.e. horiwontal blockmatrix) and a colblock (i.e. a vertical 
#' blockmatrix)
#' 
#' @param x a numeric multiblock
#' @param y a numeric multiblock
#' @usage x \%*m\% y
#' @seealso \code{\link{hadhadprod}}, \code{\link{matmatprod}}
#' @export
#' @rdname hadmatprod
#' @examples
#' # vector
#' v = 1:5
#' v %*m% v
#' # same as:  v * v
#' v %*% v
#' 
#' # block vector
#' bv = blockvector(v, parts=c(2, 3))
#' bv %*m% bv
#' 
#' # blockmatrices
#' X = blockmatrix(matrix(1:12, 3, 4), rowparts = c(3), colparts=c(2, 2))
#' Y = blockmatrix(matrix(rep(c(1,2), each=6), 4, 3, byrow=TRUE),
#'                 rowparts=c(2,2), colparts=c(3))
#'
#' X %*m% Y
"%*m%" <- function(x, y) {
  hadmatprod(x, y)
}


#' @rdname hadmatprod
#' @S3method hadmatprod default
#' @S3method hadmatprod blockvector
#' @S3method hadmatprod blockmatrix
#' @S3method hadmatprod matrix
#' @S3method hadmatprod numeric
hadmatprod <- function(x, y) {
  UseMethod("hadmatprod", x)
}

hadmatprod.default <- function(x, y) 
{
  if (!is.numeric(x) | !is.numeric(y))
    non_numeric_args("'%*m%'")
  if (!is.multiblock(x) & !is.multiblock(y))
    x %*% y
}

hadmatprod.blockmatrix <- function(x, y) 
{
  if (!is.multiblock(y)) {
    invalid_args("'%*m%'")
  }
  # bmatrix by bmatrix
  if (is.bmatrix(y)) {
    return(hadmatprod_bmatrix_bmatrix(x, y))  
  }
  # bmatrix by bvector
  if (is.bvector(y)) {
    return(hadmatprod_bmatrix_bvector(x, y))  
  }
}

hadmatprod.blockvector <- function(x, y) 
{
  # block-vector and block-vector
  if (!is.bvector(y)) {
    invalid_args("'%*m%'")
  } else {
    return(hadmatprod_bvector_bvector(x, y))
  }
}

hadmatprod.matrix <- function(x, y) 
{
  # matrix and others non-multiblock
  if (!is.multiblock(y)) {
    return(x %*% y)
  } 
  # matrix and block-matrix only
  if (is.multiblock(y)) 
    invalid_args("'%*m%'")
}

hadmatprod.numeric <- function(x, y) 
{
  # vector and others non-multiblock
  if (!is.multiblock(y)) {
    return(x %*% y)
  } 
  # vector and block-vector only
  if (is.multiblock(y)) 
    invalid_args("'%*m%'")
}




# private function
hadmatprod_bmatrix_bmatrix <- function(x = NULL, y = NULL) 
{
  if (!is.rowblock(x))
    stop("\n'x' must be a rowblock")
  if (!is.colblock(y))
    stop("\n'y' must be a colblock")
  
  if (!same_nblocks(x, y))
    incompatible_args("'%*m%'")
  
  if (!identical(colparts(x), rowparts(y)))
    incompatible_args("'%*m%'")
  
  # product
  result = blockmatrix(matrix(0, nrow(x), ncol(y)*bcol(x)), 
                       rowparts(x), rep(ncol(y),bcol(x)))
  # from and to indices
  cols = from_to(colparts(result))
  
  for (j in 1:bcol(x)) 
  {
    # extract 'ij' blocks 
    X_ij = get_block(x, i=1, j=j)
    Y_ji = get_block(y, i=j, j=1)
    # specified starting and ending indices
    jcols = cols$from[j]:cols$to[j]
    # usual product
    result[,jcols] = X_ij %*% Y_ji
  }
  
  # output
  result
}


# private function
hadmatprod_bmatrix_bvector <- function(x = NULL, y = NULL) 
{
  if (!identical(colparts(x), parts(y)))
    incompatible_args("'%*m%'")
  
  ydummy = vector_to_dummy(parts(y)) * y
  xy = x %*% ydummy
  blockmatrix(xy, rparts(x), rep(1, dims(y)))
}


# private function
hadmatprod_bvector_bvector <- function(x = NULL, y = NULL) 
{
  if (!same_parts(x, y))
    incompatible_args("'%*m%'")
  
  x_aux = separate(x)
  y_aux = separate(y)
  result = rep(0, nblocks(x))
  for (i in 1:nblocks(x))
    result[i] = sum(x_aux[[i]] * y_aux[[i]])
  # output
  result
}

