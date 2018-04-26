#' @title Hadamard extended product
#' 
#' @description 
#' Hadamard product extended for multiblocks \cr
#' 
#' @details When multiplying multiblock objects, this product 
#' requires objects to have compatible block-dimensions
#' 
#' @param x a numeric multiblock
#' @param y a numeric multiblock
#' @usage x \%**\% y
#' @seealso \code{\link{hadmatprod}}, \code{\link{matmatprod}}
#' @export
#' @rdname hadhadprod
#' @examples
#' # vector
#' v = 1:5
#' v %**% v
#' # same as:  v * v
#' v * v
#' 
#' # block vector
#' bv = blockvector(v, parts=c(2, 3))
#' bv %**% bv
#' 
#' # matrix
#' a = matrix(1:20, 4, 5)
#' rownames(a) = paste("ind", 1:4, sep='')
#' colnames(a) = c("a1", "a2", "b1", "b2", "b3")
#' a %**% a
#' 
#' # block matrix
#' A = blockmatrix(a, c(2, 2), c(2, 3))
#' A %**% A
"%**%" <- function(x, y) {
  hadhadprod(x, y)
}


#' @rdname hadhadprod
#' @S3method hadhadprod default
#' @S3method hadhadprod blockvector
#' @S3method hadhadprod blockmatrix
#' @S3method hadhadprod matrix
#' @S3method hadhadprod numeric
hadhadprod <- function(x, y) {
  UseMethod("hadhadprod", x)
}

hadhadprod.default <- function(x, y) 
{
  if (!is.numeric(x) | !is.numeric(y))
    non_numeric_args("'%**%'")
  if (!is.multiblock(x) & !is.multiblock(y))
    x * y
}

hadhadprod.blockmatrix <- function(x, y) 
{
  # block-matrix and block-matrix
  if (!is.bmatrix(y)) {
    invalid_args("'%**%'")
  } else {
    return(hadhadprod_bmatrix_bmatrix(x, y))  
  }
}

hadhadprod.blockvector <- function(x, y) 
{
  # block-vector and block-vector
  if (!is.bvector(y)) {
    invalid_args("'%**%'")
  } else {
    return(hadhadprod_bvector_bvector(x, y))
  }
}

hadhadprod.matrix <- function(x, y) 
{
  # matrix and others non-multiblock
  if (!is.multiblock(y)) {
    return(x * y)
  } 
  # matrix and block-matrix only
  if (is.multiblock(y)) 
  {
    if (!is.bmatrix(y)) {
      invalid_args("'%**%'")
    } else {
      return(hadhadprod_matrix_bmatrix(x, y))
    }
  }
}

hadhadprod.numeric <- function(x, y) 
{
  # vector and others non-multiblock
  if (!is.multiblock(y)) {
    return(x * y)
  } 
  # vector and block-vector only
  if (is.multiblock(y)) 
  {
    if (!is.bvector(y)) {
      invalid_args("'%**%'")
    } else {
      return(hadhadprod_vector_bvector(x, y))
    }
  }
}



# private function
hadhadprod_bmatrix_bmatrix <- function(x = NULL, y = NULL) 
{
  if (!same_blockdim(x,y)) incompatible_args("'%**%'")
  # output
  x * y
}

# private function
hadhadprod_bvector_bvector <- function(x = NULL, y = NULL) 
{
  if (!same_parts(x, y)) incompatible_args("'%**%'")
  # output
  x * y
}

# private function
hadhadprod_matrix_bmatrix <- function(x = NULL, y = NULL) 
{
  if (!identical(dim(x), dims(y))) incompatible_args("'%**%'")
  
  # from and to indices
  rows = from_to(rowparts(y))
  cols = from_to(colparts(y))
  
  for (i in 1:brow(y)) 
  {
    for (j in 1:bcol(y)) 
    {
      # specified starting and ending indices
      irows = rows$from[i]:rows$to[i]
      jcols = cols$from[j]:cols$to[j]
      # hadamard product
      y[irows,jcols] = x[i,j] * y[irows,jcols]
    }
  }
  # output
  y
}

# private function
hadhadprod_vector_bvector <- function(x = NULL, y = NULL) 
{
  if (length(x) != nblocks(y)) incompatible_args("'%**%'")
  
  # output
  x_aux = rep(x, parts(y))
  x_aux * y
}
