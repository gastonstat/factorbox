#' @title Block Vector
#' 
#' @description 
#' \code{blockvector} creates a block-vector from the given set of values \cr
#' \code{as.bvector} attempts to turn its argument into a block-vector \cr
#' \code{is.bvector} tests if its argument is a (strict) block-vector
#' 
#' @param data a vector
#' @param parts vector of partitions
#' @param dims integer indicating the number of blocks
#' @return An object of class \code{"blockvector"}
#' @seealso \code{\link{blockmatrix}}, \code{\link{vector}}
#' @export
#' @examples
#' # some vectors
#' vnum = runif(10)
#' vlog = sample(c(TRUE, FALSE), size=10, replace=TRUE)
#' vstr = letters[1:10]
#' 
#' # convert vectors into block-vectors
#' bnum = blockvector(vnum, 10, 1)
#' blog = blockvector(vlog, c(5,5), 2)
#' bstr = blockvector(vstr, c(3,3,4), 3)
#' 
#' # test if objects are blockvectors
#' is.bvector(vnum)  # FALSE
#' is.bvector(bnum)  # TRUE
#' 
#' # generate a vector
#' v = 1:10
#' 
#' # convert 'v' into a block-vector (one block)
#' bv = as.bvector(v)
#' bv
blockvector <- function(data, parts = NULL, dims = NULL)
{
  if (is_not_vector(data))
    stop("'data' must be a vector")
  
  # check partitions
  if (!is.null(parts)) {
    if (!is.atomic(parts) || any(parts <= 0))
      stop("Invalid 'parts'")
    if (sum(parts) != length(data))
      stop("'parts' doesn't match length of data")
  } else parts = length(data)
  
  # check dims
  if (!is.null(dims)) {
    if (length(dims) > 1 || dims < 0 || is.na(dims))
      stop("'dims' must be a positive integer")    
    if (dims != length(parts))
      stop("'dims' doesn't match 'parts'")
  } else dims = length(parts)
  
  # output
  attr(data, "blockdimension") = blockdimension(parts, dims)
  class(data) = "blockvector"
  data
}


#' @S3method print blockvector
print.blockvector <- function(x, ...) 
{
  cat('A block-vector:', '\n')
  #  cat('A block-vector with block dimensions:', '\n')
  #  print(attr(x, "blockdim"))
  #  cat('\n')
  attr(x, "blockdimension") = NULL
  print(unclass(x))
}


#' @rdname blockvector
#' @param x an R object
#' @aliases is.blockvector is.bvector
#' @export is.blockvector is.bvector
is.blockvector <- function(x) is(x, "blockvector")
is.bvector <- function(x) is.blockvector(x)


#' @rdname blockvector
#' @aliases as.blockvector as.bvector
#' @export as.blockvector as.bvector
as.blockvector <- function(x) blockvector(x)
as.bvector <- function(x) as.blockvector(x)


