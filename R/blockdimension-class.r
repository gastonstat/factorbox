# private function to validate blockdimension
validate_blockdimension <- function(object) 
{
  errors <- character()
  ## check parts
  if (!is_numeric_vector(object@parts)) {
    msg <- "slot parts must be a numeric vector"
    errors <- c(errors, msg)
  }
  if (any(object@parts < 1L)) {
    msg <- "slot parts must be positive"
    errors <- c(errors, msg)
  }
  
  ## check dims
  if (!is_numeric_vector(object@dims)) {
    msg <- "slot dims must be a numeric vector"
    errors <- c(errors, msg)
  }
  if (any(object@dims < 1L)) {
    msg <- "slot dims must be positive"
    errors <- c(errors, msg)
  }
  
  if (sum(object@dims) != length(object@parts)) {
    msg <- "dims and parts are incompatible"
    errors <- c(errors, msg)
  }
  
  if (length(errors) == 0) 
    TRUE
  else
    errors
}


#' @title blockdimension class
#'
#' @description
#' blockdimension is an S4 class representing the concept of block-dimension. 
#' It contains two slots: "parts" (partitions) and "dims" (dimensions)
#' 
#' @name blockdimension-class
#' @rdname blockdimension-class
#' @exportClass blockdimension
setClass(Class = "blockdimension",
         slots = c(parts = "integer", dims = "integer"),
         prototype = prototype(parts = 1L, dims = 1L),
         validity = validate_blockdimension)


#' @docType methods
#' @rdname blockdimension-class
#' @aliases show,blockdimension-method
#' @export
setMethod("show", signature(object = "blockdimension"),
          function(object) {
            cat("partitions:", "\n")
            print(object@parts)
            cat("\ndimensions:", "\n")
            print(object@dims)
          })



#' @title Create a blockdimension object
#' 
#' @description
#' blockdimension creates a blockdimension object with the specified values.
#' 
#' @export blockdimension
#' @param parts an integer positive vector with the partitions
#' @param dims an integer positive vector with the block structure
#' @return a blockdim object
#' @seealso \code{\link{parts}}
#' @keywords multiblock classes
#' @examples
#' # blockdimension associated to a blockvector
#' blockdimension(parts=c(5,5), dims=2)
#' 
#' # blockdimension associated to a blockmatrix
#' blockdimension(parts=c(5,5,3,4), dims=c(2,2))
#' 
blockdimension <- function(parts, dims)
{
  if (!is.numeric(parts) || !is.numeric(dims))
    stop("\n'parts' and 'dims' must be numeric")
  if (has_missing(parts))
    stop("\n'parts' contains missing values")
  if (has_missing(dims))
    stop("\n'dims' contains missing values")
  
  # output
  new("blockdimension",
      parts = as.integer(parts),
      dims = as.integer(dims))
}


#' @title Test if x is multiblock
#' 
#' @description
#' If an object has blockdim return TRUE, otherwise FALSE
#' 
#' @param x an R object
#' @seealso \code{\link{blockdimension}}
#' @keywords multiblock
#' @export
#' @examples
#' # a blockvector
#' x = 1:10
#' bx = blockvector(x, parts=c(5,5), dims=2)
#' 
#' # test if multiblock
#' is.multiblock(x) # FALSE
#' is.multiblock(bx) # TRUE
is.multiblock <- function(x) {
  if(!is.null(attr(x, "blockdimension"))) TRUE else FALSE
}
