#' @title Similar vectors?
#' 
#' @description
#' Compares if two vectors are similar for testing convergence
#' FALSE otherwise
#' 
#' @param x a vector
#' @param y a vector
#' @param tol tolerance threshold
#' @return whether \code{x} and \code{y} can be considered similar
#' @export
#' @keywords internal
similar <- function(x, y, tol = 0.0001) 
{
  if (abs(sum(x - y)) < tol) TRUE else FALSE
  #  if (vnorm(vnorm(x - y)) < tol) TRUE else FALSE
}



# @title Test if an object has one-dimension
# 
# @description
# Returns TRUE if an object is a vector or one-dimensional matrix,
# FALSE otherwise
# 
# @param x an R object
# @return whether x is one-dimensional
# @keywords internal
check_one_dim <- function(x)
{
  one_dim = TRUE
  if (lacks_dim(x)) {
    if (is.list(x)) one_dim = FALSE
  } else {
    if (dim(x)[1L] > 1 && dim(x)[2L] > 1)
      one_dim = FALSE
  }
  # output
  one_dim
}




#' @title Non Conformable Arguments
#' 
#' @description
#' Stops execution of a binary operator when arguments are 
#' non-conformable
#' 
#' @param operator name of operator
#' @return stop message
#' @keywords internal
incompatible_args <- function(operator) {
  stop(paste("\nnon-conformable arguments for", operator))
}

#' @title Invalid Arguments
#' 
#' @description
#' Stops execution of a binary operator when arguments are 
#' invalid
#' 
#' @param operator name of operator
#' @return stop message
#' @keywords internal
invalid_args <- function(operator) {
  stop(paste("\ninvalid arguments for", operator))
}

#' @title Requires Numeric Arguments
#' 
#' @description
#' Stops execution when arguments are non-numeric
#' 
#' @param operator name of operator
#' @return stop message
#' @keywords internal
non_numeric_args <- function(operator) {
  stop(paste("\n", operator, " requires numeric arguments", sep=""))
}
