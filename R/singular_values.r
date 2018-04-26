#' @title Singular Values
#' 
#' @description 
#' Internal function that calculates singular values
#' from a list of scores
#' 
#' @param score_list list of scores
#' @return An object of class \code{"blockmatrix"}
#' @export
#' @keywords internal
#' @examples
#' # create a list of scores
#' A = matrix(rnorm(15), 5, 3)
#' B = matrix(rnorm(15), 5, 3)
#' scores = list(A=A, B=B)
#' scores
#' 
#' # get singular values
#' singular_values(scores)
singular_values <- function(score_list)
{
  # singular values as norms of each score
  values_list = lapply(score_list, apply, 2, vnorm)
  # put in matrix format
  values = do.call("rbind", values_list)
  rownames(values) = paste("Dim", 1:nrow(values), sep="")
  colnames(values) = paste("Block", 1:ncol(values), sep="")
  # output as blockmatrix
  as.bmatrix(values, rowparts=nrow(values), colparts=rep(1,ncol(values)))
}
