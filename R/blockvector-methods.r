#' @title Blockvector Norm
#' 
#' @description Computes the norm of a blockvector
#'
#' @param x a blockvector
#' @return norm of x
#' @seealso \code{\link{vnorm}}
#' @export
#' @examples
#' # numeric blockvector
#' bv = blockvector(1:10, c(4,6), 2)
#' vnorm(bv)
bvnorm <- function(x) 
{
  if (!is.numeric(x))
    stop("\nbvnorm() requires a numeric blockvector")
  
  # get block partitions
  xparts = parts(x)
  # indexify
  index = unlist(mapply(rep, seq_along(xparts), xparts))
  # calculate norm for each block
  res = rep(0, length(xparts))
  for (i in 1:length(xparts)) {
    res[i] = vnorm(x[index==i])
  }
  # output
  res
}
