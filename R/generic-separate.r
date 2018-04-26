#' @title separate
#' 
#' @description
#' Separate a multiblock into blocks
#' 
#' @param x multiblock object
#' @export
#' @examples
#' # block-vector
#' bnum = blockvector(runif(10), c(5,5), 2)
#' 
#' # separate bnum
#' separate(bnum)
#' 
#' # block-matrix
#' a = matrix(runif(20), 5, 4)
#' dimnames(a) = list(1:5, paste("x", 1:4, sep=''))
#' A = blockmatrix(a, rowpart=c(2, 3), colpart=c(2, 2))
#' 
#' # separate A
#' separate(A)
separate <- function(x) {
  UseMethod("separate", x)
}


#' @S3method separate default
separate.default <- function(x) 
{
  if (!is.multiblock(x))
    stop("\n'separate()' requires a multiblock object")
}


#' @S3method separate blockvector
separate.blockvector <- function(x) 
{
  # get block partitions
  xparts = parts(x)
  # indexify
  index = unlist(mapply(rep, seq_along(xparts), xparts))
  # break blockvector apart
  blocks_list = vector("list", length(xparts))
  for (i in 1:length(xparts)) {
    blocks_list[[i]] = x[index==i]
  }
  names(blocks_list) = paste("block", seq_along(xparts), sep='_')
  blocks_list
}


#' @S3method separate blockmatrix
separate.blockmatrix <- function(x) 
{
  x_rparts = rowparts(x)
  x_cparts = colparts(x)
  
  rows = from_to(rowparts(x))
  cols = from_to(colparts(x))
  
  blocks_list = vector(mode="list", length=nblocks(x))
  block_labels = rep("", nblocks(x))
  nb = 1
  for (i in seq_along(x_rparts)) {
    irows = rows$from[i]:rows$to[i]
    for (j in seq_along(x_cparts)) {
      jcols = cols$from[j]:cols$to[j]
      blocks_list[[nb]] = x[irows,jcols]
      block_labels[nb] = paste("block", i, j, sep='_')
      nb = nb + 1
    }
  }
  # output
  names(blocks_list) = block_labels
  blocks_list
}
