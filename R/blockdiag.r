#' @title Blockdiagonal matrix
#' 
#' @description Diagonalize a blockvactor, row-blockmatrix 
#' or a column-blockmatrix
#' 
#' @param x a blockvector or a blockmatrix
#' @return a block diagonal matrix
#' @aliases blockdiag bdiag
#' @export blockdiag bdiag
#' @examples
#' # create a blockvector
#' d = blockvector(data=1:4, parts=c(1,1,1,1), dims=4)
#' # blockdiagonal
#' blockdiag(d)
#' 
#' # create a regular matrix
#' a = matrix(runif(20), 5, 4)
#' 
#' # block-matrix with one single block 
#' A1 = blockmatrix(a, 5, 4) 
#' # block-matrix with one column block 
#' A2 = blockmatrix(a, c(2, 3), 4)
#' # block-matrix with one row block
#' A3 = blockmatrix(a, 5, c(2,2))
#' # block-matrix with 2 row-blocks and 2 col-blocks
#' A4 = blockmatrix(a, c(2, 3), c(2, 2))  
#' 
#' # block-diagonalizations
#' blockdiag(A1)  # TRUE
#' blockdiag(A2)  # FALSE
#' blockdiag(A3)  # TRUE
#' # blockdiag(A4)  # CANNOT
blockdiag <- function(x) 
{
  if (!is.multiblock(x))
    stop("\n'blockdiag()' requires a multiblock")
  
  if (is.bvector(x))
    return(diagonalize_bvector(x))
  
  if (is.megablock(x))
    x_diag = diagonalize_megablock(x)
  
  # one single block (do nothing)
  if (is.oneblock(x)) {
    x_diag = x
  } else {
    # diagonalize one row block
    if (is.rowblock(x))
      x_diag = diagonalize_rowblock(x)
    # diagonalize one column block
    if (is.colblock(x))
      x_diag = diagonalize_colblock(x)
  }
  # output
  x_diag
}

bdiag <- function(x) {
  blockdiag(x)
}


diagonalize_bvector <- function(x)
{
  # BlockDiagonal matrix
  BD = vector_to_dummy(parts(x)) * x
  # output
  if (has_names(x)) 
  {
    rownames(BD) = names(x)
    colnames(BD) = paste("block", 1:nblocks(x), sep=".")
  }
  BD  
}

diagonalize_rowblock <- function(x)
{
  row_parts = rep(nrow(x), bcol(x))
  # from and to indices
  cols = from_to(colparts(x))
  rows = from_to(row_parts)
  
  # BlockDiagonal matrix
  BD = blockmatrix(matrix(0, nrow(x)*bcol(x), ncol(x)), 
                   row_parts, colparts(x))
  # fill BD
  for (i in 1:bcol(x)) 
  {
    # specified starting and ending indices
    irows = rows$from[i]:rows$to[i]
    icols = cols$from[i]:cols$to[i]
    # select block ii
    BD[irows,icols] = x[,icols]
  }
  # output
#  dimnames(BD) = dimnames(x)
  rownames(BD) = rep(rownames(x), bcol(x))
  colnames(BD) = colnames(x)
  BD
}

diagonalize_colblock <- function(x)
{
  col_parts = rep(ncol(x), brow(x))
  # from and to indices
  rows = from_to(rowparts(x))
  cols = from_to(col_parts)
  
  # BlockDiagonal matrix
  BD = blockmatrix(matrix(0, nrow(x), ncol(x)*brow(x)), 
                   rowparts(x), col_parts)
  # fill BD
  for (i in 1:brow(x)) 
  {
    # specified starting and ending indices
    irows = rows$from[i]:rows$to[i]
    icols = cols$from[i]:cols$to[i]
    # select block ii
    BD[irows,icols] = x[irows,]
  }
  # output
#  dimnames(BD) = dimnames(x)
  rownames(BD) = rownames(x)
  colnames(BD) = rep(colnames(x), brow(x))
  BD
}

diagonalize_megablock <- function(x)
{
  # extract colmun blocks (in a list)
  col_blocks = vector("list", bcol(x))
  for (j in 1:bcol(x)) {
    col_blocks[[j]] = bdiag(colblock(x, j))
  }
  
  # mega-diagonal multiblock
  BD = matrix(0, sum(rowparts(x)) * bcol(x), sum(colparts(x)) * brow(x))
  # start-ending indices
  rows = from_to(sapply(col_blocks, nrow))
  cols = from_to(sapply(col_blocks, ncol))
  # assemble
  for (i in 1:bcol(x)) 
  {
    irows = rows$from[[i]]:rows$to[[i]]
    icols = cols$from[[i]]:cols$to[[i]]
    BD[irows,icols] = col_blocks[[i]]
  }
  # output
  blockmatrix(BD, rowparts=rowparts(x) * bcol(x), 
              colparts=colparts(x) * brow(x))
}



# old diagonalization (not used anymore)
slow_diagonalize_bvector <- function(x)
{
  col_parts = rep(1, nblocks(x))
  # from and to indices
  rows = from_to(parts(x))
  
  # BlockDiagonal matrix
  BD = blockmatrix(matrix(0, length(x), nblocks(x)), 
                   parts(x), col_parts)
  # fill BD
  for (i in 1:nblocks(x)) 
  {
    # specified starting and ending indices
    irows = rows$from[i]:rows$to[i]
    # select block ii
    BD[irows,i] = x[irows]
  }
  # output
  if (has_names(x)) 
  {
    rownames(BD) = names(x)
    colnames(BD) = paste("block", 1:nblocks(x), sep=".")
  }
  BD  
}



#' @title Get blocks in diagonal
#' 
#' @description Extracts those diagonal blocks from a given blockmatrix
#' 
#' @param x a blockmatrix
#' @return a block diagonal matrix
#' @export
#' @examples
#' # create a blockmatrix
#' bv = blockvector(1:5, parts=c(2, 3))
#' B = bv %*o% bv
#' 
#' # blockmatrix with blocks in diagonal
#' get_diag(B)
get_diag <- function(x) 
{
  if (!is.bmatrix(x))
    stop("\n'blockdiag()' requires a block-matrix")
  
  # from and to indices
  rows = from_to(rowparts(x))
  cols = from_to(colparts(x))
  
  # BlockDiagonal matrix
  BD = blockmatrix(matrix(0, nrow(x), ncol(x)), rowparts(x), colparts(x))
  for (i in 1:brow(x)) 
  {
    # specified starting and ending indices
    irows = rows$from[i]:rows$to[i]
    icols = cols$from[i]:cols$to[i]
    # select block ii
    BD[irows,icols] = x[irows,icols]
  }
  # output
  dimnames(BD) = dimnames(x)
  BD
}

