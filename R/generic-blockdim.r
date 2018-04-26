#' @title block-dimension of a multi-block object
#' 
#' @description
#' \code{blockdim} returns the blockdimension \cr
#' \code{parts} returns the partitions \cr
#' \code{dims} returns the dimensions \cr
#' \code{ord} returns the order \cr
#' 
#' @param x a multiblock object
#' @seealso \code{\link{nblocks}}, \code{\link{same_blockdim}}
#' @export blockdim parts dims ord
#' @aliases blockdim parts dims ord
#' @S3method blockdim default
#' @S3method blockdim blockvector
#' @S3method blockdim blockmatrix
#' @S3method parts default
#' @S3method parts blockdimension
#' @S3method parts blockvector
#' @S3method parts blockmatrix
#' @S3method dims default
#' @S3method dims blockdimension
#' @S3method dims blockvector
#' @S3method dims blockmatrix
#' @S3method ord default
#' @S3method ord blockdimension
#' @S3method ord blockvector
#' @S3method ord blockmatrix
#' @examples
#' # blockvetcor
#' bv = blockvector(1:10, parts=c(5,5), dims=2)
#' blockdim(bv)
#' 
#' # blockdimension associated to a blockmatrix
#' M = matrix(1:70, 10, 7)
#' bm = blockmatrix(M, parts=c(5,5,3,4), dims=c(2,2))
#' blockdim(bm)
#' parts(bm)
#' dims(bm)
#' ord(bm)
#' 
#' # old blockvector
#' old_one = blockvector(1:10, 10, 1)
#' blockdim(old_one)
#'
#' # new blockvector
#' new_one = old_one
#' 
#' # replace blockdimension in new blockvector
#' blockdim(new_one) = list(parts=c(3,7), dims=2)
#' blockdim(new_one)
blockdim <- function(x) {
  UseMethod("blockdim", x)
}

blockdim.default <- function(x) {
  if (!is.multiblock(x))
    stop("\nblockdim() requires a multiblock")
}

blockdim.blockvector <- function(x) {
  attr(x, "blockdimension")
}

blockdim.blockmatrix <- function(x) {
  attr(x, "blockdimension")
}

parts <- function(x) {
  UseMethod("parts", x)
}

parts.default <- function(x) {
  if (!is.multiblock(x))
    stop("\nparts() requires a multiblock")
}

parts.blockdimension <- function(x) {
  x@parts
}

parts.blockvector <- function(x) {
  aux = attr(x, "blockdimension")
  aux@parts
}

parts.blockmatrix <- function(x) {
  aux = attr(x, "blockdimension")
  aux@parts
}

dims <- function(x) {
  UseMethod("dims", x)
}

dims.default <- function(x) {
  if (!is.multiblock(x))
    stop("\ndims() requires a multiblock")
}

dims.blockdimension <- function(x) {
  x@dims
}

dims.blockvector <- function(x) {
  aux = attr(x, "blockdimension")
  aux@dims
}

dims.blockmatrix <- function(x) {
  aux = attr(x, "blockdimension")
  aux@dims
}

ord <- function(x) {
  UseMethod("ord", x)
}

ord.default <- function(x) {
  if (!is.multiblock(x))
    stop("\nord() requires a multiblock")
}

ord.blockdimension <- function(x) {
  length(x@dims)
}

ord.blockvector <- function(x) {
  aux = attr(x, "blockdimension")
  length(aux@dims)
}

ord.blockmatrix <- function(x) {
  aux = attr(x, "blockdimension")
  length(aux@dims)
}



#' @rdname blockdim
#' @aliases "blockdim<-"
#' @usage blockdim(x) <- value
#' @param value a list with new \code{parts} and \code{dims}
#' @export "blockdim<-"
"blockdim<-" <- function(x, value) {
  UseMethod("blockdim<-", x)
}

#' @S3method "blockdim<-" default
"blockdim<-.default" <- function(x, value) 
{
  if (!is.multiblock(x))
    stop("\nA multiblock is required")

  if (!is.list(value) & length(value) != 2)
    stop("\n'value' must be a list with two elements")
  
  new_blockdim = blockdimension(value[[1L]], value[[2L]])
  old_blockdim = attr(x, "blockdimension")
  
  check_equivalent_blockdims(new_blockdim, old_blockdim)

  attr(x, "blockdimension") = new_blockdim
  x
}

# private function
check_equivalent_blockdims <- function(new_bdim, old_bdim)
{
  if (length(new_bdim@dims) != length(old_bdim@dims))
    stop("\ninvalid 'dims' in blockdimension 'value'")
  if (sum(new_bdim@parts) != sum(old_bdim@parts))
    stop("\ninvalid 'parts' in blockdimension 'value'")
  # else
  TRUE
}


#' @title Number of Blocks
#' 
#' @description return the number of blocks of a multiblock object
#' 
#' @param x a multiblock object
#' @return the number of blocks
#' @seealso \code{\link{blockdim}}
#' @export
#' @examples
#' # create some block-matrices
#' A = blockmatrix(matrix(1:12, 3, 4), 3, c(2,2))
#' B = blockmatrix(matrix(1:20, 5, 4), c(3,2), c(2,2))
#' 
#' # number of blocks
#' nblocks(A)
#' nblocks(B)
nblocks <- function(x) 
{
  if (is(x, "blockdimension")) {
    return(prod(dims(x)))
  } else {
    if (!is.multiblock(x))
      stop("\nnblocks() requires a multiblock")
    # total number of blocks
    return(prod(dims(x)))
  }
}



#' @title Test for same block dimension
#' 
#' @description
#' \code{same_blockdim} tests if two multiblock objects have 
#' same block-dimension \cr
#' \code{same_parts} tests if two multiblock objects have
#' same partitions \cr
#' \code{same_dims} tests if two multiblock objects have
#' same dimensions \cr
#' \code{same_nblocks} tests if two multiblock objects have
#' same number of blocks \cr
#' 
#' @param x a multiblock object
#' @param y a multiblock object
#' @seealso \code{\link{blockdim}}
#' @keywords multiblock
#' @aliases same_blockdim same_parts same_dims same_nblocks
#' @export same_blockdim same_parts same_dims same_nblocks
#' @examples
#' # blockvectors
#' bx = blockvector(1:10, parts=c(5,5), dims=2)
#' by = blockvector(1:10, parts=c(2,3,5), dims=3)
#' 
#' # blockmatrix
#' bm = blockmatrix(matrix(1:12, 3, 4), 3, c(2,2))
#' 
#' # test if same blockdimensions
#' same_blockdim(bx, bx) # TRUE
#' same_blockdim(bm, bm) # TRUE
#' same_blockdim(bx, bm) # FALSE
#' 
#' # test if same partitions
#' same_parts(bx, bx) # TRUE
#' same_parts(bm, bm) # TRUE
#' same_parts(bx, bm) # FALSE
#' 
#' # test if same dimensions
#' same_dims(bx, bx) # TRUE
#' same_dims(bm, bm) # TRUE
#' same_dims(bx, bm) # FALSE
#' 
#' # test if same number of blocks
#' same_nblocks(bx, bx) # TRUE
#' same_nblocks(bx, by) # FALSE
same_blockdim <- function(x, y)
{
  if (!is.multiblock(x) || !is.multiblock(y))
    stop("\n'same_blockdim()' requires multiblocks")
  # output
  (identical(parts(x), parts(y)) && identical(dims(x), dims(y)))
}

same_parts <- function(x, y)
{
  if (!is.multiblock(x) || !is.multiblock(y))
    stop("\n'same_parts()' requires multiblocks")
  # output
  identical(parts(x), parts(y))
}

same_dims <- function(x, y)
{
  if (!is.multiblock(x) || !is.multiblock(y))
    stop("\n'same_dims()' requires multiblocks")
  # output
  identical(dims(x), dims(y))
}

same_nblocks <- function(x, y)
{
  if (!is.multiblock(x) || !is.multiblock(y))
    stop("\n'same_nblocks()' requires multiblocks")
  # output
  (nblocks(x) == nblocks(y))
}
