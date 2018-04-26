#' @title Describe
#' 
#' @description
#' Provide an overall description of an R object
#' 
#' @param x an R object
#' @export
#' @examples
#' # four objects
#' some_vector = 1:10
#' some_matrix = matrix(1:20, 5, 4)
#' some_bvector = blockvector(1:10, c(5,5), 2)
#' some_bmatrix = blockmatrix(some_matrix, c(3,2), c(2,2))
#' 
#' # describe objects
#' describe(some_vector)
#' describe(some_matrix)
#' describe(iris)
#' describe(some_bvector)
#' describe(some_bmatrix)
describe <- function(x) {
  UseMethod("describe", x)
}

#' @S3method describe default
describe.default <- function(x) {
  cat('Object description', '\n\n')
  cat("class:  ", class(x), '\n')
  cat("mode:   ", mode(x), '\n')
  cat("length: ", length(x), '\n')
}

#' @S3method describe matrix
describe.matrix <- function(x) {
  cat('Object description', '\n\n')
  cat("class:  ", class(x), '\n')
  cat("mode:   ", mode(x), '\n')
  cat("dim:    ", dim(x), '\n')
}

#' @S3method describe data.frame
describe.data.frame <- function(x) {
  cat('Object description', '\n\n')
  cat("class:  ", class(x), '\n')
  cat("mode:   ", mode(x), '\n')
  cat("dim:    ", dim(x), '\n')
}

#' @S3method describe blockvector
describe.blockvector <- function(x) {
  cat('Object description', '\n\n')
  cat("class:   ", class(x), '\n')
  cat("mode:    ", mode(x), '\n')
  cat("length:  ", length(x), '\n')
  cat("blocks:  ", nblocks(x), '\n')
  cat("parts:   ", parts(x), '\n')
}

#' @S3method describe blockmatrix
describe.blockmatrix <- function(x) {
  cat('Object description', '\n\n')
  cat("class:       ", class(x), '\n')
  cat("mode:        ", mode(x), '\n')
  cat("dim:         ", dim(x), '\n')
  cat("blocks:      ", nblocks(x), '\n')
  cat("row blocks:  ", brow(x), '\n')
  cat("col blocks:  ", bcol(x), '\n')
}
