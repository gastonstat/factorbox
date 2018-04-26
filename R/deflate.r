#' @title Deflation
#'
#' @description
#' Deflates a block-matrix based on factorization results
#' 
#' @details Deflation works only when column-partition is
#' identical to partitions in block-vector
#' @param X a block-matrix
#' @param factors a list with factorization results from a procedure
#' @param option what type of deflation ("loadings" or "scores")
#' @export
deflation <- function(X, factors, option = "loadings")
{
  if (!is.list(factors))
    stop("\n'deflation()' requires a 'factors' list")
  
  if (option == "loadings") {
    # deflation by loadings
    v = factors$Right
    X_residuals = X - X %mm% get_diag(v %mm% t(v))
    Wk = v
  } else {
    # deflation by scores
    Ck = factors$Left
    Wk = t(Ck) %*% X
    X_residuals = X - Ck %*% Wk
  }
  
  # block-matrix structure
  list(
    X_residuals = X_residuals,
    W_projection = Wk
  )
}
