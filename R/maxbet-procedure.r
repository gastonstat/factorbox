#' @title MaxBet Procedure (No deflations)
#' 
#' @description Maxbet procedure algorithm (ver 30-sep-2013)
#' Mohamed's preferred output: Scores as list of 
#' matrices (blocks-by-dimensions)
#' 
#' @param X a block-matrix (object of class \code{"bmatrix"})
#' @param specs list with algorithm specifications
#' @return list with the following elements
#' @return \item{factorization}{list of factorization results containing
#' singular \code{values}, \code{Left} vectors (aka scores), 
#' and \code{Right} vectors (aka loadings)}
#' @return \item{firt_crit}{list of vectors with loadings}
#' @return \item{iters}{number of iterations}
#' @return \item{specs}{list of specifications}
#' @references 
#' Hanafi M., Ten Berge J.M.F. (2003) Global Optimality
#' of the Successive Maxbet Algorithm. 
#' \emph{Psychometrika}, \bold{68}, No. 1, 97-103.
#' 
#' van de Geer J.P. (1984) Linear relations among k sets of variables.
#' \emph{Psychometrika}, \bold{49}, 79-94.
#' 
#' @export
#' @examples
#' # load wines
#' data(wines)
#' 
#' # center wines
#' wines = scale(wines, scale=FALSE)
#' 
#' # wines as block-matrix
#' bwines = blockmatrix(wines, colparts=c(4,3,4,3))
#' 
#' # apply maxbet_comp
#' wmbc = maxbet_proc(bwines)
#' wmbc
maxbet_proc <- 
function(X, specs = NULL)
{
  # list of specifications:
  #   $tol      tolerance
  #   $maxiter  maximum number of iterations
  #   $crit     convergence criterion 
  #   $display  whether to display results
  #   $init     initialization
  
  if (is.null(specs)) {
    specs = list(
      tol = 0.001, 
      maxiter = 20, 
      crit = 1, 
      display = FALSE, 
      init = NULL)
  }
  
  # NULL vector to store fit criterion values
  fit_crit = NULL
  
  # start
  X_new = X
  
  # supermatrix
  XX = t(X_new) %mm% X_new
  # initial block-vector (should it be normalized?)
  v_old = blockvector(rep(1, ncol(X)), parts=colparts(X))
  
  # iterations
  iters = 1
  repeat
  {
    v_new = XX %mm% v_old
    v_new = (1 / bvnorm(v_new)) %**% v_new   # normalize by blocks
    # check convergence
    if (similar(v_old, v_new, tol=specs$tol)) break
    # store fit criterion
    fit_crit = c(fit_crit, vnorm(as.vector(v_old - v_new)))
    # update blockvector
    v_old = v_new
    # check number of iterations
    iters = iters + 1
    if (iters == specs$maxiter) {
      warning("\nmaximum number of iterations was reached")
      break
    }
  }
  
  # (non-normalized) Left vectors (aka scores)
  Left_vectors = X_new %*m% v_new
  # singular values
  sing_values = apply(Left_vectors, 2, vnorm)
  # (normalized) Left vectors
  Left_vectors = blockmatrix(Left_vectors %*% diag(1/sing_values),
                             rowparts(Left_vectors),
                             colparts(Left_vectors))
  # Right vectors (as blockmatrix)
  Right_vectors = blockmatrix(v_new)
  
  # factorization results
  factorization = list(values = sing_values,
                       Left = Left_vectors,
                       Right = Right_vectors)
  # output
  list(factorization = factorization,
       fit_crit = fit_crit,
       iters = iters,
       specs = specs)
}
