#'<brief desc>
#'
#'<full description>
#' @param X <what param does>
#' @param y <what param does>
#' @param lambda <what param does>
#' @param scale <what param does>
#' @param ... <what param does>
#' @export
LeastSquaresRegression <- function(X, y, lambda=0, scale=FALSE, ...) {
  
  warning("TODO: reimplement scaling")
  scaling<-NULL
  n<-nrow(X)
  m<-ncol(X)
  if (nrow(X)<ncol(X)) inv <- function(X) { ginv(X) }
  else inv <- function(X) { ginv(X) }
  
  theta <- inv(t(X) %*% X + n*lambda*diag(c(0,rep(1,(m-1))))) %*% (t(X) %*% y)
  
  new("LeastSquaresRegression",
      scaling=scaling,
      theta=theta)
}
