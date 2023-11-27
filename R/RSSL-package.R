#' @keywords internal
"_PACKAGE"

#' @importFrom kernlab vanilladot rbfdot kernelMatrix as.kernelMatrix
#' @import quadprog 
#' @import Matrix
#' @import ggplot2
#' @import dplyr
#' @import reshape2
#' @importFrom methods as is new .hasSlot
#' @importFrom stats binomial cov delete.response df dist fitted glm.fit median model.extract model.frame model.matrix model.response na.fail na.omit napredict nlm optim princomp rbeta rnorm runif terms var formula
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom MASS mvrnorm ginv
#' @importFrom cluster pam
#' @useDynLib RSSL, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL