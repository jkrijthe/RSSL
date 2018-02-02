#' R Semi-Supervised Learning Package
#'
#' RSSL provides implementations for semi-supervised classifiers, as well as some functions to aid in the evaluation of these procedures.
#'
#' Most functions take a \code{formula} and \code{data.frame} or a \code{matrix} and \code{factor} as input and output a trained \code{Classifier object}, whose class is the class of a specific type of classifier model. \code{predict} can then be used to generate predictions for new objects, \code{decisionvalues} returns the decision values for new objects and \code{loss} outputs the loss used by the classifier evaluated on a set of new objects. 
#' 
#' For a complete list of functions, use library(help = "RSSL").
#' 
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
#' @docType package
#' @name RSSL
NULL