#' R Semi-Supervised Learning Package
#'
#' RSSL provides implementations for Semi-Supervised learning models, as well as some functions to aid in the evaluation of these procedures.
#'
#' Interface
#'
#' Most functions take a dataset as input and output a trained Classifier/Regression object, whose class is the class of a specific type of classifier/regression model. \code{predict} can then be used to generate predictions for new objects. \code{posterior} can be used to output the posterior probabilities of new objects for the different classes. \code{loss} outputs the loss used by the classifier on a set of new objects.
#'
#' Semi-Supervised Classifiers
#'
#' The following are currently working implementations of semi-supervised classifiers:
#' ICLeastSquaresClassifier
#' ICLinearDiscriminantClassifier
#' EMLinearDiscriminantClassifier
#' SelfLearning
#' 
#' Supervised Classifiers
#' 
#' NearestMeanClassifier
#' LinearDiscriminantClassifier
#' LogisticRegression
#' LogisticLossClassifier
#' LeastSquaresClassifier
#' 
#' Additional Functionality
#' 
#' ErrorCurve
#' ErrorCurveSSL
#' Crossvalidation
#' CrossvalidationTransductive
#' CrossvalidationSSL
#' 
#' Several toy datasets are provided through the following functions 
#' GenerateSlicedCookie
#' Generate2ClassGaussian
#' 
#' @importFrom kernlab vanilladot rbfdot kernelMatrix as.kernelMatrix
#' @import quadprog 
#' @import Matrix
#' @import ggplot2
#' @import dplyr
#' @import reshape2
#' @importFrom methods as is new
#' @importFrom stats binomial cov delete.response df dist fitted glm.fit median model.extract model.frame model.matrix model.response na.fail na.omit napredict nlm optim princomp rbeta rnorm runif terms var
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom MASS mvrnorm ginv
#' @importFrom limSolve linp
#' @importFrom flexclust kcca kccaFamily dist2
#' @useDynLib RSSL
#' @importFrom Rcpp sourceCpp
#' @docType package
#' @name RSSL
NULL