#' Performance measures used in classifier evaluation
#' 
#' Classification error on test set
#' 
#' @details Performance measures that can be used in \code{\link{CrossValidationSSL}} and \code{\link{LearningCurveSSL}}
#' 
#' @rdname evaluation-measures
#' @param trained_classifier the trained classifier object
#' @param X_l design matrix with labeled object
#' @param y_l labels of labeled objects
#' @param X_u design matrix with unlabeled object
#' @param y_u labels of unlabeled objects
#' @param X_test design matrix with test object
#' @param y_test labels of test objects
#' @export
measure_accuracy <- function(trained_classifier, X_l,y_l,X_u,y_u,X_test,y_test) { 
  mean(y_test==predict(trained_classifier, X_test)) 
}
#' Classification error on test set
#' @describeIn evaluation-measures
#' @export
measure_error <- function(trained_classifier, X_l,y_l,X_u,y_u,X_test,y_test) { 
  1-mean(loss(trained_classifier, X_test, y_test)) 
}
#' Average loss on test objects
#' @describeIn evaluation-measures
#' @export
measure_losstest <- function(trained_classifier, X_l,y_l,X_u,y_u,X_test,y_test) { 
  mean(loss(trained_classifier, X_test, y_test)) 
}
#' Average loss on labeled objects
#' @describeIn evaluation-measures
#' @export
measure_losslab <- function(trained_classifier, X_l,y_l,X_u,y_u,X_test,y_test) { 
  mean(loss(trained_classifier, X_l, y_l)) 
}
#' Arevage loss on labeled and unlabeled objects
#' @describeIn evaluation-measures
#' @export
measure_losstrain <- function(trained_classifier, X_l,y_l,X_u,y_u,X_test,y_test) { 
  mean(loss(trained_classifier, rbind(X_l,X_u), unlist(list(y_l,y_u))))
} 
