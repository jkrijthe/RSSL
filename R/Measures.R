#' Performance measures used in classifier evaluation
#' 
#' Classification accuracy on test set and other performance measure that can be used in \code{\link{CrossValidationSSL}} and \code{\link{LearningCurveSSL}}
#' 
#' @family RSSL utilities
#' 
#' @rdname evaluation-measures
#' @param trained_classifier the trained classifier object
#' @param X_l design matrix with labeled object
#' @param y_l labels of labeled objects
#' @param X_u design matrix with unlabeled object
#' @param y_u labels of unlabeled objects
#' @param X_test design matrix with test object
#' @param y_test labels of test objects
#' 
#' @export
measure_accuracy <- function(trained_classifier, 
                             X_l=NULL, y_l=NULL, 
                             X_u=NULL, y_u=NULL, 
                             X_test=NULL, y_test=NULL) { 
  mean(y_test==predict(trained_classifier, X_test)) 
}

#' @describeIn evaluation-measures Classification error on test set
#' @export
measure_error <- function(trained_classifier, 
                          X_l=NULL, y_l=NULL, 
                          X_u=NULL, y_u=NULL, 
                          X_test=NULL, y_test=NULL) { 
  1-mean(y_test==predict(trained_classifier, X_test))
}

#' @describeIn evaluation-measures Avererage Loss on test objects
#' @export
measure_losstest <- function(trained_classifier, 
                             X_l=NULL, y_l=NULL, 
                             X_u=NULL, y_u=NULL, 
                             X_test=NULL, y_test=NULL) { 
  mean(loss(trained_classifier, X_test, y_test)) 
}

#' @describeIn evaluation-measures Average loss on labeled objects
#' @export
measure_losslab <- function(trained_classifier, 
                            X_l=NULL, y_l=NULL, 
                            X_u=NULL, y_u=NULL, 
                            X_test=NULL, y_test=NULL) { 
  mean(loss(trained_classifier, X_l, y_l)) 
}

#' @describeIn evaluation-measures Average loss on labeled and unlabeled objects
#' @export
measure_losstrain <- function(trained_classifier, 
                              X_l=NULL, y_l=NULL, 
                              X_u=NULL, y_u=NULL, 
                              X_test=NULL, y_test=NULL) { 
  mean(loss(trained_classifier, rbind(X_l,X_u), unlist(list(y_l,y_u))))
} 
