#' Throw out labels at random
#' 
#' @param df data.frame; Data frame of interest
#' @param formula formula; Formula to indicate the outputs
#' @param prob numeric; Probability of removing the label
#' @export
add_missinglabels_mar <- function(df,formula=NULL,prob=0.1) {
  if (!is.null(formula)) {
    df <- model.frame(formula,df)
  }
  n <- nrow(df)
  y_true <- df[,1]
  df[sample(1:n,ceiling(prob*n)),1] <- NA
  attr(df,"y_true") <- y_true
  return(df)
}

#' Access the true labels when they are stored as an attribute in a data frame
#' @param df data.frame;
#' @export
true_labels <- function(df) {
  stopifnot(is.data.frame(df))
  if (!is.null(attr(df,"y_true"))) {
    attr(df,"y_true")
  } else {
    stop("data.frame does not contain true labels")
  }
}

#' Access the true labels for the objects with missing labels when they are stored as an attribute in a data frame
#' @param df data.frame;
#' @export
missing_labels <- function(df) {
  stopifnot(is.data.frame(df))
  
  if (!is.null(attr(df,"y_true"))) {
    attr(df,"y_true")[is.na(df[,1])]
  } else {
    stop("data.frame does not contain true labels")
  }
}

#' Convert data.frame to matrices for semi-supervised learners
#'
#' Given a formula object and a data.frame, extract the design matrix X for the labeled observations, X_u for the unlabeled observations and y for the labels of the labeled observations. Note: always removes the intercept
#'
#' @param model Formula object with model
#' @param D data.frame object with objects
#' @return list object with the following objects:
#' \item{X}{design matrix of the labeled data}
#' \item{X_u}{design matrix of the unlabeled data}
#' \item{y}{integer vector indicating the labels of the labeled data}
#' \item{classnames}{names of the classes corresponding to the integers in y}
#' @export
SSLDataFrameToMatrices <- function(model,D) {
  yu <- attr(df,"yu")
  
  # Data.Frame to Matrices
  if (!is.null(model)) {
    mf <- model.frame(model, data=D, na.action=NULL)
  } else {
    mf <- model.frame(paste0(names(D)[[1]],"~."), data=D, na.action=NULL)
  }
  y <- model.response(mf)
  classnames<-levels(y)
  if (!is.factor(y)) stop("This is not a classification problem. Please supply a factor target.")
  X <- model.matrix(attr(mf, "terms"), data=mf)
  X <- X[, colnames(X) != "(Intercept)",drop=FALSE] # Remove intercept
  X_u <- X[is.na(y),,drop=FALSE]
  X <- X[!is.na(y),,drop=FALSE]
  y <- y[!is.na(y)]
  list(X=X, y=y, X_u=X_u, y_u=yu)
}

#' Convert data.frame with missing labels to matrices
#' @param df data.frame; Data
#' @param formula formula; Description of problem
#' @export
df_to_matrices <- function(df,formula=NULL) {
  SSLDataFrameToMatrices(formula,df)
}

#' Create Train, Test and Unlabeled Set
#' @param X matrix; Design matrix
#' @param y factor; Label vector
#' @param frac_train numeric; Fraction of all objects to be used as training objects
#' @param frac_ssl numeric; Fraction of training objects to used as unlabeled objects
#' 
#' @export
split_dataset_ssl<-function(X, y, frac_train=0.8, frac_ssl=0.8) {
  n <- length(y)
  idx_train<-sample(1:n,size=ceiling(frac_train*n))
  n_lab<-length(idx_train) #Number of labeled objects
  idx_labeled<-idx_train[sample(1:n_lab,size=ceiling((1-frac_ssl)*n_lab))]
  
  return(list(X=X[idx_labeled,,drop=FALSE],
              y=y[idx_labeled], 
              X_u=X[setdiff(idx_train,idx_labeled),,drop=FALSE],
              y_u=y[setdiff(idx_train,idx_labeled)],
              X_test=X[-idx_train,,drop=FALSE],
              y_test=y[-idx_train])
  )
}
