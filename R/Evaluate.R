#' @export
add_missinglabels_mar <- function(df,formula=NULL,prob=0.1) {
  if (!is.null(formula)) {
    df <- model.frame(formula,df)
  }
  n <- nrow(df)
  yu <- df[sample(1:n,ceiling(prob*n)),1]
  df[sample(1:n,ceiling(prob*n)),1] <- NA
  attr(df,"yu") <- yu
  return(df)
}

#' Convert data.frame to matrices for semi-supervised learners
#'
#' Given a formula object and a data.frame, extract the design matrix X for the labeled observations, X_u for the unlabeled observations and y for the labels of the labeled observations. Note: always removes the intercept
#'
#' @usage SSLDataFrameToMatrices(model,D)
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
    mf <- model.frame(1~., data=D, na.action=NULL)
  }
  y <- model.response(mf)
  classnames<-levels(y)
  if (!is.factor(y)) stop("This is not a classification problem. Please supply a factor target.")
  X <- model.matrix(attr(mf, "terms"), data=mf)
  X <- X[, colnames(X) != "(Intercept)"] # Remove intercept
  X_u <- X[is.na(y),,drop=FALSE]
  X <- X[!is.na(y),,drop=FALSE]
  y <- y[!is.na(y)]
  list(X=X, y=y, X_u=X_u, y_u=yu)
}

#' @export
df_to_matrices <- function(df,formula=NULL) {
  SSLDataFrameToMatrices(formula,df)
}

#' @export
split_dataset_ssl<-function(X, y, frac_train=0.8, frac_ssl=0.8) {
  n<-length(y)
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
