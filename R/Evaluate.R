#' Throw out labels at random
#' 
#' Original labels are saved in attribute \code{y_true}
#' 
#' @param df data.frame; Data frame of interest
#' @param formula formula; Formula to indicate the outputs
#' @param prob numeric; Probability of removing the label
#' 
#' @family RSSL utilities
#' 
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
#' @param df data.frame; data.frame with y_true attribute
#' @family RSSL utilities
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
#' @family RSSL utilities
#' @param df data.frame; data.frame with y_true attribute
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
#' @family RSSL utilities
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
#' @family RSSL utilities
#' @export
df_to_matrices <- function(df,formula=NULL) {
  SSLDataFrameToMatrices(formula,df)
}

#' Randomly split dataset in multiple parts
#' 
#' The data.frame should start with a vector containing labels, or formula should be defined.
#' 
#' @param df data.frame; Data frame of interest
#' @param formula formula; Formula to indicate the outputs
#' @param splits numeric; Probability of of assigning to each part, automatically normalized, should be >1
#' @param min_class integer; minimum number of objects per class in each part
#' @return list of data.frames
#' 
#' @family RSSL utilities
#' @examples 
#' library(dplyr)
#' 
#' df <- generate2ClassGaussian(200,d=2)
#' dfs <- df %>% split_random(Class~.,split=c(0.5,0.3,0.2),min_class=1) 
#' names(dfs) <- c("Train","Validation","Test")
#' lapply(dfs,summary)
#' 
#' @export
split_random <- function(df,formula=NULL,splits=c(0.5,0.5),min_class=0) {
  if (!is.null(formula)) {
    df <- model.frame(formula,df)
  }
  
  if (length(splits)<2) { stop("Need at least two splits.")}
  if (!is.factor(df[[1]])) {stop("First variable needs to be a factor.")}
  splits <- splits/sum(splits)
  m <- length(splits)
  
  
  dfs <- list()
  
  idx <- seq_len(nrow(df))
  
  # index for each class
  idx_per_class <- lapply(levels(df[[1]]),
                          function(c) sample(idx[df[[1]]==c],m*min_class))
  
  # Add minimum required objects to each group
  idx_per_group <- list()
  for (i in seq_len(m)) {
    idx_per_group[[i]] <- unlist(sapply(idx_per_class,
                                        function(x) x[rep(seq_len(m),min_class)==i]),
                                 recursive = FALSE)
  }
  
  # Remove selected objects and shuffle
  if (length(unlist(idx_per_group))>0) idx <- sample(idx[-unlist(idx_per_group)])
  else idx <- sample(idx)
  
  # Randomly sample from remaining objects
  boundaries <- c(0,ceiling(cumsum(splits)*length(idx)))
  
  for (i in seq_len(m)) {
    idx_per_group[[i]] <- c(idx_per_group[[i]], 
                            idx[(boundaries[i]+1):boundaries[i+1]])
  }
  
  return(lapply(idx_per_group, function(ix) {df[ix,,drop=FALSE] }))
}

#' Create Train, Test and Unlabeled Set
#' @family RSSL utilities
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
