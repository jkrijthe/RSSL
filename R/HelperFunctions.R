#' @title Maximum Likelihood Covariance estimate
#' 
#' @param X matrix with observations
#' @export
cov_ml<-function(X) { ((nrow(X)-1)/nrow(X))*cov(X) }

#' @title Standard Error of a vector
#' @param x numeric; vector for which to calculate standard error
#' @export
stderror <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x))) # http://stackoverflow.com/questions/2676554/in-r-how-to-find-the-standard-error-of-the-mean

#' @title Convert data.frame to matrices for semi-supervised learners
#'
#' Given a formula object and a data.frame, extract the design matrix X for the labeled observations, X_u for the unlabeled observations and y for the labels of the labeled observations
#'
#' @usage SSLDataFrameToMatrices(model,D,intercept=TRUE)
#'
#' @param model Formula object with model
#' @param D data.frame object with objects
#' @param intercept Whether to include an intercept in the design matrices
#' @return list object with the following objects:
#' \item{X}{design matrix of the labeled data}
#' \item{X_u}{design matrix of the unlabeled data}
#' \item{y}{integer vector indicating the labels of the labeled data}
#' \item{classnames}{names of the classes corresponding to the integers in y}
#' @export
SSLDataFrameToMatrices <- function(model,D,intercept=TRUE) {
  # Split labeled and unlabelled data
  classname<-all.vars(model)[1] # determine the name of the dependent variable
  
  D_l <- D[!is.na(D[,classname]),] # labeled data
  D_u <- D[is.na(D[,classname]),] # unlabeled data
  
  # Data.Frame to Matrices
  
  X<-model.matrix(model, D_l)
  y<-as.factor(data.matrix(D_l[,classname]))
  if (!is.factor(y)) stop("This is not a classification problem. Please supply a factor target.")
  classnames <- levels(y)
  
  X_u<-NULL
  if (nrow(D_u)>0) {
    D_u[,classname] <- 1
    X_u <- model.matrix(model, D_u)
  }
  
  if (!intercept) {
    selected.columns<-colnames(X) != "(Intercept)"
    X <- X[, selected.columns] # Remove intercept
    if (nrow(D_u)>0) X_u <- X_u[, selected.columns,drop=FALSE]
  }
  y<-as.integer(y)
  
  return(list(X=X, y=y, X_u=X_u, classnames=classnames))
  
  # Data.Frame to Matrices
#   mf <- model.frame(formula=model, data=D)
#   y <- model.response(mf)
#   classnames<-levels(y)
#   if (!is.factor(y)) stop("This is not a classification problem. Please supply a factor target.")
#   X <- model.matrix(attr(mf, "terms"), data=mf)
#   X <- X[, colnames(X) != "(Intercept)"] # Remove intercept
#   Y <- model.matrix(~y-1)
}

#' Preprocess the input to a classification function
#'
#' The following actions are carried out: 1. data.frames are converted to matrix form and labels converted to integers 2. An intercept column is added if requested 3. centering and scaling is applied if requested.
#' 
#' @param X Design matrix, intercept term is added within the function
#' @param y Vector or factor with class assignments
#' @param X_u Design matrix of the unlabeled observations
#' @param scale If TRUE, apply a z-transform to the design matrix X
#' @param intercept Whether to include an intercept in the design matrices
#' @param x_center logical (default: TRUE); Whether the feature vectors should be centered
#' @return list object with the following objects:
#' \item{X}{design matrix of the labeled data}
#' \item{y}{integer vector indicating the labels of the labeled data}
#' \item{X_u}{design matrix of the unlabeled data}
#' \item{classnames}{names of the classes corresponding to the integers in y}
#' \item{scaling}{a scaling object used to scale the test observations in the same way as the training set}
#' \item{modelform}{a formula object containing the used model}
#' @export
PreProcessing<-function(X,y,X_u=NULL,scale=FALSE,intercept=FALSE,x_center=FALSE) {
  # Make sure we get a matrix from the model representation
  if (is(X,"formula") & is.data.frame(y)) {
    modelform<-X
    list2env(SSLDataFrameToMatrices(X,y,intercept=intercept),environment())
    classnames<-classnames
  } else if (is.matrix(X) & (is.factor(y) | is.vector(y))) {
    if (intercept) { 
      X<-cbind(matrix(1,nrow(X),1),X)
      if (!is.null(X_u)) { X_u<-cbind(matrix(1,nrow(X_u),1),X_u) }
    } # Add intercept term
    
    modelform<-NULL
    if (is.factor(y)) {
      classnames<-levels(y)
      y<-as.numeric(y)
    } else {
      classnames<-1:length(unique(y)) 
    }
  } else {
    stop("No valid input for X and y, see help.")
  }
  
  if (scale | x_center) {
    if (intercept) { 
      cols<-2:ncol(X) #Do not scale the intercept column
    } else { 
      cols<-1:ncol(X)
    }
    
    if (!is.null(X_u)) {
        Xe<-rbind(X,X_u)
        
        scaling<-scaleMatrix(Xe[,cols,drop=FALSE], center = TRUE, scale = scale)
        X[,cols]<-predict(scaling,X[,cols,drop=FALSE]) 
        X_u[,cols]<-predict(scaling,X_u[,cols,drop=FALSE])
    } else {
      
      scaling<-scaleMatrix(X[,cols,drop=FALSE], center = TRUE, scale = scale)
      X[,cols]<-predict(scaling,X[,cols,drop=FALSE]) 
    }

  } else {scaling=NULL}
  
  return(list(X=X,y=y,X_u=X_u,classnames=classnames,scaling=scaling,modelform=modelform))
}

#' Preprocess the input for a new set of test objects for classifier
#'
#' The following actions are carried out: 1. data.frames are converted to matrix form and labels converted to integers 2. An intercept column is added if requested 3. centering and scaling is applied if requested.
#'
#' 
#' @param modelform Formula object with model
#' @param newdata data.frame object with objects
#' @param y Vector or factor with class assignments (default: NULL)
#' @param scaling Apply a given z-transform to the design matrix X (default: NULL)
#' @param intercept Whether to include an intercept in the design matrices
#' @return list object with the following objects:
#' \item{X}{design matrix of the labeled data}
#' \item{y}{integer vector indicating the labels of the labeled data}
#' @export
PreProcessingPredict<-function(modelform,newdata,y=NULL,scaling=NULL,intercept=FALSE) {
if (!is.null(modelform)) {
  list2env(SSLDataFrameToMatrices(modelform,newdata,intercept=intercept),environment())
  X<-X
} else {
  if (!is.matrix(newdata)) { stop("Training data and Testing data don't match.")}
  X<-newdata
  if (intercept) { X<-cbind(matrix(1,nrow(X),1),X) } # Add intercept term
}

if (!is.null(scaling)) {
  if (intercept) {
    X[,2:ncol(X)]<-predict(scaling,X[,2:ncol(X),drop=FALSE]) 
  } else {
    X[,1:ncol(X)]<-predict(scaling,X[,1:ncol(X),drop=FALSE])
  }
}

y<-as.numeric(y)

return(list(X=X,y=y))
}
