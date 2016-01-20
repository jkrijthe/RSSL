#' Biased (maximum likelihood) estimate of the covariance matrix
#' 
#' @param X matrix with observations
#' @export
cov_ml<-function(X) { ((nrow(X)-1)/nrow(X))*cov(X) }

#' Calculate the standard error of the mean from a vector of numbers
#' @param x numeric; vector for which to calculate standard error
#' @export
stderror <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
# http://stackoverflow.com/questions/2676554/in-r-how-to-find-the-standard-error-of-the-mean

#' Use mclapply conditional on not being in RStudio
#' @param X vector
#' @param FUN function to be aplpied to the elements of X
#' @param ... optional arguments passed to FUN
#' @param mc.cores number of cores to use
#' @export
clapply <- function(X,FUN,...,mc.cores=getOption("mc.cores", 2L)) {
  if (Sys.getenv("RSTUDIO") == 1 | mc.cores == 1) {
    lapply(X,FUN,...)
  } else {
    parallel::mclapply(X,FUN,...,mc.cores=mc.cores)
  }
}

#' Preprocess the input to a classification function
#'
#' The following actions are carried out: 1. data.frames are converted to matrix form and labels converted to an indicator matrix 2. An intercept column is added if requested 3. centering and scaling is applied if requested.
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
    modelform <- X
    problem <- SSLDataFrameToMatrices(X,y)
    out <- PreProcessing(problem$X,problem$y,problem$X_u,scale=scale,intercept=intercept,x_center=x_center)
    out$modelform <- X
    return(out)
  } else if ((is.matrix(X) || is.data.frame(X)) && (is.factor(y))) {
    
    modelform <- NULL
    
    if (is.data.frame(X)) {
      if (intercept) {
        X <- model.matrix(~., X)
        if (!is.null(X_u)) { X_u <- model.matrix(~., X_u) }
      } else {
        X <- model.matrix(~.-1, X)
        if (!is.null(X_u)) { X_u <- model.matrix(~.-1, X_u) }
      }
    } else {
      if (intercept) { 
        X<-cbind(matrix(1,nrow(X),1),X)
        if (!is.null(X_u)) { X_u<-cbind(matrix(1,nrow(X_u),1),X_u) }
      }
    }
    
    classnames<-levels(y)
    if (length(classnames)>2) {
      Y <- model.matrix(~y-1, data.frame(y))
    } else {
      Y <- model.matrix(~y-1, data.frame(y))[,1,drop=FALSE]
    }

  } else {
    stop("No valid input for X, y and X_u.")
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
  
  return(list(X=X,y=y,Y=Y,X_u=X_u,classnames=classnames,scaling=scaling,modelform=modelform))
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
#' @param classnames Vector with class names
#' @return list object with the following objects:
#' \item{X}{design matrix of the labeled data}
#' \item{y}{integer vector indicating the labels of the labeled data}
#' @export
PreProcessingPredict<-function(modelform,newdata,y=NULL,classnames=NULL,scaling=NULL,intercept=FALSE) {
  if (!is.null(modelform)) {
    problem<- SSLDataFrameToMatrices(modelform,newdata)
    return(PreProcessingPredict(NULL,problem$X,problem$y,classnames=classnames,scaling=scaling,intercept=intercept))
  } else {
    if (!(is.matrix(newdata) || is.data.frame(newdata))) { stop("Training data and Testing data don't match.")}
    if (is.data.frame(newdata)) {
      if (intercept) {
        X <- model.matrix(~.,newdata)
      } else {
        X <- model.matrix(~.-1,newdata)
      }
    } else {
      X <- newdata
    }
    if (intercept) { X<-cbind(matrix(1,nrow(X),1),X) } # Add intercept term
  }

  if (!is.null(scaling)) {
    if (intercept) {
      X[,2:ncol(X)]<-predict(scaling,X[,2:ncol(X),drop=FALSE]) 
    } else {
      X[,1:ncol(X)]<-predict(scaling,X[,1:ncol(X),drop=FALSE])
    }
  }

  Y <- classlabels_to_indicatormatrix(y,classnames) 

  return(list(X=X,Y=Y,y=y))
}

classlabels_to_indicatormatrix <- function(y,classnames) {
  if (is.null(y)) {return(NULL)}
  Y <- model.matrix(~y-1, data.frame(y))
  if (length(classnames)==2) { Y <- Y[,1,drop=FALSE] }
  return(Y)
}
