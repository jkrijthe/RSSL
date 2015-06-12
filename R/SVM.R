#' @include Classifier.R
setClass("SVM",
         representation(scaling="ANY",alpha="ANY",bias="ANY",kernel="ANY",Xtrain="ANY",intercept="ANY",time="ANY"),
         prototype(name="Support Vector Machine"),
         contains="Classifier")

#' SVM Classifier
#'
#' @param X Design matrix
#' @param y Labels of the observations
#' @param C Cost variable
#' @param method Estimation procedure c("Dual","Primal","BGD","SGD","Pegasos")
#' @param scale Whether a z-transform should be applied (default: TRUE)
#' @param intercept Whether an intercept should be added (default: FALSE)
#' @param eps Small value to ensure positive definiteness of the matrix in the QP formulation
#' @inheritParams BaseClassifier
#' @return S4 object of type SVM
#' @export
SVM<-function(X, y, C=1, method="Dual",scale=TRUE,intercept=FALSE,kernel=NULL,eps=1e-9) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,scale=scale,intercept=intercept,x_center=TRUE)
  X<-ModelVariables$X
  y<-ModelVariables$Y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  # Check for two classes and transform to {-1,1}
  if (length(classnames)!=2) stop("Dataset does not contain 2 classes")
  y<-as.numeric((y*2)-1)

  ## Start Implementation
  time.begin<-Sys.time()
  if (method=="Dual") {
    
    if (!is.null(kernel)) {
      if (inherits(kernel,"kernel")) {
        Xtrain<-X
        K<-kernelMatrix(kernel,X,X)
      }
    } else {
      Xtrain <- X
      K <- X %*% t(X)
    }
    
    Dmat <- (diag(y) %*% K %*% diag(y)) + eps*diag(nrow(X)) #Add small constant to diagonal to ensure numerical PSD
    dvec <- matrix(1, nrow(X), 1)
    Amat <- diag(nrow(X))
    Amat <- t(rbind(y,Amat,-Amat))
    bvec <- c(rep(0,nrow(X)+1),rep(-C,nrow(X)))
    
    opt_result <- solve.QP(Dmat, dvec, Amat, bvec, meq=1)
    alpha <- opt_result$solution*y
    SVs <- (abs(alpha) > 0.001) & (abs(alpha) < (C-0.001))

    bias <- -median(K[SVs,] %*% alpha - y[SVs])
    #TODO: check this: should we exclude objects not on the margin (alpha=1) in calculating b?
    #print(K[SVs,] %*% alpha - y[SVs])   
  }  else{
    stop("Unknown optimization method.")
  }
  
  time.passed<-Sys.time()-time.begin
  
  return(new("SVM",
             alpha=alpha,
             bias=bias,
             Xtrain=Xtrain,
             kernel=kernel,
             scaling=scaling,
             modelform=modelform,
             classnames=classnames,
             intercept=intercept,
             time=time.passed))
}

#' @rdname decisionvalues-methods
#' @aliases decisionvalues,SVM-method
setMethod("decisionvalues", signature(object="SVM"), function(object, newdata) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,y=NULL,scaling=object@scaling,intercept=object@intercept,classnames=object@classnames)
  X <- ModelVariables$X
  
  if (!is.null(object@kernel)) {
    output <- object@alpha %*% kernelMatrix(object@kernel,object@Xtrain,X) + object@bias
  } else {
    output <- object@alpha %*% object@Xtrain %*% t(X) + object@bias
  }
  
  return(as.numeric(output))
})


#' @rdname rssl-predict
#' @aliases predict,SVM-method
setMethod("predict", signature(object="SVM"), function(object, newdata) {
  output <- decisionvalues(object,newdata)
  factor(as.numeric(output>0),levels=0:1,labels=object@classnames)
})

#' Losses per object for SVM
#'
#' Hinge loss on new objects of a trained SVM
#' @rdname loss-methods
#' @aliases loss,LinearSVM-method
setMethod("loss", signature(object="SVM"), function(object, newdata, y=NULL) {
  ModelVariables <- PreProcessingPredict(object@modelform,newdata,y=y,object@scaling,intercept=TRUE,classnames=object@classnames)
  X <- ModelVariables$X
  Y <- ModelVariables$Y
  y <- as.numeric((Y*2)-1)
  
  output <- decisionvalues(object,newdata)
  d <- 1 - y * output
  d[d<0] <- 0
  return(d)
})
