setClass("Classifier",
         representation(name="character",
                        modelform="ANY",
                        classname="ANY",
                        classnames="ANY",
                        scaling="ANY"),
         prototype(modelform=NULL, classname=NULL, classnames=NULL,scaling=NULL)
)

#' Basic print method
setMethod("show", signature(object="Classifier"), function(object) {
  print(object@name)
  print(object@classnames)
})

setClass("SemiSupervisedClassifier",contains="Classifier")

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
  
  X_u<-matrix()
  if (nrow(D_u)>0) {
    D_u[,classname] <- 1
    X_u <- model.matrix(model, D_u)
  }
  
  if (!intercept) {
    selected.columns<-colnames(X) != "(Intercept)"
    X <- X[, selected.columns] # Remove intercept
    if (nrow(D_u)>0) X_u <- X_u[, selected.columns,drop=FALSE]
  }
  
  
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