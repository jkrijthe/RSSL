#' @include Classifier.R
setClass("TSVM", 
         representation(),
         prototype(name="Transductive Support Vector Machine"), 
         contains="Classifier")

#' @title Transductive SVM classifier
#'
#' @param X matrix; Design matrix, intercept term is added within the function
#' @param y vector; Vector or factor with class assignments
#' @param X_u matrix; Design matrix of the unlabeled data, intercept term is added within the function
#' @param Clabeled numeric; Cost parameter of the SVM
#' @param Cunlabeled numeric; Cost parameter of the unlabeled objects
#' @param fracpos numeric; fraction of test points to assign to the first class
#' @param intercept TRUE if an intercept should be added to the model
#' @param scale If TRUE, apply a z-transform to all observations in X and X_u before running the regression
#' @param ... additional arguments
#' @inheritParams BaseClassifier
#' @return S4 object of class TSVM with the following slots:
#' \item{theta}{weight vector}
#' \item{classnames}{the names of the classes}
#' \item{modelform}{formula object of the model used in regression}
#' \item{scaling}{a scaling object containing the paramters of the z-transforms applied to the data}
#' \item{optimization}{the object returned by the optim function}
#' \item{unlabels}{the labels assigned to the unlabeled objects}
#' 
#' @export
TSVM <- function(X, y, X_u, Clabeled=0, Cunlabeled=0, fracpos=0.5, intercept=TRUE, x_center=TRUE, scale=TRUE, ...) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=intercept,x_center=x_center)
  X<-ModelVariables$X
  y<-ModelVariables$y
  X_u<-ModelVariables$X_u
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
#   modelform<-ModelVariables$modelform
# 
#   numpos<-ceiling(fracpos*nrow(X_u))
#   
#   svm_new<-solve_svm_qp()
#   w<-svm_new$w
#   slack<-svm_new$slack
# # 
#   #Assign num_plus objects with highest decision value to plus class
#   #TODO
#   
#   
#   Cminus<-10e5
#   Cplus<-10e5*num_plus/(k-num_plus)
# 
# 
#   while(Cminus<Cunlabeled | Cplus < Cunlabeled) {
#   	flip_pair<-nextpair()
#   	while(!is.na(flip_pair)) {
#   		y_unlabeled[flip_pair[1]]=-y_unlabeled[flip_pair[1]]
#   		y_unlabeled[flip_pair[2]]=-y_unlabeled[flip_pair[2]]
#   		svm_new<-solve_svm_qp()
#       w<-svm_new$w
#       slack<-svm_new$slack
#   		nextpair(Cminus, Cplus)
#   	}
#   	Cmin<-min(Cmin*2,Cunlabeled)
#   	Cplus<-min(Cplus*2,Cunlabeled)
#   }
}

solve_svm_qp<-function() {
  
}

nextpair<-function() {
  
}

