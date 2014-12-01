#' @include Classifier.R
setClass("GRFClassifier",
         representation(theta="matrix",unlabels="ANY",scaling="ANY",optimization="ANY",intercept="ANY"),
         prototype(name="GRFClassifier",scaling=NULL), 
         contains="Classifier")

#' @title Gaussian Random Fields and Harmonic functions
#' 
#' @param X matrix; Design matrix for labeled data
#' @param y factor or integer vector; Label vector
#' @param X_u matrix; Design matrix for unlabeled data
#' @param kernel TODO: kernel function??
#' @param eta The influence of the external classifier
#' @param y_u Labels given by external classifier
#' @param CMN Should the Class Mass Normalization heuristic be applied? (default: TRUE)
#'  
#' @export
GRFClassifier<-function(X,y,X_u,kernel=NULL,y_u=NULL,eta=0.1,CMN=TRUE) {
  #only do evaluation if we need predictions
  #TODO: include external classifier scores
  
  Y<-model.matrix(~as.factor(y)-1)
  
  W<-NULL
#   if (is.null(kernel)) {
#     W<-kernelMatrix(rbfdot(sigma = 1), rbind(X,X_u))
#   }
  W<-NULL
  #TODO: Learn the kernel!
  
  harmonic_function(W,Y)
  
  #TODO: What is the output?
#   return(new("GRFClassifier",
#              modelform,
#              scaling,
#              X,
#              y,
#              X_u,
#              ))
}

setMethod("predict", signature(object="GRFClassifier"),function(object,newdata,...) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,y,scaling=object@scaling,intercept=object@intercept)
  X<-ModelVariables$X 
})

# Direct R Translation of Xiaojin Zhu's Matlab code
harmonic_function <- function(W,Y) {
  l <- nrow(Y) # the number of labeled points
  n <- nrow(W) # total number of points
  
  # the graph Laplacian L=D-W
  L <- diag(colSums(W)) - W;
  
  # the harmonic function.
  fu <- -solve(L[(l+1):n, (l+1):n]) %*% L[(l+1):n, 1:l] %*% Y
  
  # compute the CMN solution
  #q = colSums(Y)+1 % the unnormalized class proportion estimate from labeled data, with Laplace smoothing
  #TODO: fu_CMN = fu .* repmat(q./sum(fu), n-l, 1);
}
# function [fu, fu_CMN] = harmonic_function(W, fl)
# % [fu, fu_CMN] = harmonic_function(W, fl)
# %
# % Semi-supervised learning with basic harmonic function.
# % That is eq (5) in "Semi-Supervised Learning Using Gaussian Fields 
# % and Harmonic Functions".  Xiaojin Zhu, Zoubin Ghahramani, John Lafferty.  
# % The Twentieth International Conference on Machine Learning (ICML-2003).
# %
# % Input:
#   %   W: n*n weight matrix.  The first L entries(row,col) are for labeled data,
# %      the rest for unlabeled data.  W has to be symmetric, and all
# %      entries has to be non-negative.  Also note the graph may be disconnected,
# %      but each connected subgraph has to have at least one labeled point.
# %      This is to make sure the sub-Laplacian matrix is invertible.
# %   fl: L*c label matrix.  Each line is for a labeled point, in 
# %      one-against-all encoding (all zero but one 1).  For example in binary
# %      classification each line would be either "0 1" or "1 0".
# %
# % Output:
#   %   fu: The harmonic solution, i.e. eq (5) in the ICML paper.
# %      (n-L)*c label matrix for the unlabeled points.  Each row is for
# %      an unlabeled point, and each column for a class.  The class with the 
# %      largest value is the predicted class of the unlabeled point.
# %   fu_CMN: The solution after applying Class Mass Normalization (CMN), as
# %      in eq (9) of the ICML paper.  The class proportions are the maximum
# %      likelihood (frequency) estimate from labeled data fl.  The CMN heuristic
# %      is known to sometimes improve classification accuracy.  As before,
# %      the class with the largest value in a row is the predicted class.
# %
# % Note:
#   %   If there is a warning "Matrix is singular to working precision.", 
# %   and fu is all NaN, it usually means the original graph is disconnected, 
# %   and some connected components have no labeled data.  To solve the problem:
#   %  1. Try a different graph 
# %   2. Use a different algorithm, e.g. the Gaussian Field kernels
# %
# % 
# % Please note there is no warranty.  In fact it is not even a software, but
# % merely research code.  Feel free to modify it for your work. 
# % Xiaojin Zhu, zhuxj@cs.cmu.edu
# % 2004
# 
# l = size(fl, 1); % the number of labeled points
# n = size(W, 1); % total number of points
# 
# % the graph Laplacian L=D-W
# L = diag(sum(W)) - W;
# 
# % the harmonic function.
# fu = - inv(L(l+1:n, l+1:n)) * L(l+1:n, 1:l) * fl;
# 
# % compute the CMN solution
# q = sum(fl)+1; % the unnormalized class proportion estimate from labeled data, with Laplace smoothing
# fu_CMN = fu .* repmat(q./sum(fu), n-l, 1);
