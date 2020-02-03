#' @include LeastSquaresClassifier.R
setClass("EMLeastSquaresClassifier",
         representation(responsibilities="ANY",opt_res="ANY",intermediate="ANY"),
         prototype(name="Expectation Maximization Least Squares Classifier"),
         contains="LeastSquaresClassifier")

#' An Expectation Maximization like approach to Semi-Supervised Least Squares Classification
#' 
#' As studied in Krijthe & Loog (2016), minimizes the total loss of the labeled and unlabeled objects by finding the weight vector and labels that minimize the total loss. The algorithm proceeds similar to EM, by subsequently applying a weight update and a soft labeling of the unlabeled objects. This is repeated until convergence.
#' 
#' By default (method="block") the weights of the classifier are updated, after which the unknown labels are updated. method="simple" uses LBFGS to do this update simultaneously. Objective="responsibility" corresponds to the responsibility based, instead of the label based, objective function in Krijthe & Loog (2016), which is equivalent to hard-label self-learning.
#' 
#' @param scale Should the features be normalized? (default: FALSE)
#' @param eps Stopping criterion for the minimization
#' @param verbose logical; Controls the verbosity of the output
#' @param alpha numeric; the mixture of the new responsibilities and the old in each iteration of the algorithm (default: 1)
#' @param method character; one of "block", for block gradient descent or "simple" for LBFGS optimization (default="block")
#' @param objective character; "responsibility" for hard label self-learning or "label" for soft-label self-learning
#' @param init objective character; "random" for random initialization of labels, "supervised" to use supervised solution as initialization or a numeric vector with a coefficient vector to use to calculate the initialization
#' @param max_iter integer; maximum number of iterations
#' @param beta numeric; value between 0 and 1 that determines how much to move to the new solution from the old solution at each step of the block gradient descent
#' @param save_all logical; saves all classifiers trained during block gradient descent
#' @references Krijthe, J.H. & Loog, M., 2016. Optimistic Semi-supervised Least Squares Classification. In International Conference on Pattern Recognition (To Appear).
#' @inheritParams BaseClassifier
#' @family RSSL classifiers
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' 
#' set.seed(1)
#'
#'df <- generate2ClassGaussian(200,d=2,var=0.2) %>% 
#'  add_missinglabels_mar(Class~.,prob = 0.96)
#'
#'# Soft-label vs. hard-label self-learning
#'classifiers <- list(
#'  "Supervised"=LeastSquaresClassifier(Class~.,df),
#'  "EM-Soft"=EMLeastSquaresClassifier(Class~.,df,objective="label"),
#'  "EM-Hard"=EMLeastSquaresClassifier(Class~.,df,objective="responsibility")
#')
#'
#'df %>% 
#'  ggplot(aes(x=X1,y=X2,color=Class)) +
#'  geom_point() +
#'  coord_equal() +
#'  scale_y_continuous(limits=c(-2,2)) +
#'  stat_classifier(aes(linetype=..classifier..),
#'                  classifiers=classifiers)
#'                  
#' @export
EMLeastSquaresClassifier <- function(X, y, X_u, x_center=FALSE, scale=FALSE, verbose=FALSE, intercept=TRUE,lambda=0, eps=10e-10, y_scale=FALSE, alpha=1,beta=1, init="supervised", method="block", objective="label", save_all=FALSE, max_iter=1000) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=intercept,x_center=FALSE)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  
  
  Y<-ModelVariables$Y[,1,drop=FALSE]
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  if(length(classnames)!=2) { stop("EMLeastSquaresClassifier requires 2 classes.")}
  
  n <- nrow(X)
  m <- ncol(X)
  k <- ncol(Y)
  
  Xe<-rbind(X,X_u)
  
  opt_res <- NULL
  intermediate <- NULL
  
  if (y_scale) {
    y_scale <- colMeans(Y)
  } else {
    y_scale <- rep(0,ncol(Y))
  }
  
  Y <- sweep(Y,2,y_scale)
  
  if (nrow(X)<ncol(X)) inv <- function(X) { ginv(X) }
  else inv <- function(X) { ginv(X) }
  
  w_sup <- inv(t(X) %*% X + n*lambda*diag(c(0,rep(1,(m-1))))) %*% (t(X) %*% t(t(Y)))
  
  if (intercept) {
    XeInv <- inv(t(Xe) %*% Xe + n*lambda*diag(c(0,rep(1,(m-1))))) 
  } else {
    XeInv <- inv(t(Xe) %*% Xe + n*lambda*diag(rep(1,m)))
  }
  
  if (method=="block") {
    if (intercept) {
      theta <- inv(t(X) %*% X + n*lambda*diag(c(0,rep(1,(m-1))))) %*% (t(X) %*% t(t(Y)))
    } else {
      theta <- inv(t(X) %*% X + n*lambda*diag(rep(1,m))) %*% (t(X) %*% t(t(Y)))
    }
    
    if (init=="random") {
      resp <- runif(nrow(X_u))
    } else if (init=="supervised") {
      resp <- X_u %*% theta
    } else if (is.numeric(init)) {
      theta <- init
      resp <- X_u %*% theta
    }
  
    resp_old <- rep(Inf,nrow(X_u))
    
    if (save_all) {
      intermediate_resp <- list(list(resp))
      intermediate_theta <- list(list(theta))
    }
    
    
    iterations <- 0
    while (sum(abs(resp-resp_old))>eps && iterations<max_iter) {
      Ye <- rbind(Y,matrix(resp,ncol=1))
      
      theta_old <- theta
      theta <- XeInv %*% (t(Xe) %*% t(t(Ye)))
      
      theta <- beta*theta + (1-beta) * theta_old
      
      resp_old <- resp
      
      resp <- X_u %*% theta
  
      if (objective=="responsibility" || objective=="hard") {
        resp <- as.integer(resp>0.5)
      } else if (objective=="label" || objective=="soft") {
        resp <- pmin(1,pmax(0,resp))
      } else if (objective=="contrastive") {
        resp <- as.integer(X_u %*% theta > X_u %*% w_sup)
      } else {
        stop("Objective not known.")
      }
      
      resp <- alpha * resp + (1-alpha) * resp_old
      
      if (save_all) {
        intermediate_resp <- c(intermediate_resp,list(resp))
        intermediate_theta <- c(intermediate_theta,list(theta))
      }
      
      iterations <- iterations + 1
      if (verbose) print(sum(abs(resp-resp_old)))
    }
    
    if (save_all) { intermediate <- list(intermediate_resp,intermediate_theta) }
    if (verbose) { cat("Number of iterations: ",iterations,"\n")}
    opt_res <- list(counts=iterations)
    
  } else if (method=="simple") {
    
    if (init=="random") {
      theta <- c(rnorm(ncol(Xe)),runif(nrow(X_u)))
    } else if (init=="supervised") {
      theta <- c(w_sup, X_u %*% w_sup)
    }
    
    if (objective=="label" || objective=="soft") {
      opt_res <- optim(theta,
                       loss_minmin_lsy,
                       gr=gradient_minmin_lsy,
                       Xe=Xe,Y=Y,X_u=X_u,
                       method="L-BFGS-B",
                       control=list(maxit=1000),
                       lower=c(rep(-Inf,ncol(X)),
                               rep(0.0-y_scale,nrow(X_u))),
                       upper=c(rep(Inf,ncol(X)),
                               rep(1.0-y_scale,nrow(X_u))))
      theta <- opt_res$par
      resp <- theta[-c(1:ncol(Xe))]
      theta <- matrix(theta[1:ncol(Xe)])
    } else if (objective=="responsibility" || objective=="hard") {
      opt_res <- optim(theta,
                       loss_minmin_lsq,
                       gr=gradient_minmin_lsq,
                       Xe=Xe,Y=Y,X_u=X_u,X=X,
                       method="L-BFGS-B",
                       control=list(maxit=1000),
                       lower=c(rep(-Inf,ncol(X)),
                               rep(0.0,nrow(X_u))),
                       upper=c(rep(Inf,ncol(X)),
                               rep(1.0,nrow(X_u))))
      theta <- opt_res$par
      resp <- theta[-c(1:ncol(Xe))]
      theta <- matrix(theta[1:ncol(Xe)])
    } else if (objective=="contrastive") {
      opt_res <- optim(theta,
                       loss_minmin_contrastive_ls,
                       gr=gradient_minmin_contrastive_ls,
                       Xe=Xe,Y=Y,X_u=X_u,X=X,w_sup=w_sup,
                       method="L-BFGS-B",
                       control=list(maxit=1000),
                       lower=c(rep(-Inf,ncol(X)),
                               rep(0.0,nrow(X_u))),
                       upper=c(rep(Inf,ncol(X)),
                               rep(1.0,nrow(X_u))))
      theta <- opt_res$par
      resp <- theta[-c(1:ncol(Xe))]
      theta <- matrix(theta[1:ncol(Xe)])
    } 
  } else {
    stop("Unknown method")
  }
  
  new("EMLeastSquaresClassifier", 
      classnames=classnames,
      scaling=scaling,
      theta=theta,
      modelform=modelform,
      intercept=intercept,
      responsibilities=as.numeric(resp),
      y_scale=y_scale,
      opt_res=opt_res,
      intermediate=intermediate)
}

loss_minmin_lsy <- function(theta,Xe,Y,X_u) {
  w <- theta[1:ncol(Xe)]
  Ye <- rbind(Y,matrix(theta[-c(1:ncol(Xe))],ncol=1))
  
  sum((Xe %*% w - Ye)^2)
}

gradient_minmin_lsy <- function(theta,Xe,Y,X_u) {
  w <- theta[1:ncol(Xe)]
  u <- theta[-c(1:ncol(Xe))]
  Ye <- rbind(Y,matrix(u,ncol=1))

  c(2 * t(Xe) %*% Xe %*% w - 2 * t(Xe) %*% Ye,
    -2 * (X_u %*% w - u))
}

# Only for 0,1 encoding
loss_minmin_lsq <- function(theta,Xe,Y,X_u,X) {
  w <- theta[1:ncol(Xe)]
  q <- theta[-c(1:ncol(Xe))]
  Ye <- rbind(Y,matrix(theta[-c(1:ncol(Xe))],ncol=1))
  
  sum((X %*% w - Y)^2) + sum(q * ((X_u %*% w)-1)^2) + sum((1-q) * ((X_u %*% w)-0)^2)
}

#Only for 0,1 encoding
gradient_minmin_lsq <- function(theta,Xe,Y,X_u,X) {
  w <- theta[1:ncol(Xe)]
  u <- theta[-c(1:ncol(Xe))]
  Ye <- rbind(Y,matrix(u,ncol=1))
  
  c(2 * t(Xe) %*% Xe %*% w - 2 * t(Xe) %*% Ye,
    -2 * (X_u %*% w - 0.5))
}

loss_minmin_contrastive_ls <- function(theta,Xe,Y,X_u,X,w_sup) {
  w <- theta[1:ncol(Xe)]
  q <- theta[-c(1:ncol(Xe))]
  Ye <- rbind(Y,matrix(theta[-c(1:ncol(Xe))],ncol=1))
  
  sum((Xe %*% w - Ye)^2) -sum((Xe %*% w_sup - Ye)^2) 
}

gradient_minmin_contrastive_ls <- function(theta,Xe,Y,X_u,X,w_sup) {
  w <- theta[1:ncol(Xe)]
  u <- theta[-c(1:ncol(Xe))]
  Ye <- rbind(Y,matrix(u,ncol=1))
  
  c(2 * t(Xe) %*% Xe %*% w - 2 * t(Xe) %*% Ye,
    -2 * (X_u %*% w - X_u %*% w_sup))
}