#' @include LeastSquaresClassifier.R
setClass("ICLeastSquaresClassifier", 
         representation(theta="matrix",optimization="ANY",unlabels="ANY"), 
         prototype(name="ICLeastSquaresClassifier"), 
         contains="LeastSquaresClassifier")

#' Implicitly Constained Least Squares Classifier
#'
#' Implicitly constrained semisupervised learning with quadratic loss. Least squares regression is used treating classes as targets (1 for one class, 2 for the other). We find an (fractional) labelling of the unlabeled objects, whose least squares regression solution minimizes the least squares loss on the labeled training data only. This is equivalent to finding a weighting of the unlabeled objects belonging to either of the two classes.
#'
#' @param X Design matrix, intercept term is added within the function
#' @param y Vector or factor with class assignments
#' @param X_u Design matrix of the unlabeled data, intercept term is added within the function
#' @param lambda1 Regularization parameter in the unlabeled+labeled data regularized least squares
#' @param lambda2 Regularization parameter in the labeled data only regularized least squares
#' @param intercept TRUE if an intercept should be added to the model
#' @param x_center logical; Whether the feature vectors should be centered
#' @param scale logical; If TRUE, apply a z-transform to all observations in X and X_u before running the regression
#' @param method Either "LBFGS" for solving using L-BFGS-B gradient descent or "QP" for a quadratic programming based solution
#' @param projection One of "supervised", "semisupervised" or "euclidean"
#' @param lambda_prior numeric; prior on the deviation from the supervised mean y
#' @param trueprob numeric; true mean y for all data
#' @param ... additional arguments
#' @return S4 object of class ICLeastSquaresClassifier with the following slots:
#' \item{theta}{weight vector}
#' \item{classnames}{the names of the classes}
#' \item{modelform}{formula object of the model used in regression}
#' \item{scaling}{a scaling object containing the paramters of the z-transforms applied to the data}
#' \item{optimization}{the object returned by the optim function}
#' \item{unlabels}{the labels assigned to the unlabeled objects}
#' @examples
#' data(testdata)
#' w1 <- LeastSquaresClassifier(testdata$X, testdata$y, intercept = TRUE,x_center = FALSE, scale=FALSE)@@theta
#' w2 <- ICLeastSquaresClassifier(testdata$X, testdata$y, testdata$X_u, intercept = TRUE, x_center = FALSE, scale=FALSE)@@theta
#' plot(testdata$X[,1],testdata$X[,2],col=factor(testdata$y),asp=1)
#' points(testdata$X_u[,1],testdata$X_u[,2],col="darkgrey",pch=16,cex=0.5)
#' abline((0.5-w1[1])/w1[3],-w1[2]/w1[3],lty=2)
#' abline((0.5-w2[1])/w2[3],-w2[2]/w2[3],lty=1)
#' @export
ICLeastSquaresClassifier<-function(X, y, X_u=NULL, lambda1=0, lambda2=0, intercept=TRUE,x_center=FALSE,scale=FALSE,method="LBFGS",projection="supervised",lambda_prior=0,trueprob=NULL,eps=10e-10,y_scale=FALSE) {
  
  #   if (!is.factor(y)) { stop("Input labels should be a factor!")}
  #if (length(levels(y))!=2) { stop("We need a two class problem.")}
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=intercept,x_center=x_center)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  y <- ModelVariables$Y
  
  if (!(nrow(X)==length(y))) { stop("Length of y and number of rows in X should be the same.")}
  
  if (y_scale) {
    y_scale <- mean(y)
    y <- y-y_scale
  } else {
    y_scale <- 0
  }
  
  if ((nrow(X)+nrow(X_u))<ncol(X)) inv <- function(M) { ginv(M) }
  else inv <- function(M) { ginv(M) } #Another possibility: chol2inv(chol(M))
  
  if (nrow(X_u)==0) Xe <- X
  else Xe <- rbind(X,X_u)
  
  if (!is.null(trueprob)) {
    mean_y <- trueprob
  } else {
    mean_y <- mean(y)
  }
  
  m <- ncol(Xe)

  C <- inv(t(Xe) %*% Xe + nrow(Xe)*lambda1*diag(c(0,rep(1,(m-1)))))
  C2 <- inv(t(X) %*% X)
  F <- X %*% C 
  G <- X_u %*% t(F) %*% F
  
  ## Quadratic Programming implementation
  if (method=="QP") {
    if (projection=="supervised") {
      dvec <- X_u %*% C %*% t(X) %*% y - G %*% t(X) %*% y
      Dmat <- G %*% t(X_u)
      Dmat <- Dmat + eps*diag(nrow(Dmat))
    } else if (projection=="semisupervised")  {
      warning("Does not correspond with LBFGS solution yet")
      dvec <- - X_u %*% C %*% t(X) %*% y + X_u %*% C2 %*% t(X) %*% y
      Dmat <- X_u %*% C %*% t(X_u)
      Dmat <- Dmat + eps*diag(nrow(Dmat))
    } else if (projection=="euclidean")  {
      dvec <- - X_u %*% C %*% C %*% t(X) %*% y + X_u %*% C %*% C2 %*% t(X) %*% y
      Dmat <- X_u %*% C %*% C %*% t(X_u)
      Dmat <- Dmat + eps*diag(nrow(Dmat))
    } else {
      stop("There is no QP implementation of this projection. Check the spelling of the projection argument.")
    }
    
    # Box constraints
    Amat <- t(rbind(diag(nrow(X_u)), -diag(nrow(X_u))))
    bvec <- c(rep(0,nrow(X_u)), rep(-1,nrow(X_u)))
    meq <- 0
    
    # Hard constraint on the posterior of the weight being equal to the prior of the labels
    if (lambda_prior!=0) {
      if (ncol(y)>1) stop("Mean constraint not implemented for multi-class")
      Amat <- cbind(1,Amat)
      bvec <- c(mean_y*nrow(X_u),bvec)
      meq <- 1
    }    
  
    # Multiclass
    if (ncol(y)>1) {
      Dmat <- bdiag(rep(list(Dmat),ncol(y)))
      bvec <- rep(bvec,ncol(y))
      Amat <- bdiag(rep(list(Amat),ncol(y)))
      Amat <- cbind(kronecker(matrix(1,ncol(y),1),diag(nrow(X_u))), as.matrix(Amat))
      bvec <-  c(rep(1,nrow(X_u)),bvec)
      meq <- nrow(X_u)
    }
    
    unlabels<-solve.QP(Dmat, dvec, Amat, bvec, meq=meq, factorized=FALSE)$solution
    #unlabels<-ipop(-dvec, Dmat, l=rep(0,nrow(X_u)), u=rep(1,nrow(X_u)),A=diag(nrow(X_u)),b=rep(0,nrow(X_u)),r=rep(1,nrow(X_u)))@primal # Works better in some cases TODO: check which is faster
    opt_result<-0
  }
  ## LBFGS implementation
  else if (method=="LBFGS") {
    if (projection=="supervised") {
      opt_func <- function(theta) {
        theta <- matrix(theta)
        labels <- theta
        theta <- C %*% t(Xe) %*% rbind(matrix(y),theta)
        if (lambda2>0) { 
          if (intercept) { return(mean((X %*% theta - y)^2) + lambda2 * sum(theta[2:nrow(theta),1]^2) + lambda_prior*(mean(labels)-mean_y)^2)}
          else  { return(mean((X %*% theta - y)^2) + lambda2 * sum(theta^2) + lambda_prior*(mean(labels)-mean_y)^2) }
        }
        else { return(mean((X %*% theta - y)^2) + lambda_prior*(mean(labels)-mean_y)^2) }
      }
      
      O1 <- 2/nrow(X) * G %*% t(X) %*% y
      O2 <- 2/nrow(X) * G %*% t(X_u)
      O3 <- 2/nrow(X) * X_u %*% t(F) %*% y
      
      if (lambda2>0) {
        O4<- 2 * lambda2 * X_u %*% C %*% diag(c(0,rep(1,(m-1)))) %*% t(F) %*% y
        O5<- 2 * lambda2 * X_u %*% C %*% diag(c(0,rep(1,(m-1)))) %*% C %*% t(X_u)
      }
      
      opt_grad <- function(theta) {
        theta <- matrix(theta)
        reg3 <- lambda_prior*2*(mean(theta)-mean_y)/nrow(theta)
        if (lambda2>0) return(O1 + O2 %*% theta - O3 + O4 + O5 %*% theta + reg3)
        else return(O1 + O2 %*% theta - O3 + reg3  ) 
      }
      
      theta <- rep(0.5,nrow(X_u))
      
#       browser()
#       library("numDeriv")
#       lambda_prior<- 1
#       grad(opt_func,c(0.1,0.9),method="simple")
#       opt_grad(c(0.1,0.9))
      
        # Bounded optimization
      opt_result <- optim(theta, opt_func, gr=opt_grad, method="L-BFGS-B", lower=0.0, upper=1.0, control=list(fnscale=1))
      theta<-opt_result$par
      
      unlabels<-theta
    }
    else if (projection=="semisupervised") {
      m<-ncol(Xe)
      
      w_sup<- matrix(inv(t(X)%*%X+nrow(X)*lambda2*diag(c(0,rep(1,(m-1))))) %*% t(X) %*% y,ncol=1)
      
      C <- inv(t(Xe) %*% Xe + nrow(Xe)*lambda1*diag(c(0,rep(1,(m-1))))) # Check this
      F <- Xe %*% C 
      G <- X_u %*% t(F) %*% F
      O1 <- X_u %*% C %*% t(X_u)
      O2 <- X_u %*% C %*% t(X) %*% y
      O3 <- X_u %*% w_sup 
      
      opt_func_projection <- function(theta) {
          # Return a Mahanalobis type distance between the semi-supervised and supervised coefficients
          theta<-matrix(theta)
          w_semi <- matrix(C %*% t(Xe) %*% rbind(matrix(y),theta),ncol=1)

          t(theta) %*% O1 %*% theta + 2 * t(theta) %*% O2 - 2 * t(theta) %*% O3 + lambda_prior*(mean(theta)-mean_y)^2
        }
        
        opt_grad_projection <- function(theta) {
          theta<-matrix(theta)
          # Check gradient
          #cbind(matrix(grad(opt_func_projection,theta),ncol=1), 
          #        2 * O1 %*% theta + 2 * O2 - 2 * O3)
          reg3 <- lambda_prior*2*(mean(theta)-mean_y)/nrow(theta)
          2 * O1 %*% theta + 2 * O2 - 2 * O3 + reg3
        }

      theta <- rep(0.5,nrow(X_u))

      # Bounded optimization
      opt_result <- optim(theta, opt_func_projection, gr=opt_grad_projection, method="L-BFGS-B", lower=0.0, upper=1.0, control=list(fnscale=1))
      theta<-opt_result$par
      
      unlabels<-theta
    } else if (projection=="semisupervisedold") {
      m<-ncol(Xe)
      
      C <- inv(t(Xe) %*% Xe) 
      F<- Xe %*% C 
      G <- X_u %*% t(F) %*% F
     
      w_sup<- matrix(inv(t(X)%*%X) %*% t(X) %*% y,ncol=1)
      
            opt_func_projection <- function(theta) {
              # Return a Mahanalobis type distance between the semi-supervised and supervised coefficients
              
              theta<-matrix(theta)
              w_semi <- matrix(C %*% t(Xe) %*% rbind(matrix(y),theta),ncol=1)
              if (lambda1>0) {
                return((1/nrow(Xe))*(t(w_semi-w_sup) %*% (t(Xe) %*% Xe + nrow(Xe)*lambda1*diag(c(0,rep(1,(m-1)))))  %*% (w_semi-w_sup))) 
              }
              else { 
                return((1/nrow(Xe))*(t(w_semi-w_sup) %*% t(Xe) %*% Xe %*% (w_semi-w_sup)))   
              }
            }
      
            O1 <- 2/nrow(Xe) * G %*% t(X) %*% y
            O2 <- 2/nrow(Xe) * G %*% t(X_u)
            O3 <- 2/nrow(Xe) * X_u %*% w_sup
            
            if (lambda2>0) {
              O4<- 2 * lambda2 * X_u %*% C %*% diag(c(0,rep(1,(m-1)))) %*% t(F) %*% y
              O5<- 2 * lambda2 * X_u %*% C %*% diag(c(0,rep(1,(m-1)))) %*% C %*% t(X_u)
            }
            
            opt_grad_projection <- function(theta) {
              theta<-matrix(theta)
              reg3<-lambda_prior*2*(rep(sum(theta),nrow(theta))-rep(nrow(theta)*mean_y,nrow(theta)))
              
              # Check gradient
              # browser()
              #cbind(matrix(grad(opt_func_projection,theta),ncol=1), O1 + O2 %*% theta - O3)
              
              if (lambda2>0) return(O1 + O2 %*% theta - O3 + O4 + O5 %*% theta + reg3)
              else return(O1 + O2 %*% theta - O3 + reg3  ) 
            }
      
      theta <- rep(0.5,nrow(X_u))
      # Bounded optimization
      opt_result <- optim(theta, opt_func_projection, gr=opt_grad_projection, method="L-BFGS-B", lower=0.0, upper=1.0, control=list(fnscale=1))
      theta<-opt_result$par
      
      unlabels<-theta
    } 
    else if (projection=="euclidean") {
      w_sup<- matrix(inv(t(X)%*%X) %*% t(X) %*% y,ncol=1)
      
      opt_func <- function(theta) {
        theta<-matrix(theta)
        theta <- C %*% t(Xe) %*% rbind(matrix(y),theta)
        sum((theta-w_sup)^2)
      }
      
      O1 <- 2 * t(y) %*% X %*% C %*%  C %*% t(X_u)
      O2 <- 2 * X_u %*% C %*%  C %*% t(X_u)
      O3 <- 2 * t(w_sup) %*% C %*% t(X_u)
      
      opt_grad <- function(theta) {
        theta<-matrix(theta)
        # For debugging purposes:
        # grad(opt_func,theta)
        # O1 + t(t(O2) %*% theta) - O3
        return(O1 + t(t(O2) %*% theta) - O3 ) 
      }
      
      theta <- rep(0.5,nrow(X_u))
      # Bounded optimization
      opt_result <- optim(theta, opt_func, gr=opt_grad, method="L-BFGS-B", lower=0.0, upper=1.0, control=list(fnscale=1))
      theta<-opt_result$par
      
      unlabels<-theta
    } else {
      stop("There is no LBFGS implementation of this projection.")
    }
  } else if (method=="exhaustive") {
    if (nrow(X_u)!=2 | ncol(X_u)!=3) {
      warning("Too many objects to do exhausitively. We are going to sample.")
      n_u<-nrow(X_u)
      searchgrid <- matrix(rbeta(10000*n_u,shape1=0.5,shape2=0.5),10000,n_u)
      #searchgrid <- 1+matrix(runif(100000*n_u),100000,n_u)
    } else {
      searchgrid<-expand.grid(replicate(2,seq(0,1,0.1),simplify=FALSE))
    }
    
    m<-ncol(Xe)
    
    C <- inv(t(Xe) %*% Xe + nrow(Xe)*lambda1*diag(c(0,rep(1,(m-1))))) 
    F<- Xe %*% C 
    G <- X_u %*% t(F) %*% F
    w_sup<- matrix(inv(t(X)%*%X) %*% t(X) %*% y,ncol=1)
    
    opt_func_projection <- function(theta) {
      # Return a Mahanalobis type distance between the semi-supervised and supervised coefficients
      
      theta<-matrix(theta)
      w_semi <- matrix(C %*% t(Xe) %*% rbind(matrix(y),theta),ncol=1)
      if (lambda2>0) { 
        stop("Not implemented") #todo
      }
      else { 
        return((1/nrow(Xe))*(t(w_semi-w_sup) %*% t(Xe) %*% Xe %*% (w_semi-w_sup)))   
      }
    }
    
    ws<-matrix(NA,nrow(searchgrid), ncol(X))
    objectiveval<-rep(NA,nrow(searchgrid))
    for (i in 1:nrow(searchgrid)) {
      theta<-as.numeric(searchgrid[i,])
      theta<-matrix(theta)
      ws[i,] <- matrix(C %*% t(Xe) %*% rbind(matrix(y),theta),ncol=1)
      objectiveval[i]<-opt_func_projection(theta) # Save objective values
      # Save
    }
    return(list(ws=ws,sg=searchgrid,w_sup=w_sup,objectiveval=objectiveval))
    
  }  else {
    stop("Optimization method is unknown.")
  }
  unlabels <- matrix(unlabels,nrow(X_u))
  theta <- inv(t(Xe) %*% Xe + nrow(Xe)*lambda1*diag(c(0,rep(1,(m-1)))) ) %*% (t(Xe) %*% rbind(y,unlabels))
  
  new("ICLeastSquaresClassifier",
      classnames=classnames,
      modelform=modelform,
      theta=theta,
      unlabels=unlabels,
      scaling=scaling,
      intercept=intercept,
      optimization=opt_result,
      y_scale=y_scale
      )
}


#LowRankQP(Dmat,dvec,Amat,bvec,uvec,method="PFCF",verbose=FALSE,niter=200)@alpha

#       O1 <-  G %*% t(X) %*% y
#       O2 <-  G %*% t(X_u)
#       O3 <-  X_u %*% t(F) %*% y
#       
#       opt_grad <- function(theta) {
#         theta<-matrix(theta)
#         browser()
#         cbind(grad(function(theta) { 0.5 * (t(theta) %*% Dmat %*% theta) - t(theta) %*% dvec}, (theta)), O1 + O2 %*% theta - O3,theta)
#         return(O1 + O2 %*% theta - O3) 
#       }
#       
#       sol<-optim(rep(0,nrow(X_u)), fn=function(theta) { 0.5 * theta %*% Dmat %*% theta - theta %*% dvec}, gr=opt_grad, method="L-BFGS-B", lower=0, upper=1, control=list(fnscale=1))
#       sol<-optim(rep(0,nrow(X_u)), fn=opt_func, gr=opt_grad, method="L-BFGS-B", lower=0, upper=1, control=list(fnscale=1))
