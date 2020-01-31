#' @include LinearDiscriminantClassifier.R
setClass("MCPLDA",
         representation(responsibilities="matrix"),
         prototype(name="Maximum Contrastive Pessimistic Linear Discriminant Classifier"),
         contains="LinearDiscriminantClassifier")

#' Maximum Contrastive Pessimistic Likelihood Estimation for Linear Discriminant Analysis
#' 
#'  Maximum Contrastive Pessimistic Likelihood (MCPL) estimation (Loog 2016) attempts to find a semi-supervised solution that has a higher likelihood compared to the supervised solution on the labeled and unlabeled data even for the worst possible labeling of the data. This is done by attempting to find a saddle point of the maximin problem, where the max is over the parameters of the semi-supervised solution and the min is over the labeling, while the objective is the difference in likelihood between the semi-supervised and the supervised solution measured on the labeled and unlabeled data. The implementation is a translation of the Matlab code of Loog (2016).
#'  
#' @references Loog, M., 2016. Contrastive Pessimistic Likelihood Estimation for Semi-Supervised Classification. IEEE Transactions on Pattern Analysis and Machine Intelligence, 38(3), pp.462-475.
#' 
#' @family RSSL classifiers
#' 
#' @param max_iter integer; Maximum number of iterations
#' @inheritParams BaseClassifier
#' 
#' @export
MCPLDA <- function(X, y, X_u, x_center=FALSE, scale=FALSE, max_iter=1000) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,x_center=x_center,scale=scale,intercept=FALSE)
  X <- ModelVariables$X
  X_u <- ModelVariables$X_u
  y <- ModelVariables$y
  Y <- ModelVariables$Y
  
  res <- minimaxlda(X,Y,X_u,max_iter)
  
  new("MCPLDA", modelform=ModelVariables$modelform, 
      means=res$m, prior=as.matrix(res$p), 
      sigma=lapply(1:ncol(Y),function(x) {solve(res$iW)}),
      classnames=ModelVariables$classnames,scaling=ModelVariables$scaling,
      responsibilities=res$uw)
}


#' Taking the square root of a matrix using the singular value decomposition
#' @param X matrix; square input matrix
#' @return Y matrix; square root of the input matrix
svdsqrtm <- function(X) {
  res <- svd(X)
  s <- res$d
  Y = res$u %*% diag(sqrt(s)) %*% t(res$v)
  return(Y)
}

#' Taking the inverse of the square root of the matrix using the singular value decomposition
#' @param X matrix; square input matrix
#' @return Y matrix; inverse of the square root of the input matrix
svdinvsqrtm <- function(X) {
  res <- svd(X)
  s <- res$d
  s[s>0] <- 1/sqrt(s[s>0])
  Y <- res$u %*% diag(s) %*% t(res$v)
  return(Y)
}


#' Inverse of a matrix using the singular value decomposition
#' @param X matrix; square input matrix
#' @return Y matrix; inverse of the input matrix
svdinv <- function(X) {
  res <- svd(X)
  s <- res$d
  s[s>0] <- 1/s[s>0]
  Y <- res$v %*% diag(s) %*% t(res$u)
  #warning("Should this not be the transpose of this?")
  return(Y)
}

svdeig <- function(A,B) {
  T <- svdinvsqrtm(B);
  res <- svd(t(T) %*% A %*% T)
  E <- T %*% res$u
  return(list(E=E,D=diag(res$d)))
}

#' Measures the expected log-likelihood of the LDA model defined by m, p,
#' and iW on the data set a, where weights w are potentially taken into
#' account
#' 
#' @param m means
#' @param p class prior
#' @param iW is the inverse of the within covariance matrix
#' @param a design matrix
#' @param w weights
#' @return Average log likelihood
wlda_loglik <- function(m,p,iW,a,w) {
  
  # renormalize the weights [just to be sure?]
  sumw <- rowSums(w)
  w <- sweep(w,1,sumw,"/")
  
  # some preps
  K <- ncol(w) # number of classes
  N <- nrow(a) # number of samples
  D <- ncol(a) # number of dimensions
  #%detiW = det(iW); % not used
  
  S <- svd(iW)$d
  logdetiW <- sum(log(S))
  
  
  sqrtiW <- svdsqrtm(iW);
  C <- (-D*log(2*pi)+logdetiW)/2;
  LL <- 0
  
  for (i in 1:K) {
    #X <- sweep(a,2,-m[i,,drop=FALSE],"+") %*% sqrtiW #X = bsxfun(@plus,a,-m(i,:))*sqrtiW
    X <- rowwise_addition(a,-m[i,,drop=FALSE]) %*% sqrtiW
    ll <- C - rowSums(X^2)/2 + log(p[i])
    LL <- LL + colSums(ll*w[,i,drop=FALSE])
  }
  
  LL <- LL/N
  
  return(LL)
}  


#' Measures the expected error of the LDA model defined by m, p,
#' and iW on the data set a, where weights w are potentially taken into
#' account
#' 
#' @inheritParams wlda_loglik
wlda_error <- function(m,p,iW,a,w) {
  
  # renormalize the weights [just to be sure?]
  sumw <- rowSums(w)
  w <- sweep(w,1,sumw,"/")
  
  # some preps
  K <- ncol(w) # number of classes
  N <- nrow(a) # number of samples
  D <- ncol(a) # number of dimensions
  #%detiW = det(iW); % not used
  
  S <- svd(iW)$d
  logdetiW <- sum(log(S))
  
  sqrtiW <- svdsqrtm(iW);
  C <- (-D*log(2*pi)+logdetiW)/2;
  LL <- matrix(NA,N,K)
  
  for (i in seq_len(K)) {
    X <- sweep(a,2,-m[i,,drop=FALSE],"+") %*% sqrtiW #X = bsxfun(@plus,a,-m(i,:))*sqrtiW
    LL[,i] <- C - rowSums(X^2)/2 + log(p[i])
  }
  
  # find the maximum...
  IDX <- which_rowMax(LL)
  LL <- matrix(0,ncol(LL),nrow(LL))
  LL[as.numeric(IDX)+(K*(0:(N-1)))] <- 1
  EE <- 1-sum(t(LL)*w)/N #1-sum(sum(LL'.*w))/N
  return(EE)
}


#' Implements weighted likelihood estimation for LDA
#' @param a is the data set
#' @param w is an indicator matrix for the K classes or, potentially, a weight matrix in which the fraction with which a sample belongs to a particular class is indicated
#' @return m contains the means, p contains the class priors, iW contains the INVERTED within covariance matrix
wlda <- function(a,w) {
  
  # renormalize the weights [just to be sure?]
  sumw <- rowSums(w)
  w <- sweep(w,1,sumw,"/")
  
  # priors
  sumw <- colSums(w)
  p <- sumw/sum(sumw) # get the priors of different classes
  
  # weighted means and within covariance matrix
  K <- ncol(w) # number of classes
  N <- nrow(a) # number of samples
  m <- matrix(NA,K,ncol(a))
  W <- matrix(0,ncol(a),ncol(a))
  
  for (i in seq_len(K)) {
    m[i,] <- colSums(sweep(a,1,w[,i,drop=FALSE],"*"))/sumw[i] #bsxfun(@times,a,w(:,i))
   
    tempa <- sweep(a,2,-m[i,,drop=FALSE],"+")
    wtempa <- sweep(tempa,1,w[,i,drop=FALSE],"*")
    #(1/sumw(i))*(t(tempa)*wtempa)
    W <- W + t(tempa) %*% wtempa
  }
  
  iW <- W + t(W)
  W <- iW/(2*N)
  iW <- svdinv(W)
  return(list(m=m,p=p,iW=iW))
}

#' project an n-dim vector y to the simplex Dn
#' 
#' Dn = { x : x n-dim, 1 >= x >= 0, sum(x) = 1}
#' R translation of Loog's version of Xiaojing Ye's initial implementation.
#' The algorithm works row-wise
#'
#' @references Algorithm is explained as in http://arxiv.org/abs/1101.6081
#' @param y matrix with vectors to be projected onto the simplex
#' @return projection of y onto the simplex    
projection_simplex <- function(y) {
  N <- nrow(y)
  m <- ncol(y)

  S <- sort_matrix(y)
  #Sold <- t(apply(y,1,sort,decreasing=TRUE))
  #if(!all(S==Sold)) warning("Check failed!")
  CS <- t(apply(S,1,cumsum))
  TMAX <- sweep(CS-1,2,1:m,"/")
  Bget <- TMAX[,-m,drop=FALSE] < S[,2:m,drop=FALSE]
  I <- matrix(rowSums(Bget),ncol=1)+1
  TMAX <- t(TMAX)
  x <- sweep(y,1,-TMAX[I+(m*(0:(N-1)))],"+")
  x[x<0] <-0
  return(x)
}

#' Implements weighted likelihood estimation for LDA
#' @param a is the data set
#' @param w is an indicator matrix for the K classes of a or, potentially, a weight matrix in which the fraction with which a sample belongs to a particular class is indicated
#' @param u is a bunch of unlabeled data
#' @param iter decides on the amount of time we spend on minimaxing the stuff
#' @return m contains the means, p contains the class priors, iW contains the INVERTED within covariance matrix, uw returns the weights for the unlabeled data, i returns the number of iterations used
minimaxlda <- function(a,w,u,iter) {
  
  alpha <- 1e-3
  # means etc. of regular, supervised version
  res <- wlda(a,w)
  msup <- res$m
  psup <- res$p
  iWsup <- res$iW

  #init weights for unlabeled data
  #uw = ones(size(u,1),1)*psup; % somewhat conservative choice?  stupid?
  #uw = rand(size(w));
  #uw = bsxfun(@rdivide,uw,sum(uw,2));
  #uw = w;
   
  K <- ncol(w) # number of classes
  D <- ncol(a) # number of dimensions
  #%Nu = size(u,1); % number of unlabeled samples
  #detiWsup = det(iWsup);
  S <- svd(iWsup)$d
  logdetiWsup <- sum(log(S)) # ln(covariance matrix)
  sqrtiWsup <- svdsqrtm(iWsup) # sqrt of inverse of covariance matrix
  Csup <- (-D*log(2*pi)+logdetiWsup)/2 # -1/2*ln(covariance matrix)-1/2*ln(2*pi)
  
  # make supervised part of the gradient on the weights
  LLGradsup <- matrix(0,nrow(u),K)
   
  for (k in seq_len(K)) {
    
    #X <- sweep(u,2,-msup[k,,drop=FALSE],"+") %*% sqrtiWsup #X = bsxfun(@plus,u,-msup(k,:))*sqrtiWsup;
    X <- rowwise_addition(u,-msup[k,,drop=FALSE]) %*% sqrtiWsup
    LLGradsup[,k] <- Csup - rowSums(X^2)/2 + log(psup[k]) # g1(x) and g2(x)
  }
  
  # unlabeled weights init
  uw <- LLGradsup
  uw[uw < -1e3] <- -1e3
  uwtmp <- projection_simplex(uw)
  uw <- projection_simplex(uwtmp);
  
  # the main loop in which the weights are minimaxed
  LLGradtra <- matrix(0,nrow(LLGradsup),ncol=ncol(LLGradsup))
  A <- rbind(a,u)
  rm(a) # could matter for big data  :-)
  obj <- matrix(NA,iter,1)
  
  for (i in seq_len(iter)) {
    
    # update means etc. of semi-supervised version
    res <- wlda(A,rbind(w,uw))
    mtra <- res$m
    ptra <- res$p
    iWtra <- res$iW
    
    obj[i] <- wlda_loglik(mtra,ptra,iWtra,A,rbind(w,uw)) - wlda_loglik(msup,psup,iWsup,A,rbind(w,uw))
    #cat(i,": ",obj[i],"\n")
    
    #obj(i)
    #if (i > 1) { abs(obj[i-1]-obj[i]) }
    
    if ((i > 1) && (abs(obj[i-1]-obj[i]) < 1e-6)) { break }
    
    #detiWtra = det(iWtra); % not used...    
    S <- svd(iWtra)$d
    logdetiWtra <- sum(log(S))
    
    sqrtiWtra <- svdsqrtm(iWtra)
    Ctra <- (-D*log(2*pi)+logdetiWtra)/2
    
    for (k in seq_len(K)) {
      #X <- sweep(u,2,-mtra[k,,drop=FALSE],"+") %*% sqrtiWtra #X = bsxfun(@plus,u,-msup(k,:))*sqrtiWsup;
      X <- rowwise_addition(u,-mtra[k,,drop=FALSE]) %*% sqrtiWtra
      LLGradtra[,k] <- Ctra - rowSums(X^2)/2 + log(ptra[k]) -  LLGradsup[,k,drop=FALSE]
    }
    
    #uwold = uw;
    
    grad <- LLGradtra;
    uwtmp <- uw - alpha*grad/i
    uw <- uwtmp
    uw[uw < -1e3] <- -1e3
    uwtmp <- projection_simplex(uw)
    uw <- projection_simplex(uwtmp) # project twice to be sure??
    
    #some plots for some "insight" 
    #figure(1)
    #subplot(3,1,1), plot(uw,'.'), axis tight
    #subplot(3,1,2), semilogx(obj), axis tight
    #subplot(3,1,3), plot(grad,'.'), axis tight
  }
  return(list(m=mtra,p=ptra,iW=iWtra,uw=uw,i=i))
}
