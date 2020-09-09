#' @include Classifier.R
setClass("WellSVM", 
         representation(alpha="matrix",y_set="matrix",
                        Xtrain="ANY",gamma="numeric"), 
         prototype(name="WellSVM"), 
         contains="Classifier")

#' WellSVM for Semi-superivsed Learning
#' 
#' WellSVM is a minimax relaxation of the mixed integer programming problem of finding the optimal labels for the unlabeled data in the SVM objective function. This implementation is a translation of the Matlab implementation of Li (2013) into R.
#' 
#' @references Y.-F. Li, I. W. Tsang, J. T. Kwok, and Z.-H. Zhou. Scalable and Convex Weakly Labeled SVMs. Journal of Machine Learning Research, 2013.
#' @references R.-E. Fan, P.-H. Chen, and C.-J. Lin. Working set selection using second order information for training SVM. Journal of Machine Learning Research 6, 1889-1918, 2005.
#' 
#' @family RSSL classifiers

#' @param C1 double; A regularization parameter for labeled data, default 1;
#' @param C2 double; A regularization parameter for unlabeled data, default 0.1;
#' @param gamma double; Gaussian kernel parameter, i.e., k(x,y) = exp(-gamma^2||x-y||^2/avg) where avg is the average distance among instances; when gamma = 0, linear kernel is used. default gamma = 1;
#' @param max_iter integer; Maximum number of iterations
#' @inheritParams BaseClassifier
#'
#' @example inst/examples/example-WellSVM.R
#' @export
WellSVM <- function(X,y,X_u,C1=1,C2=0.1,gamma=1,x_center=TRUE,scale=FALSE,use_Xu_for_scaling=FALSE,max_iter=20) {
  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=FALSE,x_center=x_center,use_Xu_for_scaling=use_Xu_for_scaling)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  y <- 2*ModelVariables$Y[,1,drop=FALSE]-1
  
  
  x <- t(rbind(X,X_u))
  y <- rbind(y,matrix(0,nrow=nrow(X_u)))
  
  opt <- list()
  
  opt$C <- C1
  opt$C2 <- C2
  opt$maxiter <- max_iter  # number of maximal iterations
  opt$gamma <- gamma
  # for balance constraint
  prior_ratio <- length(which(y == 1))/length(which(y != 0)) # the ratio of positive examples in labeled examples
  opt$lr <- prior_ratio # set the expected positive label ratio for unlabeled to the same of prior_ratio
  
  # for kernel paramter
  if(opt$gamma > 0) {  # when gaussian kernel is used;
    opt$gaussian <- 1
  } else {                # when linear kernel is used
    opt$gaussian <- 0
  }
  
  # calculate kernel matrix
  d <- nrow(x)
  n <- ncol(x)
  if (opt$gaussian == 1) {
    distance_matrix <- matrix(rep(t(rowSums(t(x)*t(x))),n),n,n,byrow=TRUE) + matrix(rep(rowSums(t(x)*t(x)),n),n,n,byrow=FALSE) - 2 * t(x) %*% x
    avg <- sum(distance_matrix)/n^2
    K0 <- gaussian_kernel(x, opt$gamma^2/avg)$k
    opt$gamma = opt$gamma^2/avg
  } else {
    K0 = t(x) %*% x  
  }
  
  lind <- which(y != 0) # index for labeled data;
  uind <- which(y == 0) # index for unlabeled data;
  ln <- length(lind)   # number of labeled data;
  un <- length(uind)   # number of unlabeled data;
  ind_y <- matrix(0,ln+un,1); 
  ind_y[lind] <- 1 # an indicator vector for label and unlabel data
  
  # Train inductive SVM with labeled examples
  Ktmp <- K0[lind,lind,drop=FALSE]
  #Ktmp <- cbind(1:ln,Ktmp)   # precomputed kernel for training examples
  ytmp <- y[lind]          # label vector for training examples
  flag <- 0
  if (ytmp[1] == -1) {
    flag <- 1;
  } 
  
  model <- svmd(as.kernelMatrix(Ktmp),ytmp,cost=opt$C,type="C-classification")
  Ktmp <- K0[uind,lind,drop=FALSE]
  
  decision_values <- attr(predict.svmd(model,as.kernelMatrix(Ktmp),decision.values = TRUE),"decision.values")
  
  if (flag) { 
    decision_values <- -decision_values
  }
  rm(flag)
  y1 <- threshold(decision_values,opt) # refine the prediciton result via balance constraint
  
  # obtain the inverse objective value of SVM prediction
  ytmp <- c(y[lind],y1) 
  res <- WellSVM_supervised(K0,ytmp,opt,ind_y)
  alpha0 <- res$alpha
  y_set0 <- res$y_set
  beta0 <- res$beta
  obj1 <- res$obj
  
  
  # convex relaxation via label generation by setting initialization as SVM prediction 
  res <- WellSVM_SSL(K0,y,opt,ytmp)
  alpha <- res$alpha
  y_set <- res$y_set
  beta <- res$beta 
  
  yy1 <- prediction_func(alpha,y_set,beta,x,x[,uind,drop=FALSE],opt)
  yy1 <- threshold(yy1,opt) # refine the prediction results via balance constraint
  
  
  # obtain the inverse objective value of convex relaxed solution
  ytmp <- c(y[lind], yy1)
  res <- WellSVM_supervised(K0,ytmp,opt,ind_y)
  alpha1 <- res$alpha
  y_set1 <- res$y_set
  beta1 <- res$beta
  obj2 <- res$obj
  
  # select the prediction with better objective value
  val <- max(c(obj1,obj2))
  inx <- which.max(c(obj1,obj2))
  
  if (inx==1) {
    alpha <- alpha0
    y_init = c(y[lind],y1)
  } else if (inx==2) {
    alpha <- alpha1;
    y_init <- c(y[lind],yy1)
  }
  
  # re-optimization w.r.t. S3VM objective function
  flag <- 1
  pred_y_set <- prediction_func(alpha,t(y_init),1,x,x[,uind,drop=FALSE],opt)
  tmp <- threshold(pred_y_set[,1,drop=FALSE],opt)
  newy <- c(y[lind],tmp)
  iter <- 1
  while (flag && iter <= 10) {
    if (norm(newy-y_init,type="2") == 0) {
      flag <- 0
    } else {
      y_init <- newy
      alpha = WellSVM_supervised(K0,y_init,opt,ind_y)$alpha
      pred_y_set = prediction_func(alpha,t(y_init),1,x,x[,uind,drop=FALSE],opt)
      tmp <- threshold(pred_y_set[,1,drop=FALSE],opt)
      newy <- c(y[lind],tmp)
      iter <- iter + 1;
    }
  }
  
  new("WellSVM",
      alpha=alpha,
      y_set=t(newy),
      Xtrain=x,
      gamma=opt$gamma,
      modelform=ModelVariables$modelform,
      classnames=ModelVariables$classnames,
      scaling=scaling)
}

#' @rdname rssl-predict
#' @aliases predict,WellSVM-method
setMethod("predict", signature(object="WellSVM"), function(object, newdata,...) {
  ModelVariables <- PreProcessingPredict(object@modelform,newdata,scaling=object@scaling,intercept=FALSE,classnames=object@classnames)
  X <- ModelVariables$X
  
  if (object@gamma == 0) {
    K0 = t(object@Xtrain) %*% t(X)
  } else{
    K0 = gaussian_kernel(object@Xtrain,object@gamma,t(X))$k
  }

  y1 <- (t(object@alpha)*(1 %*% object@y_set[1,,drop=FALSE]) ) %*% K0
  return(factor(y1<0,levels=c(FALSE,TRUE),labels=object@classnames))
})

#' @rdname rssl-predict
#' @aliases decisionvalues,WellSVM-method
setMethod("decisionvalues", signature(object="WellSVM"), function(object, newdata) {
  ModelVariables <- PreProcessingPredict(object@modelform,newdata,scaling=object@scaling,intercept=FALSE,classnames=object@classnames)
  X <- ModelVariables$X
  
  if (object@gamma == 0) {
    K0 = t(object@Xtrain) %*% t(X)
  } else{
    K0 = gaussian_kernel(object@Xtrain,object@gamma,t(X))$k
  }
  
  y1 <- (t(object@alpha)*(1 %*% object@y_set[1,,drop=FALSE]) ) %*% K0
  return(as.numeric(y1))
})


#' wellsvm implements the wellsvm algorithm as shown in [1].
#' @param x A Nxd training data matrix, where N is the number of training instances and d is the dimension of instance; 
#' @param y A Nx1 training label vector, where y = 1/-1 means positive/negative, and y = 0 means unlabeled;
#' @param testx A Mxd testing data matrix, where M is the number of testing instances;
#' @param testy A Mx1 testing label vector
#' @param C1 A regularization parameter for labeled data, default 1;
#' @param C2 A regularization parameter for unlabeled data, default 0.1;
#' @param gamma Gaussian kernel parameter, i.e., k(x,y) = exp(-gamma^2||x-y||^2/avg) where avg is the average distance among instances; when gamma = 0, linear kernel is used. default gamma = 1;
#' @return prediction   - A Mx1 predicted testing label vector; accuracy     - The accuracy of prediction; cputime     - cpu running time;
#' @references Y.-F. Li, I. W. Tsang, J. T. Kwok, and Z.-H. Zhou. Scalable and Convex Weakly Labeled SVMs. Journal of Machine Learning Research, 2013.
#' @references R.-E. Fan, P.-H. Chen, and C.-J. Lin. Working set selection using second order information for training SVM. Journal of Machine Learning Research 6, 1889-1918, 2005.
wellsvm_direct <- function(x,y,testx,testy,C1=1,C2=0.1,gamma=1) {

  # TODO: y to -1,1
  
  start_time <- Sys.time()
  opt <- list()
  
  opt <- list()
  
  opt$C <- C1
  opt$C2 <- C2
  opt$maxiter <- 20  # number of maximal iterations
  opt$gamma <- gamma
  # for balance constraint
  prior_ratio <- length(which(y == 1))/length(which(y != 0)) # the ratio of positive examples in labeled examples
  opt$lr <- prior_ratio # set the expected positive label ratio for unlabeled to the same of prior_ratio
  
  # for kernel paramter
  if(opt$gamma > 0) {  # when gaussian kernel is used;
    opt$gaussian <- 1
  } else {                # when linear kernel is used
    opt$gaussian <- 0
  }
  # call main function
  prediction <- main_function(t(x),y,t(testx),opt)
  
  # calculate the accuracy
  m <- length(which(prediction == testy))
  accuracy <- m/length(testy);
  
  # record the cputime
  cpu_time = Sys.time() - start_time
  
  return(list(prediction=prediction,accuracy=accuracy,cpu_time=cpu_time))
}


main_function <- function(x,y,testx,opt) {
  
  # calculate kernel matrix
  d <- nrow(x)
  n <- ncol(x)
  if (opt$gaussian == 1) {
    distance_matrix <- matrix(rep(t(rowSums(t(x)*t(x))),n),n,n,byrow=TRUE) + matrix(rep(rowSums(t(x)*t(x)),n),n,n,byrow=FALSE) - 2 * t(x) %*% x
    avg <- sum(distance_matrix)/n^2
    K0 <- gaussian_kernel(x, opt$gamma^2/avg)$k
    opt$gamma = opt$gamma^2/avg
  } else {
    K0 = t(x) %*% x  
  }
            
  lind <- which(y != 0) # index for labeled data;
  uind <- which(y == 0) # index for unlabeled data;
  ln <- length(lind)   # number of labeled data;
  un <- length(uind)   # number of unlabeled data;
  ind_y <- matrix(0,ln+un,1); 
  ind_y[lind] <- 1 # an indicator vector for label and unlabel data
            
  # Train inductive SVM with labeled examples
  Ktmp <- K0[lind,lind,drop=FALSE]
  #Ktmp <- cbind(1:ln,Ktmp)   # precomputed kernel for training examples
  ytmp <- y[lind]          # label vector for training examples
  flag <- 0
  if (ytmp[1] == -1) {
    flag <- 1;
  } 
  
  
  #model <- svm(t(x)[lind,,drop=FALSE],ytmp,cost=opt$C,kernel="radial",gamma=opt$gamma,type="C-classification",scale=FALSE)
  #model2 <- ksvm(Ktmp, ytmp, C=opt$C,type="C-svc",kernel="matrix")
  #Ktmp[,model$index] %*% model$coefs - model$rho
  #Ktmp[,model2@alphaindex[[1]]] %*% model2@coef[[1]] - model2@b
  #cbind(attr(predict(model,t(x)[uind,,drop=FALSE],decision.values=TRUE),"decision.values"), predict(model2,as.kernelMatrix(Ktmp),type="decision"))
  # decision_values <- attr(predict(model,t(x)[uind,,drop=FALSE],decision.values=TRUE),"decision.values") # inductive SVM prediction
  
  model <- svmd(as.kernelMatrix(Ktmp),ytmp,cost=opt$C,type="C-classification")
  Ktmp <- K0[uind,lind,drop=FALSE]
  
  decision_values <- attr(predict.svmd(model,as.kernelMatrix(Ktmp),decision.values = TRUE),"decision.values")
  
  if (flag) { 
    decision_values <- -decision_values
  }
  rm(flag)
  y1 <- threshold(decision_values,opt) # refine the prediciton result via balance constraint
  
  # obtain the inverse objective value of SVM prediction
  ytmp <- c(y[lind],y1) 
  res <- WellSVM_supervised(K0,ytmp,opt,ind_y)
  alpha0 <- res$alpha
  y_set0 <- res$y_set
  beta0 <- res$beta
  obj1 <- res$obj
  
  
  # convex relaxation via label generation by setting initialization as SVM prediction 
  res <- WellSVM_SSL(K0,y,opt,ytmp)
  alpha <- res$alpha
  y_set <- res$y_set
  beta <- res$beta 
  
  yy1 <- prediction_func(alpha,y_set,beta,x,x[,uind,drop=FALSE],opt)
  yy1 <- threshold(yy1,opt) # refine the prediction results via balance constraint
  
  
  # obtain the inverse objective value of convex relaxed solution
  ytmp <- c(y[lind], yy1)
  res <- WellSVM_supervised(K0,ytmp,opt,ind_y)
  alpha1 <- res$alpha
  y_set1 <- res$y_set
  beta1 <- res$beta
  obj2 <- res$obj
  
  # select the prediction with better objective value
  val <- max(c(obj1,obj2))
  inx <- which.max(c(obj1,obj2))
  
  if (inx==1) {
    alpha <- alpha0
    y_init = c(y[lind],y1)
  } else if (inx==2) {
    alpha <- alpha1;
    y_init <- c(y[lind],yy1)
  }
  
  # re-optimization w.r.t. S3VM objective function
  flag <- 1
  pred_y_set <- prediction_func(alpha,t(y_init),1,x,x[,uind,drop=FALSE],opt)
  tmp <- threshold(pred_y_set[,1,drop=FALSE],opt)
  newy <- c(y[lind],tmp)
  iter <- 1
  while (flag && iter <= 10) {
    if (norm(newy-y_init,type="2") == 0) {
        flag <- 0
    } else {
      y_init <- newy
      alpha = WellSVM_supervised(K0,y_init,opt,ind_y)$alpha
      pred_y_set = prediction_func(alpha,t(y_init),1,x,x[,uind,drop=FALSE],opt)
      tmp <- threshold(pred_y_set[,1,drop=FALSE],opt)
      newy <- c(y[lind],tmp)
      iter <- iter + 1;
    }
  }
                               
  # prediction
  prediction <- prediction_func(alpha,t(newy),1,x,testx,opt)
  prediction <- sign(prediction)
}

#' Refine the prediction to satisfy the balance constraint
#' @param y1 predictions
#' @param options options passed
#' @return y2
threshold <- function(y1,options) {
  
  lr <- options$lr  
  unlab_s <- length(y1)
  lr <- ceiling(unlab_s*lr)
  y2 <- -matrix(1,length(y1),1)
  res <- sort(y1,decreasing=TRUE,index.return=TRUE)
  val <- res$x
  inx <- res$ix
  y2[inx[1:lr]] <- 1
  return(y2)
}

#' calculated the gaussian kernel matrix
#' @param x  A d x n training data matrix
#' @param gamma  kernel parameter
#' @param x_test A d x m testing data matrix
#' @return k - A n x m kernel matrix and dis_mat - A n x m distance matrix
gaussian_kernel <- function(x,gamma,x_test=NULL) {
  
  d <- nrow(x)
  n <- ncol(x)
  
  if (is.null(x_test)) {
    dis_mat <- matrix(rep(t(rowSums(t(x)*t(x))),n),n,n,byrow=TRUE) + matrix(rep(rowSums(t(x)*t(x)),n),n,n,byrow=FALSE) - 2 * t(x) %*% x
      } else {
   m <- ncol(x_test);
   dis_mat <- matrix(rep(t(rowSums(t(x_test)*t(x_test))),n),n,m,byrow=TRUE) + matrix(rep(rowSums(t(x)*t(x)),m),n,m,byrow=FALSE) - 2 * t(x) %*% x_test
  }
  k = exp(-dis_mat*gamma)
  return(list(k=k,dis_mat=dis_mat))
}

#' A degenerated version of WellSVM where the labels are complete, that is, supervised learning
#' @inheritParams WellSVM_SSL
#' @param ind_y Labeled/Unlabeled indicator
WellSVM_supervised <- function(K0,y,opt,ind_y) {
       
  C <- opt$C
  C2 <- opt$C2
  y_set <- t(y)
  cur_beta <- 1
  K1 <- y %*% t(y)
  K2 <- c()
       
  res <- MKL(K0,K1,K2,y_set,C,C2,cur_beta, ind_y);
  return(list(alpha=res$alpha,y_set=y_set,beta=res$beta,obj=res$obj))       
}
        
#' Convex relaxation of S3VM by label generation
#' @param K0 kernel matrix
#' @param y labels
#' @param opt options
#' @param yinit label initialization (not used)
WellSVM_SSL <- function(K0,y,opt,yinit=NULL) {
  
  C <- opt$C
  C2 <- opt$C2
  lr <- opt$lr
  maxiter <- opt$maxiter
  n <- nrow(K0)
  ind_y <- matrix(0,n,1)
  ind_y[y != 0] <- 1
  
  # initilize the working set, denoted by y_set
  tmpalpha <- matrix(1,n,1)
  uy <- which(ind_y == 0)
  
  #if (!is.null(yinit)) {   
  if (FALSE) {  
    # when label assignment is initilized, use the initilized one
    y1 <- yinit
  } else {             
    # otherwise generate a candidate label assignment w.r.t. kernel alignment
    l_uy <- length(uy)
    times <- 20
    
    for (i in 1:times) {
      r <- lr    
      nr <- floor(l_uy*r)          
      rp <- sample(1:l_uy)   # generate a random permutation
      tmpy <- y
      tmpy[uy[rp[1:nr]]] = -y[uy[rp[1:nr]]]
      res = find_a_violated_label(tmpalpha,K0,y,ind_y,lr,tmpy)     
      tmpy1 <- res$y
      tmpobj <- res$obj
      
      # record the label assignment with the best kernel alignment value
      if (i == 1) {
        y1 <- tmpy1
        obj <- tmpobj
      } else if (tmpobj > obj) {
        y1 <- tmpy1
        obj <- tmpobj
      }     
    }
  }
  
  y1 <- t(y1)
  y_set <- y1
  nk <- 1  # the number of generated label assigments 

  # optimization
  bestobj <- -Inf
  obj_set <- c()
  iter <- 1
  cur_beta <- 1
  K1 <- t(y1) %*% y1
  K2 <- c()
  flag <- 1
  while (flag && iter <= maxiter) {
    
    # Multiple label-kernel learning
    res <- MKL(K0,K1,K2,y_set,C,C2,cur_beta,ind_y)
    cur_beta <- res$beta 
    w <- res$alpha
    b <- res$b
    obj <- res$obj
    
    # if the objective decrease is less than a threshold, stop
    if ( obj < 1.001*bestobj ) {
      flag <- 0 
      break
    }

    if (nk == 1) {   # for the first iteration
      beta <- cur_beta
    } else { # for the other iteration
      beta <- c(beta*cur_beta[1],cur_beta[2])
      K1 <- K1*cur_beta[1]+K2*cur_beta[2]
    }    
    bestobj <- obj
    obj_set <- c(obj_set,bestobj)

    # find a violated label vector y
    if (flag) {          
      nk <- nk + 1  # increase the number of candidate label assignment
      alpha <- w    # record the model of MKL

      # find out the suboptimal label assignment among the working set
      u_inx <- which(beta != 0);
      optinx <- 1
      for (i in 1:length(u_inx)) {
        alphay <- alpha*t(y_set[u_inx[i],,drop=FALSE])
        tmpobj <- t(alphay) %*% K0 %*% alphay
        if (i == 1) {
          yy <- t(y_set[u_inx[i],,drop=FALSE])
          curobj <- tmpobj
        } else if (tmpobj > curobj) {
          yy <- t(y_set[u_inx[i],,drop=FALSE])
          curobj <- tmpobj
          optinx <- i
        }
      }

      # find a violated label via sorting
      sig <- 1
      i <- optinx
      res <- find_a_violated_label(alpha,K0,y,ind_y,lr,t(y_set[u_inx[i],,drop=FALSE]))
      tmpy <- res$y
      tmpobj <- res$obj
      
      # if the violation is less than a threshold, stop
      if (tmpobj > curobj*(1+1e-3)) {
        sig <- 0
        yy <- tmpy
        curobj <- tmpobj
      }
      
      if (sig == 1 || (iter == maxiter - 1)) {
        flag = 0
      } else {
        y_set <- rbind(y_set,t(yy))
        K2 <- yy %*% t(yy)
        cur_beta <- c(0.9, 0.1)            
        iter <- iter + 1
      }
    }
  }
  return(list(alpha=alpha,y_set=y_set,beta=beta,obj_set=obj_set))
}

MKL <- function(K0, K1, K2, y_set, C, C2, beta, ind_y) {

  nK <- nrow(y_set)
  n <- nrow(K0)
  iter <- 1
  obj_set <- c()
  linx <- which(ind_y==1)
  uinx <- which(ind_y == 0)
  vv <- matrix(1,n,1)
  vv[linx] = vv[linx]/C
  vv[uinx] = vv[uinx]/(C2)
  
  while (iter <= 10) {
    # prepare the kernels for combination
    if (nK == 1) {
      K = K1
    } else {
      K = K1*beta[1]+K2*beta[2]
    }
    K <- K*K0 + diag(as.numeric(vv))
    
    # training SVM when coeffients of kernels are given; 
    res <- svmproblem(K)
    alpha <- res$alpha
    b <- res$b
    obj <- res$obj
    obj_set <- c(obj_set,obj)
    
    # if the decrease of objectives is less than a threshold, stop 
    if (nK == 1) {
      return(list(beta=beta,alpha=alpha,b=b,obj=obj))
    }
    if (iter == 1) {
      bestobj <- obj
    } else {
      if (obj < 1.001*bestobj) {
        return(list(beta=beta,alpha=alpha,b=b,obj=obj))
      } else {
        bestobj <- obj;
      }
    }

    # otherwise, update the coefficients of kernels
    val <- c(0, 0)
    val[1] <- t(alpha) %*% (K1 * K0) %*% alpha
    val[2] <- t(alpha) %*% (K2 * K0) %*% alpha
    val <- beta * sqrt(val)
    beta <- val/sum(val)  
    iter <- iter +1
    #print("beta")
    #print(beta)
  }
  
  return(list(beta=beta,alpha=alpha,b=b,obj=obj))
}

# Prediction function
prediction_func <- function(alpha,y_set,beta,trainx,testx,options) {
  
  if (options$gaussian == 0) {
    K0 = t(trainx) %*% testx
  } else{
    K0 = gaussian_kernel(trainx,options$gamma,testx)$k
  }
  
  ind <- which(beta > 1e-2)
  y1 <- (t(alpha)*(beta[ind] %*% y_set[ind,,drop=FALSE]) ) %*% K0
  predy = t(y1)
  return(predy)
}

#' Find a violated label
#' @param alpha classifier weights
#' @param K kernel matrix
#' @param y label vector
#' @param ind_y Labeled/Unlabeled indicator
#' @param lr positive ratio
#' @param y_init label initialization     
find_a_violated_label <-  function(alpha,K,y,ind_y,lr,y_init) {

  
  u_inx <- which(ind_y == 0)
  un <- length(u_inx)
  pos_lr <- ceiling(un*lr)
  
  
  # baseline objective
  alphay <- alpha*y_init
  obj <- t(alphay) %*% K %*% alphay
  
  # iteratively optimize the objective until convergence
  flag <- 1
  cury <- y_init
  while (flag && un > 0) {
    
    # compute the linear term for sorting
    linearterm <- (t(cury*alpha)%*%K)*t(alpha)
    linearterm <- linearterm[u_inx]

    # sorting
    res <-  sort(linearterm,decreasing=TRUE,index.return=TRUE)
    val <- res$x
    inx <- res$ix
    
    # assign the labels according to the sorting results 
    tmpy <- y
    tmpy[u_inx[inx[1:pos_lr]]] <-  1
    
    # refine the objective
    alphay <- alpha*tmpy
    tmpobj <- t(alphay) %*% K %*% alphay

    # if objective decreases, continue; otherwise, stop;
    if (tmpobj > obj) {
      obj <- tmpobj;
      cury <- tmpy;
    } else {
      flag <- 0
    }
    
  }
  
  # return the violated label
  y <- cury
  
  return(list(y=y,obj=obj))
}

#' Train SVM
#' @param K kernel
#' @return alpha, b, obj
svmproblem <- function(K) {
  
  n <- ncol(K)
  K1 <- K
  
  model <- svmd(as.kernelMatrix(K1), y=1:n, type="one-classification",nu=1/n)
  # if (verbose) print(sum(K1))
  
  # calculate the objective and record the model
  ind <- model$index 
  x <- matrix(0,n,1);
  x[ind] <- model$coefs
  fval <- 0.5*t(x[ind]) %*% K[model$index,model$index,drop=FALSE] %*% x[ind]
  # if (verbose) cat("fval:",fval,"\n")
  b <- 0
  alpha <- x
  obj <- fval
  return(list(alpha=alpha,b=b,obj=obj))
}
