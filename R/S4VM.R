#' @include Classifier.R
NULL

#' LinearSVM Class  
setClass("S4VM",
         representation(predictions="factor",labelings="ANY"),
         prototype(name="Safe Semi-supervised Support Vector Machine"),
         contains="Classifier")

#' Safe Semi-supervised Support Vector Machine (S4VM)
#'
#' R port of the MATLAB implementation of Li & Zhou (2011) of the Safe Semi-supervised Support Vector Machine.
#' 
#' The method randomly generates multiple low-density separators (controlled by the sample_time parameter) and merges their predictions by solving a linear programming problem meant to penalize the cost of decreasing the performance of the classifier, compared to the supervised SVM. S4VM is a bit of a misnomer, since it is a transductive method that only returns predicted labels for the unlabeled objects. The main difference in this implementation compared to the original implementation is the clustering of the low-density separators: in our implementation empty clusters are not dropped during the k-means procedure. In the paper by Li (2011) the features are first normalized to [0,1], which is not automatically done by this function. Note that the solution may not correspond to a linear classifier even if the linear kernel is used.
#' 
#' @references Yu-Feng Li and Zhi-Hua Zhou. Towards Making Unlabeled Data Never Hurt. In: Proceedings of the 28th International Conference on Machine Learning (ICML'11), Bellevue, Washington, 2011.
#' 
#' @family RSSL classifiers
#' 
#' @param C1 double; Regularization parameter for labeled data
#' @param C2 double; Regularization parameter for unlabeled data
#' @param gamma double; Width of RBF kernel
#' @param sample_time integer; Number of low-density separators that are generated
#' @param lambda_tradeoff numeric; Parameter that determines the amount of "risk" in obtaining a worse solution than the supervised solution, see Li & Zhou (2011)
#' @inheritParams BaseClassifier
#' 
#' @return S4VM object with slots:
#' \item{predictions}{Predictions on the unlabeled objects}
#' \item{labelings}{Labelings for the different clusters}
#' 
#' @example inst/examples/example-S4VM.R
#' @export
S4VM<-function(X,y,X_u=NULL, C1=100, C2=0.1, sample_time=100, gamma=0, x_center=FALSE,scale=FALSE,lambda_tradeoff=3) {

  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=FALSE,x_center=x_center)
  
  X<-ModelVariables$X
  y<-ModelVariables$y
  Xu<-ModelVariables$X_u
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  Y <- ModelVariables$Y[,1,drop=FALSE]
  label <- as.numeric(Y*2-1)
  y <- label
  
  labelNum <- nrow(X)
  unlabelNum <- nrow(Xu)
  instance <- rbind(X,Xu)
  C <- c(rep(C1,labelNum),rep(C2,unlabelNum))
  beta <- mean(label)
  alpha <- 0.1
  clusterNum <- floor(sample_time/10)
  Y <- matrix(0,sample_time+1,labelNum+unlabelNum)
  S <- matrix(0,sample_time+1,1)

  if (gamma==0) {
    model <- svmd(X,as.numeric(y),cost=C1,type="C-classification",kernel="linear",scale=FALSE)
  } else {
    model <- svmd(X,as.numeric(y),cost=C1,type="C-classification",kernel="radial",gamma=gamma,scale=FALSE)
  }
  ysvm <- as.numeric(as.character(predict.svmd(model,instance)))
  
  if (sum(ysvm[(labelNum+1):(labelNum+unlabelNum)]>0)==0 || sum(ysvm[(labelNum+1):(labelNum+unlabelNum)]<0)==0) {
    Y <- Y[1:sample_time,,drop=FALSE]
    S <- S[1:sample_time,drop=FALSE]
  } else {
    res <- localDescent(instance,ysvm,labelNum,unlabelNum,gamma,C,beta,alpha)
    predictBest <- res$predictLabel
    modelBest <- res$model
    
    Y[sample_time+1,] <- predictBest
    S[sample_time+1] <- modelBest$obj
  }
  for (i in 1:sample_time) {
   if (i<=sample_time*0.8) {
         y <- runif(unlabelNum)
         y[y>0.5] <- 1
         y[y<=0.5] <- -1
         labelNew <- c(label,y)
    } else {
         y <- runif(unlabelNum)
         y[y>0.8] <- -1
         y[y<=0.8] <- 1
         labelNew <- c(label, y*ysvm[(labelNum+1):(labelNum+unlabelNum)])
    }
    res <- localDescent(instance,labelNew,labelNum,unlabelNum,gamma,C,beta,alpha)
    predictBest <- res$predictLabel
    modelBest <- res$model
    Y[i,]  <- predictBest
    S[i] <- modelBest$obj
  }
  
  #[IDX,~,~,D] <- kmeans(Y,clusterNum,'Distance','cityblock','EmptyAction','drop')
  clusterNum <- min(c(nrow(unique(Y)),clusterNum))
  if (clusterNum==1) {
    warning("Only found one cluster of solutions!")
    IDX <- 1
    # Code using flexclust: gives problems on Windows platform
    # D <- flexclust::dist2(Y,Y[1,,drop=FALSE],method="manhattan")
    # D <- colSums(D) # Total distance from cluster to all objects
    D <- dist(Y,method = "manhattan") # Not very efficient...
    sum(D[seq_len(nrow(Y)-1)])
  }
  else {
    # Code using flexclust: gives problems on Windows platform
    # 
    # clustering2 <- flexclust::kcca(Y, clusterNum, family=flexclust::kccaFamily("kmedians"), control=list(initcent="kmeanspp")) # We do not explicitly drop empty clusters
    # IDX2 <- clustering2@cluster
    # D2 <- flexclust::dist2(Y,clustering2@centers,method="manhattan")
    # D2 <- colSums(D2) # Total distance from cluster to all objects

    clustering <- cluster::pam(Y,clusterNum,metric="manhattan",keep.diss=TRUE)
    
    IDX <- clustering$clustering
    D <- as.matrix(clustering$diss)[,clustering$id.med,drop=FALSE]
    D <- colSums(D)
  }
  

  # Determine the number of clusters that are left, currently not done because none are dropped
  #clusterIndex <- which(isnan(D)==0);
  #clusterNum=size(find(isnan(D)==0),2);
  
  prediction <- matrix(0,(labelNum+unlabelNum),clusterNum)

  for (i in seq_len(clusterNum)) {
    index <- which(IDX==i)
    tempS <- S[index]
    tempY <- Y[index,,drop=FALSE]
    #[~,index2]=max(tempS)
    index2 <- which.max(tempS)
    prediction[,i] <- t(tempY[index2,,drop=FALSE])
  }
  
  
  # use linear programming to get the final prediction
  
  predictions <- linearProgramming(prediction,ysvm,labelNum,lambda_tradeoff)
  
  return(new("S4VM",
             labelings=prediction,
             predictions=factor(predictions<0,levels=c(FALSE,TRUE),labels=classnames)))
}

linearProgramming <- function(yp,ysvm,labelNum,lambda) {
  yp <- yp[-c(1:labelNum),,drop=FALSE]
  ysvm <- ysvm[-c(1:labelNum)]

  u <- nrow(yp)
  yNum <- ncol(yp)

  
  #A=[ones(yNum,1) ((1-lambda)*repmat(ysvm,1,yNum)/4-(1+lambda)*yp/4)'];
  A <- cbind(matrix(1,yNum,1),  ((1-lambda)*matrix(rep(ysvm,yNum),nrow=yNum,byrow=TRUE)/4-(1+lambda)*t(yp)/4))
  A <- rbind(-A,diag(ncol(A))[-1,],-diag(ncol(A))[-1,])
  
  #C=ones(yNum,1)*(1-lambda)*u/4-(1+lambda)*yp'*ysvm/4;
  C <- rep(1,yNum)*(1-lambda)*u/4 - (1+lambda)* t(yp) %*% ysvm/4
  
  
  
  g <- c(-1,rep(0,u))
  
  
  lb <- c(rep(-1,u))
  ub <- c(rep(-1,u))
  C <- c(-C,lb,ub)
  
  prediction <- solve.QP(Dmat = diag(length(g))*1e-8,dvec=-g,
           Amat=t(A),bvec=C)$solution
  
  # Alternative requires limSolve package
  #alternative <- limSolve::linp(E=NULL,F=NULL,G=A,H=C,Cost=g,ispos=FALSE)$X
  #print(max(abs(prediction-alternative)))
  
  if (prediction[1] < -1e-5) {
    label <- ysvm
  } else {
    prediction <- prediction[-1]
    label <- sign(prediction)
  }
  return(label)
}

#' Local descent
#' 
#' Local descent used in S4VM
#' 
#' @param instance Design matrix
#' @param label label vector
#' @param labelNum Number of labeled objects
#' @param unlabelNum Number of unlabeled objects
#' @param gamma Parameter for RBF kernel
#' @param C cost parameter for SVM
#' @param beta Controls fraction of objects assigned to positive class
#' @param alpha Controls fraction of objects assigned to positive class
#' @return list(predictLabel=predictLabel,acc=acc,values=values,model=model)
localDescent <- function(instance,label,labelNum,unlabelNum,gamma,C,beta,alpha) {
  predictLabelLastLast <- label

  if(gamma==0) {
      model <- svmd(instance,predictLabelLastLast,upbound=C,type="C-classification",scale=FALSE,kernel="linear")
  } else {
      model <- svmd(instance,predictLabelLastLast,upbound=C,type="C-classification",scale=FALSE,kernel="radial",gamma=gamma)
  }
  
  res <- predict.svmd(model,instance,decision.values=TRUE)
  predictLabel <- as.integer(as.character(res))
  values <- attr(res,"decision.values")
  acc <- mean(predictLabel==predictLabelLastLast) # Is not really used anymore
  #print(acc)
  
  if (values[1]*predictLabel[1]<0) {
    values <- -values
  }
  
  # Update predictLabel
  h1 <- ceiling((labelNum+unlabelNum)*(1+beta-alpha)/2)
  h2 <- ceiling((labelNum+unlabelNum)*(1-beta-alpha)/2)
  
  res <- sort(values,decreasing=TRUE,index.return=TRUE)
  valuesSort <- res$x
  index <- res$ix
  
  predictLabel[index[1:h1]] <- 1
  predictLabel[index[(labelNum+unlabelNum-h2+1):(labelNum+unlabelNum)]] <- -1
  valuesSort <- valuesSort[(h1+1):(labelNum+unlabelNum-h2)]
  predictLabel[index[which(valuesSort>=0)+h1]] <- 1
  predictLabel[index[which(valuesSort<0)+h1]] <- -1
  predictLabelLast <- predictLabel
  modelLast <- model

  change <- generate_change_vector(unlabelNum,labelNum)

  # iterative
  stopflag <- 0
  numIterative <- 0
  while (stopflag==0) {
    labelNew <- change*predictLabelLast + (1-change)*predictLabelLastLast
    if(gamma==0) {
      model <- svmd(instance,labelNew,upbound=C,type="C-classification",scale=FALSE,kernel="linear")
    } else {
      model <- svmd(instance,labelNew,upbound=C,type="C-classification",scale=FALSE,kernel="radial",gamma=gamma)
    }
    
    res <- predict.svmd(model,instance,decision.values=TRUE)
    predictLabel <- as.integer(as.character(res))
    values <- attr(res,"decision.values")
    acc <- mean(predictLabel==labelNew)
    #print(acc)
    numIterative <- numIterative+1
    if (values[1]*predictLabel[1]<0) {
      values <- -values;
    }

    #update predictLabel
    res <- sort(values,decreasing=TRUE,index.return=TRUE)
    valuesSort <- res$x
    index <- res$ix
    
    predictLabel[index[1:h1]] <- 1
    predictLabel[index[(labelNum+unlabelNum-h2+1):(labelNum+unlabelNum)]] <- -1
    valuesSort <- valuesSort[(h1+1):(labelNum+unlabelNum-h2)]
    predictLabel[index[which(valuesSort>=0)+h1]] <- 1
    predictLabel[index[which(valuesSort<0)+h1]] <- -1

    if((sum(predictLabel==predictLabelLast)==(labelNum+unlabelNum) && model$obj==modelLast$obj) ||
       numIterative>200) {
      stopflag <- 1
    } else {
      modelLast <- model
      predictLabelLastLast <- predictLabelLast
      predictLabelLast <- predictLabel

      change <- generate_change_vector(unlabelNum,labelNum)
    }

  }
  #print(numIterative)
  return(list(predictLabel=predictLabel,acc=acc,values=values,model=model))
}

# generate a vector change of which 80% is 1 and rest is 0
generate_change_vector <- function(unlabelNum,labelNum) {
  
  num <- ceiling(unlabelNum*0.2)
  index <- sample(1:unlabelNum)
  index <- index[1:num];
  change <- rep(1,unlabelNum)
  change[index] <- 0;
  change <- c(rep(1,labelNum),change)
}
