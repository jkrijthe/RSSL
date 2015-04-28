# library("flexclust")
# library("limSolve")
# 
# #' Safe Semi-Supervised Support Vector Machine Classifier (S4VM)
# #'
# #' Implementation of the Safe Semi-Supervised Support Vector Machine Classifier introduced by Li & Zhou 2011
# #' @references Yu-Feng Li and Zhi-Hua Zhou. Towards Making Unlabeled Data Never Hurt. In: Proceedings of the 28th International Conference on Machine Learning (ICML'11), Bellevue, Washington, 2011.
# #' @param X Design matrix
# #' @param y labels
# #' @param X_u Design matrix unlabeled data
# #' 
# #' @export
# S4VM<-function(X,y,X_u, kernel=rbfdot(), C1=100,C2=0.1,sample_time=100) {
# #   %  gamma: parameter gamma is the width of RBF kernel. Default value is
# #   %         average distance between instances.
# #   In our paper, all the features of the instances are normalized to [0,1]
# #   length(pdist(instance))/sum(pdist(instance))
#   
#   ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=FALSE,x_center=x_center)
#   X<-ModelVariables$X
#   y<-ModelVariables$y
#   Xu<-ModelVariables$X_u
#   scaling<-ModelVariables$scaling
#   classnames<-ModelVariables$classnames
#   Y <- ModelVariables$Y
#   label <- as.numeric(Y*2-1)
#   
#   labelNum <- nrow(X)
#   unlabelNum <- nrow(Xu)
#   instance <- rbind(X,Xu)
#   C <- c(rep(C1,L),rep(C2,U))
#   beta <- mean(label)
#   alpha <- 0.1
#   clusterNum <- floor(sample_time/10)
#   Y <- matrix(0,sample_time+1,labelNum+unlabelNum)
#   S <- matrix(0,sample_time+1,1)
#   
#   if (class(kernel)=='vanillakernel') {
#     svm(labelInstance,label,cost=C1)
#     #model=svmtrain(label,labelInstance,ones(labelNum,1)*C1,'-t 0');
#   } else {
#     #model=svmtrain(label,labelInstance,ones(labelNum,1)*C1,['-g ',num2str(gamma)]);
#   }
#   #[ysvm,~,~]=svmpredict([label;ones(unlabelNum,1)],instance,model);
#   
#   if (sum(ysvm(labelNum+1:labelNum+unlabelNum)>0)==0||sum(ysvm(labelNum+1:labelNum+unlabelNum)<0)==0) {
#     Y=Y[1:sample_time,,drop=FALSE]
#     S=S[1:sample_time,drop=FALSE]
#   } else {
#      #[predictBest,~,~,modelBest]=localDescent(instance,ysvm,labelNum,unlabelNum,gamma,C,beta,alpha);
#      Y[sample_time+1,,drop=FALSE] <- predictBest
#      S[sample_time+1] <- modelBest$obj
#   }
#   for (i in 1:sample_time) {
#    if (i<=sample_time*0.8) {
#          y <- runif(unlabelNum)
#          y[y>0.5] <- 1
#          y[y<=0.5] <- -1
#          labelNew <- c(label,y)
#     } else {
#          y <- runif(unlabelNum)
#          y[y>0.8] <- -1
#          y[y<=0.8] <- 1
#          labelNew <- c(label, y*ysvm[labelNum+1:labelNum+unlabelNum])
#     }
#     #[predictBest,~,~,modelBest]=localDescent(instance,labelNew,labelNum,unlabelNum,gamma,C,beta,alpha);
#     Y[i,,drop=FALSE]  <- predictBest;
#     S[i] <- modelBest.obj;
#   }
#   
#   #[IDX,~,~,D] <- kmeans(Y,clusterNum,'Distance','cityblock','EmptyAction','drop')
#   clustering <- flexclust::kcca(Y, clusterNum, family=kccaFamily("kmedians"), control=list(initcent="kmeanspp")) # We do not explicitly drop empty clusters
#   IDX <- clustering@cluster
#   D <- flexclust::dist2(Y,clustering@centers,method="manhattan")  
#   D <- colSums(D) # Total distance from cluster to all objects
#   
#   # Determine the number of clusters that are left, currently not done because none are dropped
#   #clusterIndex <- which(find(isnan(D)==0);
#   #clusterNum=size(find(isnan(D)==0),2);
#   prediction=zeros(labelNum+unlabelNum,clusterNum);
#   
#   for (i in 1:clusterNum) {
#     index <- which(IDX==i)
#     tempS <- S[index]
#     tempY <- Y[index,]
#     #[~,index2]=max(tempS)
#     which.max
#     prediction[,i] <- t(tempY[index2,])
#   }
#                                                                                      # use linear programming to get the final prediction
#   prediction <- linearProgramming(prediction,ysvm,labelNum,3);
# }                                                                                                                                                               
# 
# linearProgramming <- function(yp,ysvm,labelNum,lambda) {
#   yp <- yp[-c(1:labelNum),,drop=FALSE]
#   ysvm <- ysvm[-c(1:labelNum),,drop=FALSE]
#   
#   u <- nrow(yp)
#   yNum <- ncol(yp)
#   
#   #A=[ones(yNum,1) ((1-lambda)*repmat(ysvm,1,yNum)/4-(1+lambda)*yp/4)'];
#   A <- cbind(rep(1,yNum), )
#   #C=ones(yNum,1)*(1-lambda)*u/4-(1+lambda)*yp'*ysvm/4;
#   C <-  rep(1,yNum)*(1-lambda)*u/4 - (1+lambda)* t(yp) %*% ysvm/4
#   g <- c(-1,rep(0,u))
#   lb <- c(-Inf,rep(-1,u))
#   ub <- c(Inf,rep(1,u))
#   
#   prediction <- linp(E=NULL,F=NULL,G,H,Cost=g,ispos=FALSE);
#   
#   if (prediction[1]<0) {
#     label <- ysvm
#   } else {
#     prediction <- prediction[-1]
#     label <- sign(prediction);
#   }
#   return(label)
# }
# 
# localDescent <- function(instance,label,labelNum,unlabelNum,gamma,C,beta,alpha) {
#   predictLabelLastLast <- label
#   
#   if(gamma==0) {
#       #model=svmtrain(predictLabelLastLast,instance,C,'-t 0');
#   } else {
#       #model=svmtrain(predictLabelLastLast,instance,C,['-g ',num2str(gamma)]);
#   }
#   #[predictLabel,acc,values]=svmpredict(predictLabelLastLast,instance,model);
#   if(values(1)*predictLabel(1)<0) {
#     values=-values;
#   }
#   
#   #update predictLabel
#   h1=ceil((labelNum+unlabelNum)*(1+beta-alpha)/2);
#   h2=ceil((labelNum+unlabelNum)*(1-beta-alpha)/2);
#   
#   #[valuesSort,index]=sort(values,1,'descend');
#   predictLabel(index(1:h1))=1;
#   predictLabel(index(labelNum+unlabelNum-h2+1:labelNum+unlabelNum))=-1;
#   valuesSort=valuesSort((h1+1):(labelNum+unlabelNum-h2));
#   predictLabel(index(find(valuesSort>=0)+h1))=1;
#   predictLabel(index(find(valuesSort<0)+h1))=-1;
#   predictLabelLast=predictLabel;
#   modelLast=model;
#   
#   change<- generate_change_vector(unlabelNum,labelNum)
# 
#   #iterative
#   stopflag <- 0
#   numIterative <- 0
#   while (stopflag==0) {
#     #labelNew=change.*predictLabelLast+(1-change).*predictLabelLastLast;
#     if (gamma==0) {
#       #model=svmtrain(labelNew,instance,C,'-t 0');
#     } else {
#       #model=svmtrain(labelNew,instance,C,['-g ',num2str(gamma)]);
#     }
#     #[predictLabel,acc,values]=svmpredict(labelNew,instance,model);
#     numIterative <- numIterative+1
#     if (values[1]*predictLabel[1]<0) {
#       values <- -values;
#     }
#     
#     #update predictLabel
#     #[valuesSort,index]=sort(values,1,'descend');
#     predictLabel[index[1:h1]] <- 1
#     predictLabel[index[labelNum+unlabelNum-h2+1:labelNum+unlabelNum]] <- -1
#     valuesSort <- valuesSort[(h1+1):(labelNum+unlabelNum-h2)]
#     predictLabel[index[which(valuesSort>=0)+h1]] <- 1
#     predictLabel[index(find(valuesSort<0)+h1)] <- -1
# 
#     if((sum(predictLabel==predictLabelLast)==labelNum+unlabelNum && model.obj==modelLast.obj)||numIterative>200) {
#       stopflag <- 1
#     } else {
#       modelLast <- model
#       predictLabelLastLast <- predictLabelLast
#       predictLabelLast <- predictLabel
#       
#       change <- generate_change_vector(unlabelNum,labelNum)
#     }
#   
#   }
#   return(list(predictLabel=predictLabel,acc=acc,values=values,model=model))
# } 
# 
# generate_change_vector <- function(unlabelNum,labelNum) {
#   #generate a vector change of which 80% is 1 and rest is 0
#   num <- ceiling(unlabelNum*0.2)
#   index <- sample(1:unlabelNum)
#   index <- index[1:num];
#   change <- rep(1,unlabelNum)
#   change[index] <- 0;
#   change <- c(rep(1,labelNum),change)
# }