library(RSSL)

split_dataset_ssl<-function(X, y, frac_train=0.8, frac_ssl=0.8) {
  n<-length(y)
  idx_train<-sample(1:n,size=ceiling(frac_train*n))
  n_lab<-length(idx_train) #Number of labeled objects
  idx_labeled<-idx_train[sample(1:n_lab,size=ceiling((1-frac_ssl)*n_lab))]
  
  return(list(X=X[idx_labeled,,drop=FALSE],
              y=y[idx_labeled], 
              X_u=X[setdiff(idx_train,idx_labeled),,drop=FALSE],
              y_u=y[setdiff(idx_train,idx_labeled)],
              X_test=X[-idx_train,,drop=FALSE],
              y_test=y[-idx_train])
         )
}

dataset<-Generate2ClassGaussian(n=1000,d = 2,var = 0.3,expected = TRUE)
dmat<-model.matrix(formula("y~.-1"),dataset)
tvec<-factor(dataset$y)
problem<-split_dataset_ssl(dmat,tvec,frac_train=0.5,frac_ssl=0.80)




mean(predict(LeastSquaresClassifier(problem$X,problem$y),problem$X_test)==problem$y_test) # Error
mean(predict(ICLeastSquaresClassifier(problem$X,problem$y,problem$X_u),problem$X_test)==problem$y_test)
sum(loss(LeastSquaresClassifier(problem$X,problem$y),rbind(problem$X,problem$X_u),unlist(list(problem$y,problem$y_u))))
sum(loss(ICLeastSquaresClassifier(problem$X,problem$y,problem$X_u),rbind(problem$X,problem$X_u),unlist(list(problem$y,problem$y_u))))
sum(loss(ICLeastSquaresClassifier(problem$X,problem$y,problem$X_u,projection = "semisupervisedold"),rbind(problem$X,problem$X_u),unlist(list(problem$y,problem$y_u))))
sum(loss(ICLeastSquaresClassifier(problem$X,problem$y,problem$X_u,projection = "semisupervised"),rbind(problem$X,problem$X_u),unlist(list(problem$y,problem$y_u))))
sum(loss(ICLeastSquaresClassifier(problem$X,problem$y,problem$X_u,projection = "semisupervised",x_center = TRUE,scale=TRUE),rbind(problem$X,problem$X_u),unlist(list(problem$y,problem$y_u))))


#Regularized solutions
sum(loss(LeastSquaresClassifier(problem$X,problem$y,lambda=100),rbind(problem$X,problem$X_u),c(problem$y,problem$y_u)))
sum(loss(ICLeastSquaresClassifier(problem$X,problem$y,problem$X_u,projection = "semisupervised",lambda2=100),rbind(problem$X,problem$X_u),c(problem$y,problem$y_u)))
sum(loss(LeastSquaresClassifier(problem$X,problem$y,lambda=0.01),problem$X_test,problem$y_test)) # Error
sum(loss(ICLeastSquaresClassifier(problem$X,problem$y,problem$X_u,projection = "semisupervised",lambda2=0.01),problem$X_test,problem$y_test))
sum(loss(ICLeastSquaresClassifier(problem$X,problem$y,problem$X_u,projection = "supervised",lambda2=0.01),problem$X_test,problem$y_test))

losses<-cbind(
  loss(LeastSquaresClassifier(problem$X,problem$y),rbind(problem$X,problem$X_u),unlist(list(problem$y,problem$y_u))),
  loss(ICLeastSquaresClassifier(problem$X,problem$y,problem$X_u),rbind(problem$X,problem$X_u),unlist(list(problem$y,problem$y_u))),
  loss(ICLeastSquaresClassifier(problem$X,problem$y,problem$X_u,projection = "semisupervised"),rbind(problem$X,problem$X_u),unlist(list(problem$y,problem$y_u)))
  )

table(apply(losses[,c(1,3)],1,which.min))
losses<-cbind(1:nrow(losses),losses)
colnames(losses)<-c("id","SL","IC","CP")
library(ggplot2)
plotdata<-reshape(data.frame(losses), varying=c("SL","IC","CP"), direction = "long",timevar="method",times=c("SL","IC","CP"),v.names="loss")

ggplot(data=plotdata,aes(id,fill=method,y=loss)) + geom_bar(stat="identity",position="dodge")

clplot(rbind(problem$X,problem$X_u),c(problem$y,rep(NA,nrow(problem$X_u))))
# What does this result mean?
losses<-cbind(loss(LeastSquaresClassifier(problem$X,problem$y),rbind(problem$X,problem$X_u),c(problem$y,problem$y_u)),loss(ICLeastSquaresClassifier(problem$X,problem$y,problem$X_u,projection = "semisupervised"),rbind(problem$X,problem$X_u),c(problem$y,problem$y_u)))
plot(losses,asp=1,xlab="Losses Supervised LS",ylab="Losses Projection LS",main="Losses on labeled and unlabeled objects")
abline(a=0,b=1)

clplot(rbind(problem$X,problem$X_u),c(problem$y,rep(NA,nrow(problem$X_u))))
# What does this result mean?
losses<-cbind(
  loss(LeastSquaresClassifier(problem$X,problem$y),rbind(problem$X,problem$X_u),unlist(list(problem$y,problem$y_u))),
  loss(LeastSquaresClassifier(problem$X,problem$y,lambda=1),rbind(problem$X,problem$X_u),unlist(list(problem$y,problem$y_u)))
)
plot(losses,asp=1,xlab="Losses Supervised LS",ylab="Losses Regularized LS",main="Losses on labeled and unlabeled objects")
abline(a=0,b=1)

repeats<-1000
results<-matrix(NA,repeats,2)
for (i in 1:repeats) { 
  dataset<-Generate2ClassGaussian(n=1000,d = 2,var = 0.3,expected = TRUE)
  dmat<-model.matrix(formula("y~.-1"),dataset)
  tvec<-factor(dataset$y)
  
  problem<-split_dataset_ssl(dmat,tvec,frac_train=0.5,frac_ssl=0.98)
  losses<-cbind(loss(LeastSquaresClassifier(problem$X,problem$y),rbind(problem$X,problem$X_u),c(problem$y,problem$y_u)),loss(ICLeastSquaresClassifier(problem$X,problem$y,problem$X_u),rbind(problem$X,problem$X_u),c(problem$y,problem$y_u)),loss(ICLeastSquaresClassifier(problem$X,problem$y,problem$X_u,projection = "semisupervised"),rbind(problem$X,problem$X_u),c(problem$y,problem$y_u)))
  print(i)
  results[i,]<-table(apply(losses[,c(1,3)],1,which.min))
}

# Multiclass
dmat<-model.matrix(Species~.-1,iris[1:150,])
tvec<-droplevels(iris$Species[1:150])
problem<-split_dataset_ssl(dmat,tvec,frac_train=0.5,frac_ssl=0.80)
mean(predict(LeastSquaresClassifier(problem$X,problem$y),problem$X_test)==problem$y_test)
mean(predict(ICLeastSquaresClassifier(problem$X,problem$y,problem$X_u,projection="semisupervised",method="QP",eps=10e-10),problem$X_test)==problem$y_test)
mean(predict(ICLeastSquaresClassifier(problem$X,problem$y,problem$X_u,projection="supervised",method="QP",eps=10e-10),problem$X_test)==problem$y_test)
mean(predict(ICLeastSquaresClassifier(problem$X,problem$y,problem$X_u,projection="euclidean",method="QP",eps=10e-10),problem$X_test)==problem$y_test)