repeats<-1

library(caret)
library(mlbench)
library(class)
modelform<-formula("factor(classes)~.")
classname<-all.vars(modelform)[1]
results<-matrix(nrow=repeats,ncol=10)

distance<-2.0
D_test<-data.frame(mlbench.2dnormals(1000,2,distance))
D_pop<-data.frame(mlbench.2dnormals(10000,2,distance))
#D_test<-generateBananaSet(10000,2,2)
#D_pop<-generateBananaSet(10000,2,2)

D_test<-data.frame(mlbench.ringnorm(10000,1))
D_pop<-data.frame(mlbench.ringnorm(10000,1))

D_test<-data.frame(mlbench.threenorm(10000,1))
D_pop<-data.frame(mlbench.threenorm(10000,1))

for (i in 1:repeats) {
  
  i_train<-strata(D_pop,classname,c(4,4),method="srswor")$ID_unit
  D_train<-D_pop[i_train,]
  i_l <- createDataPartition(D_train[,classname], p = .5, list = FALSE, times = 1)
  D_l <- D_train[i_l,]
  D_u <- D_train[-i_l,]
  
  D_u[,classname]<-rep(NA,nrow(D_u))
  D_train<-rbind(D_l,D_u)
  
  library(klaR)
  g_constrained <- MomentConstrainedNearestMeanClassifier(modelform, D_train)
  g_impconstrained <- ImplicitlyConstrainedNearestMeanClassifier(modelform, D_train)
  g_unconstrained <- NearestMeanClassifier(modelform,D_l)
  g_selflearned <- SelfLearning(modelform,D_l,NearestMeanClassifier)
  
  #g_ga <- gaconstrainedlr(modelform, D_train)
  
  #y<-as.numeric(data.matrix(D_l[,classname]))-1
  #X<-data.matrix(cbind(rep(1,nrow(D_l)),D_l[,!(colnames(D_l) %in% c(classname)), drop=FALSE]))
  #logLik(g_unconstrained)
  #loglikelihood_logisticregression(g_unconstrained$coefficients,X,y)
  
  # Results: Classification errors on test set
  results[i,1]<-1-mean(D_test$classes==predict(g_constrained,D_test))
  results[i,2]<-1-mean(D_test$classes==predict(g_impconstrained,D_test))
  results[i,3]<-1-mean(D_test$classes==predict(g_unconstrained,D_test))
  results[i,4]<-1-mean(D_test$classes==predict(g_selflearned,D_test))
  #gr<-qplot(x.1, x.2, colour = classes, data = D_test, asp=1)
  #gr + geom_abline(intercept=-theta[1]/theta[3], slope=-theta[2]/theta[3])
  
  # Results: Log likelihood on test set
  y<-as.numeric(data.matrix(D_test[,classname]))-1
  X<-data.matrix(cbind(rep(1,nrow(D_test)),D_test[,!(colnames(D_test) %in% c(classname)), drop=FALSE]))
  
  results[i,5]<-logLik(g_constrained, D_test)
  results[i,6]<-logLik(g_impconstrained, D_test)
  results[i,7]<-logLik(g_unconstrained, D_test)
  
  results[i,8]<-logLik(g_constrained, D_l)
  results[i,9]<-logLik(g_impconstrained, D_l)
  results[i,10]<-logLik(g_unconstrained, D_l)
  
  plot(g_impconstrained@parameters.allowed[,1],g_impconstrained@parameters.allowed[,2])
  points(g_unconstrained@means[1,],g_unconstrained@means[2,],col="green")
  points(g_constrained@means[1,],g_constrained@means[2,],col="red")
  
  pl<-qplot(g_impconstrained@parameters.allowed[,1],g_impconstrained@parameters.allowed[,2],color=g_impconstrained@parameters.allowed[,3])
  pl<-pl+geom_point(x=g_unconstrained@means[1,],y=g_unconstrained@means[2,],shape=1)
  pl+geom_point(x=g_constrained@means[1,],y=g_constrained@means[2,],shape=2)
}
print(colMeans(results))
var(results)

print("Percentage of datasets where classification error of constrained approach is lower:")
print(mean(results[!apply(is.infinite(results), 1, any),1]<results[!apply(is.infinite(results), 1,any),3]))