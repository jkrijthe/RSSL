repeats<-100

library(mlbench)
modelform<-formula("factor(classes)~.")
classname<-all.vars(modelform)[1]
results<-matrix(nrow=repeats,ncol=4)
D_pop<-data.frame(mlbench.2dnormals(10000,2,distance))
for (i in 1:repeats) {
  distance<-2
  D_test<-data.frame(mlbench.2dnormals(10000,2,distance))
  
  
  i_train<-strata(D_pop,classname,c(4,4),method="srswor")$ID_unit
  D_train<-D_pop[i_train,]
  i_l <- createDataPartition(D_train[,classname], p = .5, list = FALSE, times = 1)
  D_l <- D_train[i_l,]
  D_u <- D_train[-i_l,]
  
  D_u[,classname]<-rep(NA,nrow(D_u))
  D_train<-rbind(D_l,D_u)
  
  
  g_constrained <- constrainedlr(modelform, D_train)
  g_unconstrained <- lr(modelform, D_train)
  #g_ga <- gaconstrainedlr(modelform, D_train)
  
  y<-as.numeric(data.matrix(D_l[,classname]))-1
  X<-data.matrix(cbind(rep(1,nrow(D_l)),D_l[,!(colnames(D_l) %in% c(classname)), drop=FALSE]))
  logLik(g_unconstrained)
  loglikelihood_logisticregression(g_unconstrained$coefficients,X,y)
  
  # Results: Classification errors on test set
  results[i,1]<-1-mean(as.integer(D_test$classes)==as.integer(predict(g_constrained,D_test,type="response")>0.5)+1)
  results[i,2]<-1-mean(as.integer(D_test$classes)==as.integer(predict(g_unconstrained,D_test,type="response")>0.5)+1)
  
  #gr<-qplot(x.1, x.2, colour = classes, data = D_test, asp=1)
  #gr + geom_abline(intercept=-theta[1]/theta[3], slope=-theta[2]/theta[3])
  
  # Results: Log likelihood on test set
  y<-as.numeric(data.matrix(D_test[,classname]))-1
  X<-data.matrix(cbind(rep(1,nrow(D_test)),D_test[,!(colnames(D_test) %in% c(classname)), drop=FALSE]))
  results[i,3]<-loglikelihood_logisticregression(g_constrained$coefficients,X,y)
  results[i,4]<-loglikelihood_logisticregression(g_unconstrained$coefficients,X,y)
}
print(colMeans(results))
var(results)

print("Percentage of datasets where classification error of constrained approach is lower:")
print(mean(results[!apply(is.infinite(results), 1, any),1]<results[!apply(is.infinite(results), 1,any),2]))
print("Percentage of datasets where likelihood on the test set constrained approach is better (higher:")
print(mean(results[!apply(is.infinite(results), 1, any),3]>results[!apply(is.infinite(results), 1,any),4]))