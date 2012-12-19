# Using unlabeled data to improve error estimators

library(bootstrap)

# Parameters of simulation
repeats<-100
modelform<-formula("factor(classes)~.")
classname<-all.vars(modelform)[1]
results<-matrix(nrow=repeats,ncol=12)


distance<-0.5
D_test<-data.frame(mlbench.2dnormals(10000,2,1))
D_pop<-data.frame(mlbench.2dnormals(10000,2,1))

#D_test<-data.frame(mlbench.hypercube(10000,2,0.1))
#D_pop<-data.frame(mlbench.hypercube(10000,2,0.1))

#D_test<-data.frame(mlbench.ringnorm(100000,2))
#D_pop<-data.frame(mlbench.ringnorm(100000,2))

for (r in 1:repeats) {
  print(r)
  i_train<-strata(D_pop,classname,rep(4,2),method="srswor")$ID_unit
  D_train<-D_pop[i_train,]
  D_u<-D_pop
  yhat_cv5<-crossvalError(modelform,D_train,5)$cv.fit
  yhat_cv10<-crossvalError(modelform,D_train,10)$cv.fit
  yhat_cvLOO<-crossvalError(modelform,D_train,2)$cv.fit
  bs<-bootstrapError(modelform,D_train,200)
  g<-nearestmean(modelform,D_train)
  results[r,1]<-1-mean(D_test$classes==predict(g,D_test)) # Real error
  results[r,2]<-1-mean(D_train$classes==predict(g,D_train)) # Apparent error
  results[r,3]<-1-mean(D_train$classes==yhat_cv5)
  results[r,4]<-1-mean(D_train$classes==yhat_cv10)
  results[r,5]<-1-mean(D_train$classes==yhat_cvLOO)
  results[r,6]<-bs[[1]]
  results[r,7]<-bs[[3]]
  #results[r,8]<-DensityError(modelform,D_train,D_u)
  results[r,9]<-updatedBootstrapError(modelform,D_train,D_u,200)
  results[r,10]<-0.368*results[r,2]+0.632*results[r,9]
  #results[r,11]<-updatedBootstrapError2(modelform,D_train,D_u,200)
  #results[r,12]<-0.368*results[r,2]+0.632*results[r,11]
}

diff<-results[,-1]-results[,1]
boxplot(diff,las=2,names=c("Apparent","CV5","CV10","CV2","BS","BS0.632","SS Apparent","SS BS","SS BS0.632","SS2 BS","SS2 BS0.632"))
abline(h=0)
colMeans(diff)
summary(diff)
apply(diff,2,sd)
apply(diff,2,median)