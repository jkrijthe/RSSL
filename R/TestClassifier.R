library(mlbench)

# Settings
repeats<-100
distance<-2

# Initialization
modelform<-formula("factor(classes)~.")
classname<-all.vars(modelform)[1]
results<-matrix(nrow=repeats,ncol=8)

D_pop<-data.frame(mlbench.hypercube(10000,1))
D_test<-data.frame(mlbench.hypercube(10000,1))

#D_pop<-data.frame(mlbench.2dnormals(10000,cl=2))
#D_test<-data.frame(mlbench.2dnormals(10000,cl=2))

# Create Dataset 
i_train<-strata(D_pop,classname,rep(8,2),method="srswor")$ID_unit
D_train<-D_pop[i_train,]
i_l <- createDataPartition(D_train[,classname], p = .5, list = FALSE, times = 1)
D_l <- D_train[i_l,]
D_u <- D_train[-i_l,]
  
  D_u[,classname]<-rep(NA,nrow(D_u))
  D_train<-rbind(D_l,D_u)

p<-qplot(x,classes,data=D_l)
g<-FisherClassifier(modelform,D_l)
p<-p+geom_abline(intercept=g@theta[1],slope=g@theta[2],color="red")
g<-SemiSupervisedFisherClassifier(modelform,D_train)
p<-p+geom_abline(intercept=g@theta[1],slope=g@theta[2],color="blue")
p

g<-FisherClassifier(modelform,D_l)
1-mean(D_test$classes==predict(g,D_test))
g<-SemiSupervisedFisherClassifier(modelform,D_train)
g<-LDAClassifier(modelform,D_train)