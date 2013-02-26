library(mlbench)

# Initialization
modelform<-formula("factor(classes)~.")
classname<-all.vars(modelform)[1]
distance<-1
D_pop<-data.frame(mlbench.2dnormals(10,2,distance))
D_test<-data.frame(mlbench.2dnormals(10000,2,distance))

D_train<-D_pop
D_train[1,classname]<-NA

logLik(NearestMeanClassifier(modelform,D_train), D_pop[2:10,])
logLik(MCNearestMeanClassifier(modelform,D_train), D_pop[2:10,])



#mean(predict(LogisticRegression(modelform,D_l),D_test)==D_test[,classname])
#mean(predict(FisherClassifier(modelform,D_l),D_test)==D_test[,classname])
#mean(predict(SemiSupervisedFisherClassifier(modelform,D_train),D_test)==D_test[,classname])
#mean(predict(FisherClassifierALL(modelform,D_train),D_test)==D_test$classes)

#p<-plot(SemiSupervisedFisherClassifier(modelform,D_train))
#s<-FisherClassifier(modelform,D_l)
#p+geom_abline(intercept = (1.5-s@theta[1])/s@theta[3], slope = -s@theta[2]/s@theta[3])