library(RSSL)

dataset<-Generate2ClassGaussian(n=1000,d = 2,var = 0.3,expected = TRUE)
dmat<-model.matrix(formula("y~.-1"),dataset)
tvec<-factor(dataset$y)

problem<-split_dataset_ssl(dmat,tvec,frac_train=0.5,frac_ssl=0.98)
sum(loss(LeastSquaresClassifier(problem$X,problem$y),problem$X_test,problem$y_test))
sum(loss(UCLeastSquaresClassifier(problem$X,problem$y,problem$X_u),problem$X_test,problem$y_test))
sum(loss(ICLeastSquaresClassifier(problem$X,problem$y,problem$X_u),problem$X_test,problem$y_test))
sum(loss(ICLeastSquaresClassifier(problem$X,problem$y,problem$X_u,projection="semisupervised"),problem$X_test,problem$y_test))
sum(loss(CPLeastSquaresClassifier(problem$X,problem$y,problem$X_u),problem$X_test,problem$y_test))
mean(predict(CPLeastSquaresClassifier(problem$X,problem$y,problem$X_u),problem$X_test)==problem$y_test)
mean(predict(ICLeastSquaresClassifier(problem$X,problem$y,problem$X_u,projection="semisupervised"),problem$X_test)==problem$y_test)
mean(predict(LeastSquaresClassifier(problem$X,problem$y),problem$X_test)==problem$y_test)

#TODO: are CP and IC the same?

sum(loss(SelfLearning(X=problem$X,y=problem$y,X_u=problem$X_u,method=LeastSquaresClassifier),problem$X_test,problem$y_test))

# Nearest Mean
sum(loss(NearestMeanClassifier(X=problem$X,y=problem$y),problem$X_test,problem$y_test))
sum(loss(ICNearestMeanClassifier(X=problem$X,y=problem$y,X_u=problem$X_u),problem$X_test,problem$y_test))
sum(loss(EMNearestMeanClassifier(X=problem$X,y=problem$y,X_u=problem$X_u),problem$X_test,problem$y_test))
sum(loss(SelfLearning(X=problem$X,y=problem$y,X_u=problem$X_u,method=NearestMeanClassifier),problem$X_test,problem$y_test))


# LDA
sum(loss(LinearDiscriminantClassifier(X=problem$X,y=problem$y),problem$X_test,problem$y_test))
sum(loss(ICLinearDiscriminantClassifier(X=problem$X,y=problem$y,X_u=problem$X_u),problem$X_test,problem$y_test))
sum(loss(EMLinearDiscriminantClassifier(X=problem$X,y=problem$y,X_u=problem$X_u),problem$X_test,problem$y_test))
sum(loss(CPLinearDiscriminantClassifier(X=problem$X,y=problem$y,X_u=problem$X_u),problem$X_test,problem$y_test))
sum(loss(SelfLearning(X=problem$X,y=problem$y,X_u=problem$X_u,method=LinearDiscriminantClassifier),problem$X_test,problem$y_test))


# Logistic Regression
sum(loss(LogisticRegression(X=problem$X,y=problem$y),problem$X_test,problem$y_test))
sum(loss(LogisticLossClassifier(X=problem$X,y=problem$y,x_center=TRUE),problem$X_test,problem$y_test))
sum(loss(ICLogisticLossClassifier(X=problem$X,y=problem$y,X_u=problem$X_u),problem$X_test,problem$y_test))
sum(loss(ERLogisticLossClassifier(X=problem$X,y=problem$y,X_u=problem$X_u),problem$X_test,problem$y_test))
mean(predict(LogisticLossClassifier(X=problem$X,y=problem$y),problem$X_test)==problem$y_test)
mean(predict(ERLogisticLossClassifier(X=problem$X,y=problem$y,X_u=problem$X_u),problem$X_test)==problem$y_test)

# SVM
sum(loss(LinearSVM(X=problem$X,y=problem$y),problem$X_test,problem$y_test))


# Graph based
#GRF