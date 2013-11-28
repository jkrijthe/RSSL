# library(mlbench)

# # Initialize dataset
# modelform<-formula("factor(classes)~.")
# classname<-all.vars(modelform)[1]
# distance<-1
# D<-data.frame(mlbench.2dnormals(500,2,distance))
# D[11:500,classname]<-rep(NA,490)
# D_test<-data.frame(mlbench.2dnormals(10000,2,distance))
library(devtools)
load_all("~/Dropbox/Code/RSSL")

modelform<-formula("factor(y)~.")
classname<-all.vars(modelform)[1]
D<-GenerateSlicedCookie(500,expected=FALSE)
D<-Generate2ClassGaussian(n=500,d=2,var=1)
D<-D[sample(nrow(D)),]
D[11:500,classname]<-rep(NA,490)
D_test<-GenerateSlicedCookie(10000,expected=FALSE)

res<-SSLDataFrameToMatrices(modelform,D,intercept=FALSE)
X<-res$X
X_u<-res$X_u
y<-res$y

res<-SSLDataFrameToMatrices(modelform,D_test,intercept=FALSE)
X_test<-res$X
y_test<-res$y

source("TestLeastSquaresClassifier.R")
source("TestICLeastSquaresClassifier.R")


source("TestNearestMeanClassifier.R")
source("TestMCNearestMeanClassifier.R")
source("TestLinearDiscriminantClassifier.R")
source("TestICLinearDiscriminantClassifier.R")
source("TestQuadraticDiscriminantClassifier.R")

source("TestLogisticRegression.R")
source("TestEntropyRegularizedLogisticRegression.R")
source("TestLogisticLossClassifier.R")
source("TestICLogisticLossClassifier.R")

source("TestSelfLearning.R")
source("TestSGDSVM.R")
