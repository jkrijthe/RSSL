context("Gaussian Random Field Classifier")

# testdata <- generateTwoCircles(1000,0.1)
# testdata[-sample(1:nrow(testdata),10),]$Class <- NA
# tvec<-na.omit(testdata$Class)
# dmatU <- as.matrix(testdata[is.na(testdata$Class),1:2])
# dmat <- as.matrix(testdata[!is.na(testdata$Class),1:2])
# 
# expect_equivalent(GRFClassifier(dmat, tvec, dmatU)@unlabels,
#                   GRFClassifier(dmat, tvec, dmatU, adjacency_kernel = rbfdot(10))@unlabels)
# # Simple dataset used in the tests
# data(testdata)
# 
# # #Test Different input schemes
# GRFClassifier(dmat,tvec,dmatU)
# data <- generateTwoCircles(200, 0.05) 
# Xin <- as.matrix(data[,1:2])
# yin <- model.matrix(~y-1,data)[1:2,1,drop=FALSE]
# W <- exp(-as.matrix(dist(Xin))^2/0.1)
# 
# all(harmonic_function(W,yin)==harmonic_function(W,yin))
# 
# library(microbenchmark)
# microbenchmark(harmonic_function(W,yin),harmonic_function_cpp(W,yin))
