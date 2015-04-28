context("Implicitly Constrained Kernelized Least Squares Classifier")

data(testdata)
modelform <- testdata$modelform
classname<-all.vars(modelform)[1] 
D <- testdata$D
D_test <- testdata$D_test
X <- testdata$X
X_u <- testdata$X_u
y <- testdata$y
X_test <- testdata$X_test
y_test <- testdata$y_test

test_that("Kernel and Linear give same result: 2 class", {
  g_kernel<-ICKernelLeastSquaresClassifier(X,y,X_u,kernel=vanilladot(),lambda=0.000001,scale = TRUE)
  g_linear<-ICLeastSquaresClassifier(X,y,X_u,intercept=TRUE)
  expect_equal(predict(g_kernel,X_test),  predict(g_linear,X_test))
  expect_equal(loss(g_kernel,X_test,y_test),  loss(g_linear,X_test,y_test),tolerance =10e-6)
})
