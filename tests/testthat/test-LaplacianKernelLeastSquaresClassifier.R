context("Laplacian Kernel Least Squares Classifier")

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

test_that("Laplacian with gamma almost 0 and supervised give the same result", {
  #Scaling needs to be turned off, otherwise scaling is done differently for the supervised and semi-supervised method.
  g_lap <- LaplacianKernelLeastSquaresClassifier(X,y,X_u,
                                                 kernel=kernlab::vanilladot(),
                                                 gamma=10e-8,
                                                 lambda=0.0000001, 
                                                 scale = FALSE,y_scale=TRUE,
                                                 x_center=FALSE)
  
  g_sup <- KernelLeastSquaresClassifier(X,y, 
                                        lambda=0.0000001,
                                        scale=FALSE,x_center=FALSE,
                                        y_scale=TRUE)
  sum(loss(g_sup,X_test,y_test))
  sum(loss(g_lap,X_test,y_test))
  expect_equal(predict(g_lap,X_test),  predict(g_sup,X_test))
  expect_equal(loss(g_lap,X_test,y_test),  loss(g_sup,X_test,y_test),tolerance =10e-5)
})
