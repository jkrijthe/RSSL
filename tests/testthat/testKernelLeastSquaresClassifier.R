context("Kernelized Least Squares Classifier")

test_that("Kernel and Linear give same result: 2 class", {
  dmat<-model.matrix(Species~.-1,iris[51:150,])
  tvec<-droplevels(iris$Species[51:150])
  testdata <- data.frame(tvec,dmat[,1:2])
  colnames(testdata)<-c("Class","X1","X2")
  
  g_kernel<-KernelLeastSquaresClassifier(dmat[,1:2],tvec,kernel=vanilladot(),lambda=0.000001,scale = TRUE)
  g_linear<-LeastSquaresClassifier(dmat[,1:2],tvec,intercept=TRUE)
  expect_equal(predict(g_kernel,dmat[,1:2]),  predict(g_linear,dmat[,1:2]))
  expect_equal(loss(g_kernel,dmat[,1:2],tvec),  loss(g_linear,dmat[,1:2],tvec),tolerance =10e-6)
})

test_that("Kernel and Linear give same result: 3 class", {
  dmat<-model.matrix(Species~.-1,iris)
  tvec<-iris$Species
  testdata <- data.frame(tvec,dmat[,1:2])
  colnames(testdata)<-c("Class","X1","X2")
  
  g_kernel<-KernelLeastSquaresClassifier(dmat[,1:2],tvec,kernel=vanilladot(),lambda=0.000001,scale = TRUE)
  g_linear<-LeastSquaresClassifier(dmat[,1:2],tvec,intercept=TRUE)
  expect_equal(predict(g_kernel,dmat[,1:2]),  predict(g_linear,dmat[,1:2]))
  expect_equal(loss(g_kernel,dmat[,1:2],tvec),  loss(g_linear,dmat[,1:2],tvec),tolerance =10e-6)
})

 