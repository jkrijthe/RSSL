context("Kernelized Least Squares Classifier")

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
  dmat<-model.matrix(Species~.-1,iris[51:150,])
  tvec<-droplevels(iris$Species[51:150])
  testdata <- data.frame(tvec,dmat[,1:4])
  colnames(testdata)<-c("Class","X1","X2")
  
  g_kernel<-KernelLeastSquaresClassifier(dmat[,1:4],tvec,kernel=kernlab::vanilladot(),lambda=0.000001,scale = TRUE)
  g_linear<-LeastSquaresClassifier(dmat[,1:4],tvec,intercept=TRUE)
  expect_equal(predict(g_kernel,dmat[,1:4]),  predict(g_linear,dmat[,1:4]))
  expect_equal(loss(g_kernel,dmat[,1:4],tvec),  loss(g_linear,dmat[,1:4],tvec),tolerance =10e-6)
})

test_that("Kernel and Linear give same result: 3 class", {
  dmat<-model.matrix(Species~.-1,iris)
  tvec<-iris$Species
  testdata <- data.frame(tvec,dmat[,1:2])
  colnames(testdata)<-c("Class","X1","X2")
  
  g_kernel <- KernelLeastSquaresClassifier(dmat[,1:2],tvec,
                kernel=kernlab::vanilladot(),lambda=0.000001,scale = TRUE)
  g_linear <- LeastSquaresClassifier(dmat[,1:2],tvec,intercept=TRUE)
  
  expect_equal(as.numeric(decisionvalues(g_kernel,dmat[,1:2])),  
                    as.numeric(decisionvalues(g_linear,dmat[,1:2])),
                    tolerance=10e-7)
  
  expect_equal(predict(g_kernel,dmat[,1:2]),  
               predict(g_linear,dmat[,1:2]))
  
  expect_equal(loss(g_kernel,dmat[,1:2],tvec),
               loss(g_linear,dmat[,1:2],tvec),tolerance =10e-6)
})

test_that("Linear and Kernel implementation give the same answer.", {
  # These should all be the same
  g_1<-LeastSquaresClassifier(X,y,intercept=TRUE,scale=TRUE)
  s1 <- sum(loss(g_1,X_test,y_test))
  g_2<-LeastSquaresClassifier(X,y,intercept=TRUE,x_center=TRUE,scale=TRUE)
  expect_equal(s1, sum(loss(g_2,X_test,y_test)))
  g_3<-LeastSquaresClassifier(X,y,intercept=TRUE,x_center=TRUE,scale=TRUE,y_scale=TRUE)
  expect_equal(s1, sum(loss(g_3,X_test,y_test)))
  g_4<-LeastSquaresClassifier(X,y,intercept=TRUE,scale=TRUE,x_center=TRUE,y_scale=FALSE)
  expect_equal(s1, sum(loss(g_4,X_test,y_test)))
  g_5<-LeastSquaresClassifier(X,y,intercept=FALSE,scale=TRUE,x_center=TRUE,y_scale=TRUE)
  expect_equal(s1, sum(loss(g_5,X_test,y_test)))
  g_6<-KernelLeastSquaresClassifier(X,y,lambda=0.00001,scale=TRUE,x_center=TRUE,y_scale=TRUE)
  expect_equal(s1, sum(loss(g_6,X_test,y_test)),tolerance=10e-3)
  
  # No y scaling, no intercept should give the same bad answer
  g_1<-LeastSquaresClassifier(X,y,intercept=FALSE,scale=TRUE,x_center=TRUE,y_scale=FALSE)
  g_2<-KernelLeastSquaresClassifier(X,y,lambda=0.000000001,scale=TRUE,x_center=TRUE,y_scale=FALSE)
  expect_equal(sum(loss(g_1,X_test,y_test)),sum(loss(g_2,X_test,y_test)))
})
 