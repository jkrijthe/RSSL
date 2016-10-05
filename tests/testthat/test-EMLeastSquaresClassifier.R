context("Expectation Maximization Like Least Squares Classifier")

# Simple dataset used in the tests
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

set.seed(1)

test_that("Formula and matrix formulation give same results",{
  g_matrix <- EMLeastSquaresClassifier(X,factor(y),X_u)
  g_model <- EMLeastSquaresClassifier(modelform, D)
  
  expect_equal(predict(g_matrix,X_test),predict(g_model,D_test)) 
  expect_equal(loss(g_matrix, X_test, y_test),loss(g_model, D_test)) 
  expect_equal(g_matrix@classnames,g_model@classnames)
})

test_that("Different settings return the same loss",{
  g_1 <- EMLeastSquaresClassifier(X,y,X_u,intercept=TRUE,scale=TRUE)
  g_2 <- EMLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE,scale=TRUE)
  
  expect_equal(loss(g_1,X_test,y_test),loss(g_2,X_test,y_test),tolerance=10e-6)
  
  # We get a different loss when we center the output
  g_3<-EMLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE,scale=TRUE,y_scale=TRUE)
  g_4<-EMLeastSquaresClassifier(X,y,X_u,intercept=TRUE,scale=TRUE,x_center=TRUE,y_scale=TRUE)
  
  expect_equal(loss(g_3,X_test,y_test),loss(g_4,X_test,y_test),tolerance=10e-6)
})

test_that("Hard label EM and self-learning give the same result", {
  
  data <- generate2ClassGaussian(200,d=2, expected=FALSE)
  data <- add_missinglabels_mar(data, Class~., prob=0.9)
  data_test <- generate2ClassGaussian(200,d=2, expected=FALSE)
  
  g_block <-  EMLeastSquaresClassifier(Class~.,data,method="block",objective="responsibility",init="supervised",save_all=TRUE)
  
  g_self <- SelfLearning(Class~.,data,method=LeastSquaresClassifier)
  
  expect_equal(loss(g_block,data_test), loss(g_self,data_test))
  expect_equal(g_block@theta, g_self@model@theta)
})

test_that("Gradient superficially correct",{
  library("numDeriv")
  
  data(testdata)
  
  X <- cbind(1,testdata$X)
  X_u <- cbind(1,testdata$X_u)
  Xe <- rbind(X,X_u)
  Y <- model.matrix(~y-1,data.frame(y=testdata$y))[,1,drop=FALSE]
  
  for (i in 1:100) {
    w <- c(rnorm(ncol(X)),runif(nrow(X_u)))
    
    grad_num <- as.numeric(
      numDeriv::grad(
        loss_minmin_lsy,
        w, Xe=Xe, Y=Y, X_u=X_u,
        method="simple")
    )
    
    grad_exact <- as.numeric(
      gradient_minmin_lsy(
        w, Xe=Xe, Y=Y, X_u=X_u)
    )
    
    expect_equal(grad_num,grad_exact,
                 tolerance=10e-4)
  }
})

test_that("Gradient superficially correct",{
  library("numDeriv")
  
  data(testdata)
  
  X <- cbind(1,testdata$X)
  X_u <- cbind(1,testdata$X_u)
  Xe <- rbind(X,X_u)
  Y <- model.matrix(~y-1,data.frame(y=testdata$y))[,1,drop=FALSE]
  
  for (i in 1:100) {
    w <- c(rnorm(ncol(X)),runif(nrow(X_u)))
    
    grad_num <- as.numeric(
      numDeriv::grad(
        loss_minmin_lsq,
        w, Xe=Xe, Y=Y, X_u=X_u,X=X,
        method="simple")
    )
    
    grad_exact <- as.numeric(
      gradient_minmin_lsq(
        w, Xe=Xe, Y=Y, X_u=X_u,X=X)
    )
    
    expect_equal(grad_num,grad_exact,
                 tolerance=10e-4)
  }
})
