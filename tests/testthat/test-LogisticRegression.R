context("Logistic Regression")

library(LiblineaR)

# Simple dataset used in the tests
data(testdata)
modelform <- formula(y~.)
classname<-all.vars(modelform)[1] 
D <- testdata$D
D_test <- testdata$D_test
X <- testdata$X
X_u <- testdata$X_u
y <- testdata$y
X_test <- testdata$X_test
y_test <- testdata$y_test

test_that("Same result for different input modalities",{
  g_mat <- LogisticRegression(X,y)
  g_df <- LogisticRegression(modelform,D)
  
  expect_equal(predict(g_mat,X_test),predict(g_df,D_test))
  expect_equal(loss(g_mat,X_test,y_test),loss(g_df,D_test))
})

test_that("Centering input does not change predictions", {

  g_center <- LogisticRegression(X,y,
                                 x_center=TRUE,lambda=10)
  g_noncenter <- LogisticRegression(X,y,
                                    x_center=FALSE,lambda=10)
  
  expect_equal(g_center@w[-1],g_noncenter@w[-1],tolerance=10e-4)
  
  expect_equal(loss(g_center,X_test,y_test),
               loss(g_noncenter,X_test,y_test),
               tolerance=10e-5)
  expect_equal(predict(g_center,X_test),
               predict(g_noncenter,X_test))
  
  g_center <- LogisticRegression(X[,1,drop=FALSE],y,x_center=TRUE)
  g_noncenter <- LogisticRegression(X[,1,drop=FALSE],y,x_center=FALSE)
  
  expect_equal(loss(g_center,X_test[,1,drop=FALSE],y_test),
               loss(g_noncenter,X_test[,1,drop=FALSE],y_test),tolerance=10e-4)
  
  expect_equal(predict(g_center,X_test[,1,drop=FALSE]),
               predict(g_noncenter,X_test[,1,drop=FALSE]))
})

test_that("Gradient is superficially correct",{
  library("numDeriv")
  data(testdata)
  
  X <- cbind(1,testdata$X)
  y <- testdata$y
  classnames <- levels(y)
  
  for (i in 1:10) {
    w <- rnorm(ncol(X))
    lambda <- abs(10*rnorm(1))
    expect_equal(as.numeric(numDeriv::grad(loss_logisticregression,w,X=X,y=y,classnames=classnames,lambda=lambda, method="simple")),
                 as.numeric(grad_logisticregression(w,X=X,y=y,classnames=classnames,lambda=lambda)),
                 tolerance=10e-4)
  }
})

test_that("LogisticRegression gives same solution as other implementations",{
  
  # LiblineaR
  
  # Note LiblineaR penalizes the intercept!
  g_liblin <- LiblineaR(X,y,cost=0.5,epsilon = 0.000000001,bias=FALSE)
  g_lr <- LogisticRegression(X,y,lambda=1,intercept=FALSE)
  
  expect_equal(as.numeric(g_liblin$W), g_lr@w,tolerance=10e-5)
  expect_equivalent(predict(g_liblin,X_test)$predictions, predict(g_lr,X_test))
  
  # glmnet
  # g_glm <- glmnet(X,y,family="binomial",alpha=0,lambda=0.2,standardize=FALSE)
  # g_lr <- LogisticRegression(X,y,lambda=1)
  # expect_equal(as.numeric(coef(g_glm)),g_lr@w,tolerance=10e-7)
  
  # glm
  g_fast <- LogisticRegressionFast(X[,1,drop=FALSE],y)
  g_lr <- LogisticRegression(X[,1,drop=FALSE],y)
  expect_equal(as.numeric(g_fast@w), g_lr@w,tolerance=10e-4)

  expect_equal(predict(g_fast,X_test[,1,drop=FALSE]),
               predict(g_lr,X_test[,1,drop=FALSE]))
})
