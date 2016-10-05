context("ERLogisticLossClassifier")

# Simple dataset used in the tests
data(testdata)
modelform <- formula(y ~ .)
classname<-all.vars(modelform)[1] 
D <- testdata$D
D_test <- testdata$D_test
X <- testdata$X
X_u <- testdata$X_u
y <- testdata$y
X_test <- testdata$X_test
y_test <- testdata$y_test

set.seed(1)

test_that("Same result for different input modalities",{
  g_mat <- EntropyRegularizedLogisticRegression(X,y,X_u)
  g_df <- EntropyRegularizedLogisticRegression(modelform,D)
  
  expect_equal(predict(g_mat,X_test),predict(g_df,D_test))
  expect_equal(loss(g_mat,X_test,y_test),loss(g_df,D_test))
})


test_that("Entropy Regularized LR is the same as LR when parameter is set to 0",{
  g_sup <- LogisticRegression(X,y,lambda=1)
  g_semi <- EntropyRegularizedLogisticRegression(X,y,X_u,lambda=1,lambda_entropy=0.0)
  
  expect_equal(g_sup@w,g_semi@w,tolerance=10e-6)
  expect_equal(loss(g_sup,X_test,y_test),
               loss(g_semi,X_test,y_test)
               ,tolerance=10e-7)
})

test_that("Gradient superficially correct",{
  library("numDeriv")

  X <- cbind(1,testdata$X)
  X_u <- cbind(1,testdata$X_u)
  y <- testdata$y
  classnames <- levels(y)
  
  for (i in 1:10) {
    w <- rnorm(ncol(X))
    lambda <- abs(10*rnorm(1))
    le <- abs(10*rnorm(1))
    
    grad_num <- as.numeric(
      numDeriv::grad(
        loss_erlr,
          w, X=X, y=y, X_u=X_u,
          classnames=classnames,
          lambda=lambda, 
          lambda_entropy=le,
        method="simple")
    )
    
    grad_exact <- as.numeric(
      grad_erlr(
        w, X=X, y=y, X_u=X_u,
        classnames=classnames,
        lambda=lambda,
        lambda_entropy=le)
    )
    
    expect_equal(grad_num,grad_exact,
                 tolerance=10e-4)
  }
})
