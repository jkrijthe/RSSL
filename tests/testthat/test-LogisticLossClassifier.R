context("Logistic Loss Classifier")

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

test_that("Same result for different input modalities",{
  g_mat <- LogisticLossClassifier(X,y)
  g_df <- LogisticLossClassifier(modelform,D)
  
  expect_equal(predict(g_mat,X_test),predict(g_df,D_test))
  expect_equal(loss(g_mat,X_test,y_test),loss(g_df,D_test))
})

test_that("Centering input does not change predictions", {
  
  g_center <- LogisticLossClassifier(X,y,
                                 x_center=TRUE,lambda=10)
  g_noncenter <- LogisticLossClassifier(X,y,
                                    x_center=FALSE,lambda=10)
  
  expect_equal(g_center@w[-1],g_noncenter@w[-1],tolerance=10e-4)
  
  expect_equal(loss(g_center,X_test,y_test),
               loss(g_noncenter,X_test,y_test),
               tolerance=10e-5)
  expect_equal(predict(g_center,X_test),
               predict(g_noncenter,X_test))
  
  g_center <- LogisticLossClassifier(X[,1,drop=FALSE],y,x_center=TRUE)
  g_noncenter <- LogisticLossClassifier(X[,1,drop=FALSE],y,x_center=FALSE)
  
  expect_equal(loss(g_center,X_test[,1,drop=FALSE],y_test),
               loss(g_noncenter,X_test[,1,drop=FALSE],y_test),tolerance=10e-4)
  
  expect_equal(predict(g_center,X_test[,1,drop=FALSE]),
               predict(g_noncenter,X_test[,1,drop=FALSE]))
})


test_that("Gradient is superficially correct",{
  library("numDeriv")
  data(testdata)
  
  X <- cbind(1,testdata$X)
  y <- as.numeric(testdata$y)*2-3
  lambda <- abs(10*rnorm(1))
  
  for (i in 1:10) {
    w <- rnorm(ncol(X))
    lambda <- abs(10*rnorm(1))
    expect_equal(as.numeric(numDeriv::grad(RSSL:::loss_logistic,w,X=X,y=y,lambda=lambda, method="simple")),
                 as.numeric(RSSL:::grad_logistic(w,X=X,y=y,lambda)),
                 tolerance=10e-4)
  }
})

test_that("Gives the same result as LogisticRegression", {
  g <- LogisticLossClassifier(X[,1,drop=FALSE],y)
  g_lr <- LogisticRegression(X[,1,drop=FALSE],y)
  
  expect_equal(g@w,-g_lr@w)
  expect_equivalent(predict(g,X_test[,1,drop=FALSE]),
                    predict(g_lr,X_test[,1,drop=FALSE]))
  expect_equal(as.numeric(loss(g_lr, X_test[,1,drop=FALSE], y_test)),
               as.numeric(loss(g, X_test[,1,drop=FALSE], y_test)),
               tolerance=10e-5)
  
})
