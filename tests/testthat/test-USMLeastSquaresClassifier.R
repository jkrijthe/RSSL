context("Updated Second Moment Least Squares Classifier")

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

test_that("Formula and matrix formulation give same results",{
  g_matrix <- USMLeastSquaresClassifier(X,factor(y),X_u)
  g_model <- USMLeastSquaresClassifier(modelform, D)
  
  expect_equal(predict(g_matrix,X_test),predict(g_model,D_test)) 
  expect_equal(loss(g_matrix, X_test, y_test),loss(g_model, D_test)) 
  expect_equal(g_matrix@classnames,g_model@classnames)
})

test_that("Different settings return the same loss",{
  g_1<-USMLeastSquaresClassifier(X,y,X_u,intercept=TRUE,scale=TRUE)
  g_2<-USMLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE,scale=TRUE)
  
  expect_equal(loss(g_1,X_test,y_test),loss(g_2,X_test,y_test),tolerance=10e-6)
  
  # We get a different loss when we center the output
  g_3<-USMLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE,scale=TRUE,y_scale=TRUE)
  g_4<-USMLeastSquaresClassifier(X,y,X_u,intercept=TRUE,scale=TRUE,x_center=TRUE,y_scale=TRUE)
  
  expect_equal(loss(g_3,X_test,y_test),loss(g_4,X_test,y_test),tolerance=10e-6)
})
