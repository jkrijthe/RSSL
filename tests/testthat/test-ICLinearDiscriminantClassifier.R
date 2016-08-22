context("ICLinearDiscriminantClassifier")

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

#Test Different input schemes
test_that("Formula and matrix formulation give same results",{
  g_matrix <- ICLinearDiscriminantClassifier(X,y,X_u)
  g_model <- ICLinearDiscriminantClassifier(modelform,D)
  expect_equal(predict(g_matrix,X_test), predict(g_model,D_test))
  expect_equal(loss(g_matrix, X_test, y_test),loss(g_model, D_test))
  expect_equal(mean(loss(g_matrix, X_test, y_test)),4.03527826)
})

test_that("Gradient superficially correct",{
  library("numDeriv")
  
  data(testdata)
  
  X <- cbind(testdata$X)
  X_u <- cbind(testdata$X_u)
  Xe <- rbind(X,X_u)
  Y <- model.matrix(~y-1,data.frame(y=testdata$y))
  
  for (i in 1:10) {
    w <- c(runif(nrow(X_u)))
    
    grad_num <- as.numeric(
      numDeriv::grad(
        loss_iclda,
        w, X=X, Y=Y, X_u=X_u,
        method="simple")
    )
    
    grad_exact <- as.numeric(
      gradient_iclda(
        w, X=X, Y=Y, X_u=X_u)
    )
    
    expect_equal(grad_num,grad_exact,
                 tolerance=10e-4)
  }
})