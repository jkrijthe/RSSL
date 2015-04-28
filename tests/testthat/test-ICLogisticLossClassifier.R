context("ICLogisticLoss Classifier")

test_that("Formula and matrix formulation give same results",{
  g_matrix <- ICLogisticLossClassifier(X,y,X_u,lambda1=0.0000001,lambda2=0.0000001)
  g_model <- ICLogisticLossClassifier(modelform,D,lambda1=0.0000001,lambda2=0.0000001)
  expect_equal(predict(g_matrix,X_test), predict(g_model,D_test))
  expect_equal(loss(g_matrix, X_test, y_test),loss(g_model, D_test))
})

test_that("Regularized class posteriors", {
  g<-ICLogisticLossClassifier(X,y,X_u,lambda1=0.01,lambda2=0.01,lambda3=0.01,trueprob=mean(y_test)-1)
  cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
  cat("Loss:  ",mean(loss(g, X_test, y_test)),"\n")
  
  g<-ICLogisticLossClassifier(X,y,X_u,lambda1=0.01,lambda2=0.01,lambda3=0.001,trueprob=NULL)
  cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
  cat("Loss:  ",mean(loss(g, X_test, y_test)),"\n")
})
