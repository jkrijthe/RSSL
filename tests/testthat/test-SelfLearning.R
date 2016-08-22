context("Self-Learning")

data(testdata)

test_that("Formula and matrix formulation give same results", {
  g_matrix <- SelfLearning(testdata$X,testdata$y,testdata$X_u,method=LeastSquaresClassifier)
  g_model <- SelfLearning(testdata$modelform,testdata$D,method=LeastSquaresClassifier)
  
  # Same classification error?
  expect_that(1-mean(predict(g_matrix,testdata$X_test)==testdata$y_test),
              is_equivalent_to(1-mean(predict(g_model,testdata$D_test)==testdata$D_test[,testdata$classname]))) 
  # Same loss on test set?
  expect_that(loss(g_matrix, testdata$X_test, testdata$y_test),is_equivalent_to(loss(g_model, testdata$D_test))) 
  expect_that(g_matrix@classnames,is_equivalent_to(g_model@classnames)) # Class names the same?
})

test_that("Example where classifier should not change.", {
  X <- matrix(c(-1,+1),2,1)
  X_u <- X
  y <- factor(c(-1,+1))
  g_sup <- LeastSquaresClassifier(X,y)
  g_self <- SelfLearning(X,y,X_u,
                         method=LeastSquaresClassifier)

  expect_equal(loss(g_sup,X_u,y),
               loss(g_self,X_u,y))
  expect_equal(g_sup@theta,g_self@model@theta)
})


test_that("Self-learning decreases loss",{
  g_matrix <- SelfLearning(testdata$X,testdata$y,testdata$X_u,method=LeastSquaresClassifier)
  l_prev<-Inf
  for (i in seq_len(g_matrix@n_iter)) {
    g_iter <- SelfLearning(testdata$X,testdata$y,testdata$X_u,method=LeastSquaresClassifier,max_iter=i)
    if (i>1) {
      l_new <- mean(loss(g_matrix,rbind(testdata$X,testdata$X_u),
                         unlist(list(testdata$y,g_iter@i_labels[[i-1]]))))
      expect_true(l_new < l_prev)
      l_new <- l_prev
    }
    l_new <- mean(loss(g_matrix,rbind(testdata$X,testdata$X_u),
                       unlist(list(testdata$y,g_iter@i_labels[[i]]))))
    expect_true(l_new < l_prev)
    l_new <- l_prev
    
  }
      
})