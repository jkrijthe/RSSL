context("Helper functions")

test_that("sample_k_per_level works", {
  y<-factor(sample(letters[1:5],300,replace=TRUE))
  expect_equal(y[sample_k_per_level(y,1)], factor(letters[1:5]))
})

test_that("PreProcessingPredict does not drop unlabeled objects", {
  df <- generate2ClassGaussian(n=100)
  formula_trans <-  terms(formula(Class~.),data=df)
  out1 <- PreProcessingPredict(formula_trans,df)
  df2 <- df %>% add_missinglabels_mar(Class~.,0.8)
  out2 <- PreProcessingPredict(formula_trans,df2)
  expect_equal(out1$X, out2$X)
})

test_that("logsumexp gives reasonable results",{
  X <- matrix(runif(1000)*200,100,10)
  expect_equal(log(rowSums(exp(X))),as.numeric(logsumexp(X)))
})

test_that("rowMax gives correct result",{
  X <- matrix(runif(10000)*200,100,100)
  expect_equal(as.numeric(rowMax(X)),apply(X,1,max))
  expect_equal(as.numeric(which_rowMax(X)),apply(X,1,which.max))
  expect_equal(as.numeric(which_rowMax2(X)),apply(X,1,which.max))
  # # Some benchmarking
  # library(microbenchmark)
  # microbenchmark(rowMax(X),rowMax2(X), apply(X,1,max),
  #                which_rowMax(X), apply(X,1,which.max),which_rowMax2(X))
})
