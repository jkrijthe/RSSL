context("Helper functions")

test_that("sample_k_per_level works", {
  y<-factor(sample(letters[1:5],300,replace=TRUE))
  expect_equal(y[sample_k_per_level(y,1)], factor(letters[1:5]))
})

test_that("PreProcessingPredict does not drop unlabeled objects", {
  df <- generate2ClassGaussian(n=100)
  out1 <- PreProcessingPredict(Class~.,df)
  df2 <- df %>% add_missinglabels_mar(Class~.,0.8)
  out2 <- PreProcessingPredict(Class~.,df2)
  expect_equal(out1$X, out2$X)
})
