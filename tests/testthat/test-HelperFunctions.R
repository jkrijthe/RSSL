context("Helper functions")

test_that("sample_k_per_level works", {
  y<-factor(sample(letters[1:5],300,replace=TRUE))
  expect_equal(y[sample_k_per_level(y,1)], factor(letters[1:5]))
})