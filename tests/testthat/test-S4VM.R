context("S4VM")

data(wdbc)

test_that("S4VM gives the same accuracy as the Matlab implementation in an example.",{
  set.seed(1)
  out <- S4VM(X=wdbc$X[wdbc$idxLabs[1,],,drop=FALSE],
       y=factor(wdbc$y[wdbc$idxLabs[1,],,drop=FALSE]),
       X_u=wdbc$X[wdbc$idxUnls[1,],,drop=FALSE],
       gamma=0)
  expect_equal(mean(out@predictions==factor(wdbc$y[wdbc$idxUnls[1,],,drop=FALSE])),0.8551,tolerance=10e-6)
})
