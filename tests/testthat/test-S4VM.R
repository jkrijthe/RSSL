context("S4VM")

data(wdbc)

test_that("S4VM gives the same accuracy as the Matlab implementation in an example.",{
  set.seed(1)
  out <- S4VM(X=wdbc$X[wdbc$idxLabs[1,],,drop=FALSE],
       y=factor(wdbc$y[wdbc$idxLabs[1,],,drop=FALSE]),
       X_u=wdbc$X[wdbc$idxUnls[1,],,drop=FALSE],
       gamma=0)
  expect_equal(mean(out@predictions==factor(wdbc$y[wdbc$idxUnls[1,],,drop=FALSE])),0.85509839,tolerance=10e-6)
  
  # Extra tests: take too long to run
  # out <- S4VM(X=wdbc$X[wdbc$idxLabs[1,],,drop=FALSE],
  #             y=factor(wdbc$y[wdbc$idxLabs[1,],,drop=FALSE]),
  #             X_u=wdbc$X[wdbc$idxUnls[1,],,drop=FALSE],
  #             gamma=1.8631)
  # expect_equal(mean(out@predictions==factor(wdbc$y[wdbc$idxUnls[1,],,drop=FALSE])),0.83005367,tolerance=10e-6)
  # 
  # out <- S4VM(X=wdbc$X[wdbc$idxLabs[2,],,drop=FALSE],
  #             y=factor(wdbc$y[wdbc$idxLabs[2,],,drop=FALSE]),
  #             X_u=wdbc$X[wdbc$idxUnls[2,],,drop=FALSE],
  #             gamma=0)
  # expect_equal(mean(out@predictions==factor(wdbc$y[wdbc$idxUnls[2,],,drop=FALSE])),0.91055456,tolerance=10e-6)
  
})
