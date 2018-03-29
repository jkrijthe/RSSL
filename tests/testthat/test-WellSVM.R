context("WellSVM")

# Test Equivalence
data(diabetes)

test_that("Implementation gives same result as Matlab implementation",{
  acc <- RSSL:::wellsvm_direct(rbind(diabetes$data[diabetes$idxLabs[1,],],diabetes$data[diabetes$idxUnls[1,],]),
        rbind(diabetes$target[diabetes$idxLabs[1,],,drop=FALSE],matrix(0,length(diabetes$idxUnls[1,]),1)),
        diabetes$data[diabetes$idxTest[1,],],
        diabetes$target[diabetes$idxTest[1,],,drop=FALSE],
        C = 1,C2=0.1,gamma=1)$accuracy
  expect_equal(acc, 0.7864583,tolerance=10e-6)
})

test_that("svmd does not throw an error",{
  library(RSSL)
  library(kernlab)
  K <- kernelMatrix(rbfdot(),as.matrix(iris[1:100,1:2]))
  RSSL:::svmd(iris[1:100,1:2],y=iris$Species[1:100],fitted=FALSE)
  expect_silent(RSSL:::svmd(K,y=iris$Species[1:100],type="one-classification"))
})

test_that("WellSVM interface result equal to direct result",{
  data <- iris[1:100,]
  x <- as.matrix(data[,1:3])
  y <- model.matrix(~Species,data)[,2]*2-1
  g_train <- WellSVM(x,factor(y),x,gamma=1,x_center=TRUE)
  expect_equal(predict(g_train,x),
             factor(RSSL:::wellsvm_direct(rbind(x,x),
                            rbind(matrix(y,ncol=1),
                                  matrix(0,nrow=nrow(x))),
                            x,y,gamma=1)$prediction))
})
