context("svmlin")

library(LiblineaR)
library(Matrix)
i <- c(1,3:8); j <- c(2,9,6:10); x <- 7 * (1:7)
(A <- sparseMatrix(i, j, x = x,giveCsparse = TRUE))

t_svmlin <- svmlin(A,factor(rep(c(1,-1),4)),Xu=NULL, lambda = 1,algorithm=1);
t_liblin <- LiblineaR(as.matrix(A),rep(c(1,-1),4),type=1,epsilon=0.0001,cost=0.5)

test_that("svmlin and LiblineaR give the same results", {
  expect_equal(t_svmlin@weights[-1], t_liblin$W[-length(t_liblin$W)],tolerance=10e-4)
  expect_equal(decisionvalues(t_svmlin,A),predict(t_liblin,as.matrix(A),decisionValues = TRUE)$decisionValues[,1],tolerance=10e-4)
  expect_equal(predict(t_svmlin,A),as.factor(predict(t_liblin,as.matrix(A))$predictions),tolerance=10e-4)
})

test_that("svmlin does not throw errors", {
  t_svmlin <- svmlin(A,factor(rep(c(1,-1),4)),Xu=NULL, lambda = 1,algorithm=0)
  t_svmlin <- svmlin(A,factor(rep(c(1,-1),4)),Xu=A, lambda = 1,algorithm=1)
  t_svmlin <- svmlin(A,factor(rep(c(1,-1),4)),Xu=A, lambda = 1,algorithm=2)
  t_svmlin <- svmlin(A,factor(rep(c(1,-1),4)),Xu=A, lambda = 1,algorithm=3)
})