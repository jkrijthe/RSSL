context("svmlin")

library(LiblineaR)
library(Matrix)
i <- c(1,3:8); j <- c(2,9,6:10); x <- 7 * (1:7)
(A <- sparseMatrix(i, j, x = x,giveCsparse = TRUE))

t_svmlin <- svmlin(A,factor(rep(c(1,-1),4)),Xu=NULL, lambda = 1,algorithm=1);
t_liblin <- LiblineaR(as.matrix(A),rep(c(1,-1),4),type=1,epsilon=0.0001,cost=0.5)

test_that("svmlin and LiblineaR give the same results", {
  expect_equal(t_svmlin$Weights[-1], t_liblin$W[-length(t_liblin$W)])
  expect_equal(decisionvalues.svmlin(t_svmlin,A),predict(t_liblin,as.matrix(A),decisionValues = TRUE)$decisionValues[,1])
  expect_equal(predict(t_svmlin,A),as.factor(predict(t_liblin,as.matrix(A))$predictions))
})
