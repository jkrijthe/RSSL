context("svmlin")

library(LiblineaR)
library(Matrix)
i <- c(1,3:8); j <- c(2,9,6:10); x <- 7 * (1:7)
(A <- sparseMatrix(i, j, x = x,giveCsparse = TRUE))

t_svmlin <- svmlin(A,factor(rep(c(1,-1),4)),X_u=NULL, lambda = 1,algorithm=1);
t_liblin <- LiblineaR(as.matrix(A),rep(c(1,-1),4),type=1,epsilon=0.0001,cost=0.5)

test_that("svmlin and LiblineaR give the same results", {
  expect_equal(t_svmlin@weights, as.numeric(t_liblin$W),tolerance=10e-4)
  expect_equal(decisionvalues(t_svmlin,A),predict(t_liblin,as.matrix(A),decisionValues = TRUE)$decisionValues[,1],tolerance=10e-4)
  expect_equal(predict(t_svmlin,A),as.factor(predict(t_liblin,as.matrix(A))$predictions),tolerance=10e-4)
})

test_that("svmlin does not throw errors", {
  t_svmlin <- svmlin(A,factor(rep(c(1,-1),4)),X_u=NULL, lambda = 1,algorithm=0)
  t_svmlin <- svmlin(A,factor(rep(c(1,-1),4)),X_u=A, lambda = 1,algorithm=1)
  t_svmlin <- svmlin(A,factor(rep(c(1,-1),4)),X_u=A, lambda = 1,algorithm=2)
  t_svmlin <- svmlin(A,factor(rep(c(1,-1),4)),X_u=A, lambda = 1,algorithm=3)
})

# Simple dataset used in the tests
data(testdata)
modelform <- testdata$modelform
classname<-all.vars(modelform)[1] 
D <- testdata$D
D_test <- testdata$D_test
X <- testdata$X
X_u <- testdata$X_u
y <- testdata$y
X_test <- testdata$X_test
y_test <- testdata$y_test

test_that("Formula and matrix formulation give same results",{
  g_matrix <- svmlin(X,y,X_u,algorithm=2)
  g_model <- svmlin(modelform, D,algorithm=2)
  
  expect_equal(predict(g_matrix,X_test),
               predict(g_model,D_test))
  expect_equal(loss(g_matrix, X_test, y_test),
               loss(g_model, D_test))
  expect_equal(g_matrix@weights,g_model@weights)
  expect_equal(g_matrix@classnames,g_model@classnames)
  
  g_matrix <- svmlin(X,y,X_u,algorithm=2, scale=TRUE)
  g_model <- svmlin(modelform, D,algorithm=2, scale=TRUE)
  
  expect_equal(predict(g_matrix,X_test),
               predict(g_model,D_test))
  expect_equal(loss(g_matrix, X_test, y_test),
               loss(g_model, D_test))
  expect_equal(g_matrix@weights,g_model@weights)
  expect_equal(g_matrix@classnames,g_model@classnames)
})


test_that("Old tests",{
  FALSE
  library(testthat)
  
  data(testdata)
  modelform <- testdata$modelform
  classname<-all.vars(modelform)[1] 
  D <- testdata$D
  D_test <- testdata$D_test
  X <- testdata$X
  X_u <- testdata$X_u
  y <- testdata$y
  X_test <- testdata$X_test
  y_test <- testdata$y_test
  
  
  g_binary <- SVMlin(X,y,X_u,
                     binary_path="/Users/jkrijthe/Dropbox/Code/ssl-survey/svmlin/",
                     temp_path = "~/Downloads/",
                     type=0)
  g_direct <- svmlin(X,y,X_u,algorithm=0,intercept=TRUE,verbose=TRUE,pos_frac=prop.table(table(y))[2])
  expect_equal(g_direct@weights,read.delim("~/Downloads/tempfile.train.weights",header = FALSE)$V1,tolerance=10e-6)
  expect_equal(predict(g_binary,X_test),decisionvalues(g_direct,X_test),tolerance=10e-6)
  
  g_binary <- SVMlin(X,y,X_u,
                     binary_path="/Users/jkrijthe/Dropbox/Code/ssl-survey/svmlin/",
                     temp_path = "~/Downloads/",
                     type=0)
  g_direct <- svmlin(X,y,X_u,algorithm=0,intercept=TRUE,verbose=TRUE)
  expect_equal(g_direct@weights,read.delim("~/Downloads/tempfile.train.weights",header = FALSE)$V1,tolerance=10e-6)
  expect_equal(predict(g_binary,X_test),decisionvalues(g_direct,X_test),tolerance=10e-6)
  
  g_binary <- SVMlin(X,y,X_u,
                     binary_path="/Users/jkrijthe/Dropbox/Code/ssl-survey/svmlin/",
                     temp_path = "~/Downloads/",
                     type=1)
  g_direct <- svmlin(X,y,X_u,algorithm=1,intercept=TRUE,verbose=TRUE)
  expect_equal(g_direct@weights,read.delim("~/Downloads/tempfile.train.weights",header = FALSE)$V1,tolerance=10e-6)
  expect_equal(predict(g_binary,X_test),decisionvalues(g_direct,X_test),tolerance=10e-6)
  
  g_binary <- SVMlin(X,y,X_u,
                     binary_path="/Users/jkrijthe/Dropbox/Code/ssl-survey/svmlin/",
                     temp_path = "~/Downloads/",
                     type=2)
  g_direct <- svmlin(X,y,X_u,algorithm=2,intercept=TRUE,verbose=TRUE,pos_frac=prop.table(table(y))[2])
  expect_equal(g_direct@weights,read.delim("~/Downloads/tempfile.train.weights",header = FALSE)$V1,tolerance=10e-6)
  expect_equal(predict(g_binary,X_test),decisionvalues(g_direct,X_test),tolerance=10e-6)
  
  g_binary <- SVMlin(X,y,X_u,
                     binary_path="/Users/jkrijthe/Dropbox/Code/ssl-survey/svmlin/",
                     temp_path = "~/Downloads/",
                     type=3)
  g_direct <- svmlin(X,y,X_u,algorithm=3,intercept=TRUE,verbose=TRUE,pos_frac=prop.table(table(y))[2])
  expect_equal(g_direct@weights,read.delim("~/Downloads/tempfile.train.weights",header = FALSE)$V1,tolerance=10e-6)
  expect_equal(predict(g_binary,X_test),decisionvalues(g_direct,X_test),tolerance=10e-6)
  
  
  
})