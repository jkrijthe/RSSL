context("SVM and Linear SVM")

library(kernlab)

set.seed(91)

testdata <- generateSlicedCookie(50,expected=TRUE)
extra_testdata <- generateSlicedCookie(100,expected=TRUE)

g1 <- SVM(formula(Class~.), testdata, C=1000,eps=1e-10)
g2 <- LinearSVM(formula(Class~.), testdata, C=10000, method="Dual",eps=1e-10)
g3 <- LinearSVM(formula(Class~.), testdata, C=10000, method="Primal",eps=1e-10)

test_that("Batch Gradient Descent gives a warning", {
  expect_warning(g4 <- LinearSVM(formula(Class~.), testdata, C=500, method="BGD",reltol=1e-100, maxit=1000,eps=1e-10))
})

test_that("Same result as kernlab implementation", {
  g_nonscaled  <- SVM(formula(Class~.), testdata, C=1000,eps=1e-10,scale=FALSE)
  g_kernlab <- ksvm(formula(Class~.),data=testdata, C=1000,kernel=vanilladot(),scaled=FALSE)
  expect_equal(g_nonscaled@alpha[g_kernlab@alphaindex[[1]]],-g_kernlab@coef[[1]],tolerance=1e-2)
})

test_that("Same result as svmd implementation", {
  g_nonscaled  <- SVM(formula(Class~.), testdata, C=1,eps=1e-5,scale=FALSE,x_center=FALSE)
  g_kernlab <- svmd(formula(Class~.), kernel="linear", testdata,cost=1, scale = FALSE)

  expect_equal(g_nonscaled@alpha[g_kernlab$index],
                as.numeric(-g_kernlab$coefs),tolerance=10e-4)
})

test_that("Same result for SVM and Linear SVM.", {
  expect_equal(decisionvalues(g2,testdata),decisionvalues(g1,testdata),tolerance=1e-5)
  #expect_equal(decisionvalues(g3,testdata),decisionvalues(g1,testdata),tolerance=1e-1) # Problem on solaris
  #expect_equal(decisionvalues(g4,testdata),decisionvalues(g1,testdata),tolerance=1e-3) # BFGS does not always converge

})

test_that("Weights equal for Primal and Dual solution", {
  expect_equal(g2@w, as.numeric(g3@w),tolerance=10e-2,scale=1)
  #expect_equal(g3@w, as.numeric(g4@w),tolerance=10e-4,scale=1) # BFGS does not always converge
})

test_that("Predictions the same for SVM and LinearSVM",{
  expect_equal(predict(g2,extra_testdata), predict(g1,extra_testdata))
  expect_equal(predict(g3,extra_testdata), predict(g1,extra_testdata))
  #expect_equal(predict(g4,extra_testdata), predict(g1,extra_testdata)) # BFGS does not always converge
})

test_that("Loss functions return the same value for SVM and LinearSVM",{
  expect_equal(loss(g2,extra_testdata), loss(g1,extra_testdata),tolerance=1e-4,scale=1)
  #expect_equal(loss(g3,extra_testdata), loss(g1,extra_testdata),tolerance=1e-1,scale=1) # Problem on solaris
  #expect_equal(loss(g4,extra_testdata), loss(g1,extra_testdata),tolerance=1e-3,scale=1) # BFGS does not always converge
  
  l1 <- loss(g1,testdata)
  expect_equal(l1,loss(g2,testdata),tolerance=1e-3,scale=1)
  #expect_equal(l1,loss(g3,testdata),tolerance=1e-1,scale=1) # Problem on solaris
  #expect_equal(l1,loss(g4,testdata),tolerance=1e-3,scale=1) # BFGS does not always converge
})

test_that("Gradient is superficially correct",{
  library("numDeriv")
  data(testdata)
  
  X <- cbind(1,testdata$X)
  y <- as.numeric(testdata$y)*2-3 
  w <- rnorm(ncol(X))
  C <- 500
  expect_equal(as.numeric(numDeriv::grad(RSSL:::svm_opt_func,w,X=X,y=y,C=C, method="simple")),as.numeric(RSSL:::svm_opt_grad(w,X=X,y=y,C=C)),tolerance=1e-2)
})
