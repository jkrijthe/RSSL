context("SVM and Linear SVM")

testdata <- GenerateSlicedCookie(100,expected=FALSE)
extra_testdata <- GenerateSlicedCookie(100,expected=TRUE)

g1 <- SVM(formula(Class~.), testdata, C=1000, method="Dual",eps=1e-10)
g2 <- LinearSVM(formula(Class~.), testdata, C=1000, method="Dual",eps=1e-10)
g3 <- LinearSVM(formula(Class~.), testdata, C=1000, method="Primal",eps=1e-10)
g4 <- LinearSVM(formula(Class~.), testdata, C=1000, method="BGD")

test_that("Same result as kernlab implementation", {
  g_nonscaled  <- SVM(formula(Class~.), testdata, C=1000, method="Dual",eps=1e-10,scale=FALSE)
  g_kernlab <- ksvm(formula(Class~.),data=testdata, C=1000,kernel=vanilladot(),scaled=FALSE)
  expect_equal(g_nonscaled@alpha[g_kernlab@alphaindex[[1]]],-g_kernlab@coef[[1]],tolerance=1e-3)
})

test_that("Same result for SVM and Linear SVM.", {
  expect_equal(decisionvalues(g2,testdata),decisionvalues(g1,testdata),tolerance=1e-5)
  expect_equal(decisionvalues(g3,testdata),decisionvalues(g1,testdata),tolerance=1e-5)
  expect_equal(decisionvalues(g4,testdata),decisionvalues(g1,testdata),tolerance=1e-4)
})

test_that("Weights equal for BGD and Dual solution", {
  expect_equal(g2@w, as.numeric(g3@w),tolerance=1e-5)
  expect_equal(g3@w, as.numeric(g4@w))
})

test_that("Predictions the same for SVM and LinearSVM",{
  expect_equal(predict(g2,extra_testdata), predict(g1,extra_testdata))
  expect_equal(predict(g3,extra_testdata), predict(g1,extra_testdata))
  expect_equal(predict(g4,extra_testdata), predict(g1,extra_testdata))
})

test_that("Loss functions return the same value for SVM and LinearSVM",{
  expect_equal(loss(g2,extra_testdata), loss(g1,extra_testdata),tolerance=1e-5)
  expect_equal(loss(g3,extra_testdata), loss(g1,extra_testdata),tolerance=1e-5)
  expect_equal(loss(g4,extra_testdata), loss(g1,extra_testdata),tolerance=1e-5)
})

test_that("Gradient is superficially correct",{
  library("numDeriv")
  data(testdata)
  
  X <- cbind(1,testdata$X)
  y <- as.numeric(testdata$y)*2-3 
  w <- rnorm(ncol(X))
  C <- 100
  expect_equal(as.numeric(numDeriv::grad(svm_opt_func,w,X=X,y=y,C=C, method="simple")),as.numeric(svm_opt_grad(w,X=X,y=y,C=C)),tolerance=1e-5)
})
