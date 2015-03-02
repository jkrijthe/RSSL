context("SVM and Linear SVM")

testdata <- GenerateSlicedCookie(100,expected=FALSE)
extra_testdata <- GenerateSlicedCookie(100,expected=TRUE)

g1 <- SVM(formula(Class~.), testdata, C=1000, method="Dual",eps=1e-10)
g2 <- LinearSVM(formula(Class~.), testdata, C=1000, method="Dual",eps=1e-10)
g3 <- LinearSVM(formula(Class~.), testdata, C=1000, method="Primal",eps=1e-10)
# g4 <- LinearSVM(formula(Class~.), testdata, C=1000, method="BGD")
# g5 <- LinearSVM(formula(Class~.), testdata, 1000, method="Pegasos")
# g6 <- LinearSVM(formula(Class~.), testdata, 1000, method="SGD")

test_that("Same result for SVM and Linear SVM.", {

  expect_equal(decisionvalues(g2,testdata),decisionvalues(g1,testdata),tolerance=1e-5)
  expect_equal(decisionvalues(g3,testdata),decisionvalues(g1,testdata),tolerance=1e-5)
  # expect_equal(decisionvalues(g4,testdata),decisionvalues(g1,testdata),tolerance=1e-5)
  # expect_equal(decisionvalues(g5,testdata),decisionvalues(g1,testdata),tolerance=1e-6)
  # expect_equal(decisionvalues(g6,testdata),decisionvalues(g1,testdata),tolerance=1e-6)
})


# test_that("Weights equal for BGD and Dual solution", {
#   expect_equivalent(g3@w, g4@w)
# })

test_that("Predictions the same for SVM and LinearSVM",{
  expect_equal(predict(g2,extra_testdata), predict(g1,extra_testdata))
  expect_equal(predict(g3,extra_testdata), predict(g1,extra_testdata))
  #expect_equal(predict(g4,extra_testdata), predict(g1,extra_testdata))
  # expect_equal(predict(g5,extra_testdata), predict(g1,extra_testdata))
  # expect_equal(predict(g6,extra_testdata), predict(g1,extra_testdata))
})

test_that("Loss functions return the same value for SVM and LinearSVM",{
  expect_equal(loss(g2,extra_testdata), loss(g1,extra_testdata),tolerance=1e-5)
  expect_equal(loss(g3,extra_testdata), loss(g1,extra_testdata),tolerance=1e-5)
  #expect_equal(loss(g4,extra_testdata), loss(g1,extra_testdata))
  # expect_equal(loss(g5,extra_testdata), loss(g1,extra_testdata))
  # expect_equal(loss(g6,extra_testdata), loss(g1,extra_testdata))
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
