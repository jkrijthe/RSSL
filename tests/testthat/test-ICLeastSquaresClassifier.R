context("Implicitly Constrained Least Squares Classifier")

# Simple dataset used in the tests
data(testdata)
modelform <- formula(y~X1+X2)
classname<-all.vars(modelform)[1] 
D <- testdata$D
D_test <- testdata$D_test
X <- testdata$X
X_u <- testdata$X_u
y <- testdata$y
X_test <- testdata$X_test
y_test <- testdata$y_test

test_that("Formula and matrix formulation give same results",{
  g_matrix <- ICLeastSquaresClassifier(X,factor(y),X_u)
  g_model <- ICLeastSquaresClassifier(modelform, D)
  
  expect_that(1-mean(predict(g_matrix,X_test)==y_test),is_equivalent_to(1-mean(predict(g_model,D_test)==D_test[,classname]))) # Same classification error?
  expect_that(loss(g_matrix, X_test, y_test),is_equivalent_to(loss(g_model, D_test)))  
  expect_equal(g_matrix@classnames,g_model@classnames) # Class names the same?
})

test_that("Different settings return the same loss",{
  g_1<-ICLeastSquaresClassifier(X,y,X_u,intercept=TRUE,scale=TRUE)
  s1 <- mean(loss(g_1,X_test,y_test))
  g_2<-ICLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE,scale=TRUE)
  expect_equal(s1,mean(loss(g_2,X_test,y_test)),tolerance=10e-6)
  g_3<-ICLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE,scale=TRUE,y_scale=TRUE)
  expect_equal(s1,mean(loss(g_3,X_test,y_test)),tolerance=10e-6)
  g_4<-ICLeastSquaresClassifier(X,y,X_u,intercept=TRUE,scale=TRUE,x_center=TRUE,y_scale=TRUE)
  expect_equal(s1,mean(loss(g_4,X_test,y_test)),tolerance=10e-6)
})

test_that("Multi class gives an output", {
  dmat<-model.matrix(Species~.-1,iris[1:150,])
  tvec<-droplevels(iris$Species[1:150])
  set.seed(42)
  problem<-split_dataset_ssl(dmat,tvec,frac_train=0.5,frac_ssl=0.5)
  g_sup <- ICLeastSquaresClassifier(problem$X,problem$y,problem$X_u,
                                    projection="supervised",method="QP",eps=10e-10)
  g_semi <- ICLeastSquaresClassifier(problem$X,problem$y,problem$X_u,
                                    projection="semisupervised",method="QP",eps=10e-10)
  g_euc <- ICLeastSquaresClassifier(problem$X,problem$y,problem$X_u,
                                    projection="euclidean",method="QP",eps=10e-10)
  
  # Output has three different classes
  expect_equal(length(levels(predict(g_sup,problem$X_test))),3)
  expect_equal(length(levels(predict(g_semi,problem$X_test))),3)
  expect_equal(length(levels(predict(g_euc,problem$X_test))),3)
  
  # Correct number of predictions on example dataset
  expect_equal(length(predict(g_sup,problem$X_test)),75)
  expect_equal(length(predict(g_semi,problem$X_test)),75)
  expect_equal(length(predict(g_euc,problem$X_test)),75)
})

test_that("Different methods", {

  g_sup <- ICLeastSquaresClassifier(X,y,X_u,
                                    projection="supervised",method="QP",eps=10e-10)
  g_sup_lbfgs <- ICLeastSquaresClassifier(X,y,X_u,
                                    projection="supervised",method="LBFGS",eps=10e-10)
  
  expect_equal(predict(g_sup,X_test),predict(g_sup_lbfgs,X_test))
  
  g_semi <- ICLeastSquaresClassifier(X,y,X_u,
                                     projection="semisupervised",method="QP",eps=10e-10)
  g_semi_lbfgs <- ICLeastSquaresClassifier(X,y,X_u,
                                    projection="semisupervised",method="LBFGS",eps=10e-10)
  g_semiold <- ICLeastSquaresClassifier(X,y,X_u,
                                           projection="semisupervisedold",method="LBFGS",eps=10e-10)
  expect_equal(loss(g_semi,X_test,y_test),loss(g_semi_lbfgs,X_test,y_test),tolerance=10e-6)
  expect_equal(loss(g_semi,X_test,y_test),loss(g_semiold,X_test,y_test),tolerance=10e-5)
  
})
