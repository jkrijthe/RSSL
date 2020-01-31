context("Learning Curves")

test_that("LearningCurveSSL does not return error",{
  X <- model.matrix(Species~.-1,data=iris)
  y <- iris$Species
  
  classifiers <- list("LS"=function(X,y,X_u,y_u) {LeastSquaresClassifier(X,y,lambda=0)}, 
                      "RLS"=function(X,y,X_u,y_u) {LeastSquaresClassifier(X,y,lambda=10)})
  measures <- list("Accuracy" =  measure_accuracy,
                   "Loss" = measure_losstest,
                   "Loss labeled" = measure_losslab,
                   "Loss Lab+Unlab" = measure_losstrain
  )
  lc <- LearningCurveSSL(X,y,classifiers=classifiers,
                           measures=measures,n_l=10,repeats=3)
  expect_silent(plot(lc))
  expect_output(print(lc))
  
  lc1 <- LearningCurveSSL(list("D1"=X,"D2"=X),list("D1"=y,"D2"=y),classifiers=classifiers,
                            measures=measures,n_l="enough",repeats=3,pre_pca=TRUE)
  
  lc2 <- LearningCurveSSL(list("D1"=formula(Species~.)),list("D1"=na.omit(iris)),
                            classifiers=classifiers,
                            measures=measures,n_l=10,repeats=3,type="fraction")
  
})
