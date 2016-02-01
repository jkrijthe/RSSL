context("Crossvalidation")

test_that("Example Runs",{
  X <- model.matrix(Species~.-1,data=iris)
  y <- iris$Species
  
  classifiers <- list("LS"=function(X,y,X_u,y_u) {LeastSquaresClassifier(X,y,lambda=0)}, 
                      "RLS"=function(X,y,X_u,y_u) {LeastSquaresClassifier(X,y,lambda=10)})
  measures <- list("Accuracy" =  measure_accuracy,
                   "Loss" = measure_losstest,
                   "Loss labeled" = measure_losslab,
                   "Loss Lab+Unlab" = measure_losstrain
  )
  lc <- CrossValidationSSL(X,y,classifiers=classifiers,
                           measures=measures,n_l=10,repeats=3)
  print(lc)
  plot(lc)
})