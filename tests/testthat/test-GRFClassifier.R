context("Gaussian Random Field Classifier")

library(kernlab)

data <- generateTwoCircles(400,0.1)
data[-sample(1:nrow(data),10),]$Class <- NA
y <- na.omit(data$Class)
X <- as.matrix(data[!is.na(data$Class),1:2])
X_u <- as.matrix(data[is.na(data$Class),1:2])
Xin <- rbind(X,X_u)

Y <- model.matrix(~y-1,data.frame(y))[1:2,1:2,drop=FALSE]
W <- exp(-as.matrix(dist(Xin))^2/0.1)

test_that("Setting kernel directly gives same responsibilities", {
  expect_equivalent(GRFClassifier(X, y, X_u)@responsibilities,
                    GRFClassifier(X, y, X_u, adjacency_kernel = rbfdot(10))@responsibilities)
})


test_that("harmonic_function does not give error",{
  RSSL:::harmonic_function(W,Y)
  expect_equal(RSSL:::harmonic_function(W,Y)$fu,
               RSSL:::harmonic_function_cpp(W,Y),
               tolerance=0.01,check.attributes=FALSE)
})

# library(microbenchmark)
# microbenchmark(RSSL:::harmonic_function(W,Y),RSSL:::harmonic_function_cpp(W,Y))
