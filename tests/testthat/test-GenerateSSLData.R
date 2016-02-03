context("Generate SSL Data")

test_that("generateFourClusters does not return error",{
  library(ggplot2)
  data <- generateFourClusters()
  ggplot(data,aes(x=X1,y=X2,color=Class)) + geom_point()
})

test_that("generateCrescentMoon does not return error",{
  library(ggplot2)
  data <- generateCrescentMoon()
  ggplot(data,aes(x=X1,y=X2,color=Class)) + geom_point()
})

test_that("generate2ClassGaussian does not return error",{
  library(ggplot2)
  data <- generate2ClassGaussian()
  ggplot(data,aes(x=X1,y=X2,color=Class)) + geom_point()
})

test_that("generateTwoCircles does not return error",{
  library(ggplot2)
  data <- generateTwoCircles()
  ggplot(data,aes(x=X1,y=X2,color=Class)) + geom_point()
})
