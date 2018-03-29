context("Generate SSL Data")

test_that("generateFourClusters does not return error",{
  library(ggplot2)
  data <- generateFourClusters()
  expect_silent(ggplot(data,aes(x=X1,y=X2,color=Class)) + geom_point())
})

test_that("generateCrescentMoon does not return error",{
  library(ggplot2)
  data <- generateCrescentMoon()
  expect_silent(ggplot(data,aes(x=X1,y=X2,color=Class)) + geom_point())
})

test_that("generate2ClassGaussian does not return error",{
  library(ggplot2)
  data <- generate2ClassGaussian()
  expect_silent(ggplot(data,aes(x=X1,y=X2,color=Class)) + geom_point())
})

test_that("generateTwoCircles does not return error",{
  library(ggplot2)
  data <- generateTwoCircles()
  expect_silent(ggplot(data,aes(x=X1,y=X2,color=Class)) + geom_point())
})
