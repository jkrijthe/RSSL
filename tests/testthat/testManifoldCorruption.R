context("Manifold Corruption")

generatePerpendicular<-function(n=100) {
  sigma1 <- matrix(c(0.05,0,0,2),2,2)
  sigma2 <- matrix(c(2,0,0,0.05),2,2)
  mean1 <- matrix(c(0,-2),1,2)
  mean2 <- matrix(c(0,2),1,2)
  X1 <- sweep(matrix(rnorm(n*2),n,2) %*% chol(sigma1), 2, mean1, "+")
  X2 <- sweep(matrix(rnorm(n*2),n,2) %*% chol(sigma2), 2, mean2, "+")
  data <- data.frame(Class=factor(c(rep(1,n),rep(2,n))),rbind(X1,X2))
  data[sample(1:nrow(data)),]
}
# data <- generatePerpendicular(1000)
# qplot(x=X1,y=X2,color=Class,data=data,alpha=0.2)+coord_fixed()

# reps <- 50
# results <- matrix(NA,reps,3)
# for (i in 1:reps) {
#   lab_data <- generatePerpendicular(1)
#   dmat <- as.matrix(lab_data[2:3])
#   tvec <- lab_data$Class
#   dmatU <- as.matrix(generatePerpendicular(500)[,2:3])
#   
#   t_mani <- ManifoldCorruptionLeastSquaresClassifier(X=dmat, y=tvec, X_u=dmatU, adjacency_kernel = rbfdot(1000000),k=150,samples=200,sigma=1000,scale=FALSE)
#   t_sup <- LeastSquaresClassifier(X=dmat,y=tvec)
#   t_reg <- LeastSquaresClassifier(X=dmat,y=tvec,lambda=10000000)
#   testset <- generatePerpendicular(10000)
#   dtest <- as.matrix(testset[,2:3])
#   ttest <- testset$Class
#   
#   results[i,1] <- mean(predict(t_sup,dtest)==ttest)
#   results[i,2] <- mean(predict(t_mani,dtest)==ttest)
#   results[i,3] <- mean(predict(t_reg,dtest)==ttest)
# }
# 
# library(dplyr)
# library(magrittr)
# library(tidyr)
# p <- data.frame(results) %>%
#   set_colnames(c("Supervised","Corrupted","Regularized")) %>%
#   add_rownames %>% 
#   gather(Classifier,Error,-rowname) %>%
#   ggplot(aes(x=Classifier,y=Error,group=rowname)) +
#   geom_line(alpha=0.2)
# print(p)
# Findings:
# sigma seems to have little effect


# How to determine the distance over the manifold
# See manifold paper. Calculate distance one at a time?
#Other approach: estimate mahanalobis distance locally and apply corrupti
# x <- (rnorm(100))^3
# y <- x+rnorm(100)
# plot(x^(1/3),y)
