% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LaplacianSVM.R
\name{LaplacianSVM}
\alias{LaplacianSVM}
\title{Laplacian SVM classifier}
\usage{
LaplacianSVM(X, y, X_u = NULL, lambda = 1, gamma = 1, scale = TRUE,
  kernel = vanilladot(), adjacency_distance = "euclidean",
  adjacency_k = 6, normalized_laplacian = FALSE, eps = 1e-09)
}
\arguments{
\item{X}{matrix; Design matrix for labeled data}

\item{y}{factor or integer vector; Label vector}

\item{X_u}{matrix; Design matrix for unlabeled data}

\item{lambda}{numeric; L2 regularization parameter}

\item{gamma}{numeric; Weight of the unlabeled data}

\item{scale}{logical; Should the features be normalized? (default: FALSE)}

\item{kernel}{kernlab::kernel to use}

\item{adjacency_distance}{character; distance metric used to construct adjacency graph from the dist function. Default: "euclidean"}

\item{adjacency_k}{integer; Number of of neighbours used to construct adjacency graph.}

\item{normalized_laplacian}{logical; If TRUE use the normalized Laplacian, otherwise, the Laplacian is used}

\item{eps}{numeric; Small value to ensure positive definiteness of the matrix in the QP formulation}
}
\value{
S4 object of type LaplacianSVM
}
\description{
Manifold regularization applied to the support vector machine as proposed in Belkin et al. (2006). As an adjacency matrix, we use the k nearest neighbour graph based on a chosen distance (default: euclidean).
}
\examples{
library(RSSL)
library(ggplot2)
library(dplyr)

## Example 1: Half moons

# Generate a dataset
set.seed(2)
df_orig <- generateCrescentMoon(100,sigma = 0.3) 
df <- df_orig \%>\% 
  add_missinglabels_mar(Class~.,0.98)

lambda <- 0.001
C <- 1/(lambda*2*sum(!is.na(df$Class)))
gamma <- 10000
rbf_param <- 0.125

# Train classifiers
class_sup <- SVM(
  Class~.,df,
  kernel=kernlab::rbfdot(rbf_param),
  C=C,scale=FALSE)

class_lap <- LaplacianSVM(
  Class~.,df,
  kernel=kernlab::rbfdot(rbf_param),
  lambda=lambda,gamma=gamma,
  normalized_laplacian = TRUE,
  scale=FALSE)

classifiers <- list("Lap"=class_lap,"Sup"=class_sup)

# This takes a little longer to run:
# class_tsvm <- TSVM(
#   Class~.,df,
#   kernel=kernlab::rbfdot(rbf_param),
#   C=C,Cstar=10,s=-0.8,
#   scale=FALSE,balancing_constraint=TRUE)
# classifiers <- list("Lap"=class_lap,"Sup"=class_sup,"TSVM"=class_tsvm)

# Plot classifiers (Can take a couple of seconds)
\dontrun{
df \%>\% 
  ggplot(aes(x=X1,y=X2,color=Class)) +
  geom_point() +
  coord_equal() +
  stat_classifier(aes(linetype=..classifier..),
                  classifiers = classifiers ,
                  color="black")
}
  
# Calculate the loss
lapply(classifiers,function(c) mean(loss(c,df_orig)))

## Example 2: Two circles
set.seed(3)
df_orig <- generateTwoCircles(1000,noise=0.05)
df <- df_orig \%>\% 
  add_missinglabels_mar(Class~.,0.994)

lambda <- 0.000001
C <- 1/(lambda*2*sum(!is.na(df$Class)))
gamma <- 100
rbf_param <- 0.1

# Train classifiers (Takes a couple of seconds)
\dontrun{
class_sup <- SVM(
  Class~.,df,
  kernel=kernlab::rbfdot(rbf_param),
  C=C,scale=FALSE)

class_lap <- LaplacianSVM(
  Class~.,df,
  kernel=kernlab::rbfdot(rbf_param),
  adjacency_k=50, lambda=lambda,gamma=gamma,
  normalized_laplacian = TRUE,
  scale=FALSE)


classifiers <- list("Lap"=class_lap,"Sup"=class_sup)
}

# Plot classifiers (Can take a couple of seconds)
\dontrun{
df \%>\% 
  ggplot(aes(x=X1,y=X2,color=Class,size=Class)) +
  scale_size_manual(values=c("1"=3,"2"=3),na.value=1) +
  geom_point() +
  coord_equal() +
  stat_classifier(aes(linetype=..classifier..),
                  classifiers = classifiers ,
                  color="black",size=1)
}
}
\references{
Belkin, M., Niyogi, P. & Sindhwani, V., 2006. Manifold regularization: A geometric framework for learning from labeled and unlabeled examples. Journal of Machine Learning Research, 7, pp.2399-2434.
}
\seealso{
Other RSSL classifiers: 
\code{\link{EMLeastSquaresClassifier}},
\code{\link{EMLinearDiscriminantClassifier}},
\code{\link{GRFClassifier}},
\code{\link{ICLeastSquaresClassifier}},
\code{\link{ICLinearDiscriminantClassifier}},
\code{\link{KernelLeastSquaresClassifier}},
\code{\link{LaplacianKernelLeastSquaresClassifier}()},
\code{\link{LeastSquaresClassifier}},
\code{\link{LinearDiscriminantClassifier}},
\code{\link{LinearSVM}},
\code{\link{LinearTSVM}()},
\code{\link{LogisticLossClassifier}},
\code{\link{LogisticRegression}},
\code{\link{MCLinearDiscriminantClassifier}},
\code{\link{MCNearestMeanClassifier}},
\code{\link{MCPLDA}},
\code{\link{MajorityClassClassifier}},
\code{\link{NearestMeanClassifier}},
\code{\link{QuadraticDiscriminantClassifier}},
\code{\link{S4VM}},
\code{\link{SVM}},
\code{\link{SelfLearning}},
\code{\link{TSVM}},
\code{\link{USMLeastSquaresClassifier}},
\code{\link{WellSVM}},
\code{\link{svmlin}()}
}
\concept{RSSL classifiers}
