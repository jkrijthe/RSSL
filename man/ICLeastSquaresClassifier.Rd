\name{ICLeastSquaresClassifier}
\alias{ICLeastSquaresClassifier}
\title{Implicitly Constained Least Squares Classifier}
\usage{
ICLeastSquaresClassifier(X, y, X_u=NULL, lambda1=0, lambda2=0, intercept=TRUE,scale=FALSE,method="default", ...)
}
\arguments{
  \item{X}{Design matrix, intercept term is added within
  the function}

  \item{y}{Vector or factor with class assignments}

  \item{X_u}{Design matrix of the unlabeled data, intercept
  term is added within the function}

  \item{lambda1}{Regularization parameter in the
  unlabeled+labeled data regularized least squares}

  \item{lambda2}{Regularization parameter in the labeled
  data only regularized least squares}

  \item{intercept}{TRUE if an intercept should be added to
  the model}

  \item{scale}{If TRUE, apply a z-transform to all
  observations in X and X_u before running the regression}

  \item{method}{Either "default" for solving using L-BFGS-B
  gradient descent or "QP" fpr a quadratic programming
  based solution}

  \item{...}{additional arguments}
}
\value{
S4 object of class ICLeastSquaresClassifier with the
following slots: \item{theta}{weight vector}
\item{classnames}{the names of the classes}
\item{modelform}{formula object of the model used in
regression} \item{scaling}{a scaling object containing the
paramters of the z-transforms applied to the data}
\item{optimization}{the object returned by the optim
function} \item{unlabels}{the labels assigned to the
unlabeled objects}
}
\description{
Implicitly constrained semisupervised learning with least
squares loss. Least squares regression is used treating
classes as targets (1 for one class, 2 for the other).
Implemented using matrix inversions, not the more stable
Singular Value Decomposition method. We find an
(fractional) labelling of the unlabeled objects which,
whose least squares regression solution minimizes the least
squares loss on the labeled training data only. This is
equivalent to finding a weighting of the unlabeled objects
belonging to either of the two classes.
}

