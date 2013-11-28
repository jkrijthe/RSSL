\name{SelfLearning}
\alias{SelfLearning}
\title{Self Learning Semi-supervised Learning}
\usage{
  SelfLearning(X, y, X_u, method, prob = FALSE,
    cautious = FALSE, ...)
}
\arguments{
  \item{X}{Design matrix, intercept term is added within
  the function}

  \item{y}{Vector or factor with class assignments}

  \item{X_u}{Design matrix unlabeled data}

  \item{method}{Supervised classifier to use. Any function
  that accepts as its first argument a design matrix X and
  as its second argument a vector of labels y.}

  \item{prob}{If TRUE, run algorithm with soft labels,
  instead of hard labels}

  \item{...}{additional arguments to be passed to method}
}
\description{
  Use self learning to turn any supervised classifier into
  a semi-supervised method by iteratively labeling the
  unlabeled objects and added confident predictions to the
  set of labeled objects
}

