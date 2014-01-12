\name{LogisticLossClassifier}
\alias{LogisticLossClassifier}
\title{Logistic Loss Classifier}
\usage{
LogisticLossClassifier(X, y, lambda=0, intercept=TRUE, scale=FALSE, init=NA, ...)
}
\arguments{
  \item{X}{Design matrix, intercept term is added within
  the function}

  \item{y}{Vector with class assignments}

  \item{lambda}{Regularization parameter used for l2
  regularization}

  \item{intercept}{TRUE if an intercept should be added to
  the model}

  \item{scale}{If TRUE, apply a z-transform to all
  observations in X and X_u before running the regression}

  \item{init}{Starting parameter vector for gradient
  descent}

  \item{...}{additional arguments}
}
\value{
S4 object with the following slots \item{w}{the weight
vector of the linear classification rule}
\item{classnames}{vector with names of the classes}
}
\description{
Find the linear classifier which minimizing the logistic
loss on the training set
}

