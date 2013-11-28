\name{UCLeastSquaresClassifier}
\alias{UCLeastSquaresClassifier}
\title{UCLeastSquaresClassifier}
\usage{
  UCLeastSquaresClassifier(X, y, X_u, lambda = 0,
    intercept = TRUE, scale = FALSE, y_scale = FALSE,
    x_center = TRUE, ...)
}
\arguments{
  \item{X}{Design matrix}

  \item{y}{labels}

  \item{X_u}{Design matrix unlabeled data}
}
\description{
  UCLeastSquaresClassifier

  See for instance \cite{Shaffer1991}. Use all data to
  estimate E(X'X)
}

