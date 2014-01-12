\name{MCLinearDiscriminantClassifier}
\alias{MCLinearDiscriminantClassifier}
\title{Moment constrained semi-supervised linear discriminant analysis}
\usage{
MCLinearDiscriminantClassifier(X, y, X_u, method="closedform",prior=NULL, scale=FALSE,  ...)
}
\arguments{
  \item{X}{Matrix with n objects for m features}

  \item{y}{factor with labels for n objects}

  \item{X_u}{Matrix with n objects for m features for
  unlabeled data}

  \item{method}{either "ml" or "closedform"}

  \item{prior}{Matrix (k by 1) of class prior
  probabilities. If NULL, estimated from data}

  \item{scale}{logical Whether scaling should be apply}

  \item{...}{Additional parameters, not used}
}
\description{
Using an update of the estimated means and covariance
proposed by \cite{Loog}. Using the method="closedform"
option, uses the ad hoc parameter update proposed in
\cite{Loog}, while for method="ml" the better (but slower)
constrained maximum likelihood estimation proposed in
\cite{Loog} is used.
}

