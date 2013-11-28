\name{TSVM}
\alias{TSVM}
\title{Transductive SVM classifier}
\usage{
  TSVM(X, y, X_u, Clabeled = 0, Cunlabeled = 0,
    intercept = TRUE, scale = TRUE, ...)
}
\arguments{
  \item{X}{Design matrix, intercept term is added within
  the function}

  \item{y}{Vector or factor with class assignments}

  \item{X_u}{Design matrix of the unlabeled data, intercept
  term is added within the function}

  \item{Clabeled}{<todo>}

  \item{Cunlabeled}{<todo>}

  \item{intercept}{TRUE if an intercept should be added to
  the model}

  \item{scale}{If TRUE, apply a z-transform to all
  observations in X and X_u before running the regression}

  \item{...}{additional arguments}
}
\value{
  S4 object of class TSVM with the following slots:
  \item{theta}{weight vector} \item{classnames}{the names
  of the classes} \item{modelform}{formula object of the
  model used in regression} \item{scaling}{a scaling object
  containing the paramters of the z-transforms applied to
  the data} \item{optimization}{the object returned by the
  optim function} \item{unlabels}{the labels assigned to
  the unlabeled objects}
}
\description{
  Transductive SVM classifier
}

