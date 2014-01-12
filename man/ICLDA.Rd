\name{ICLDA}
\alias{ICLDA}
\title{Implicitly Constrained Semi-Supervised Linear Discriminant Classifier}
\usage{
ICLinearDiscriminantClassifier(X, y, X_u,prior=NULL, scale=FALSE,init=NULL, sup_prior=FALSE, factr=1e7,  ...)
}
\arguments{
  \item{X}{design matrix of the labeled objects}

  \item{y}{vector with labels}

  \item{X_u}{design matrix of the labeled objects}

  \item{prior}{<what param does>}

  \item{scale}{Should the features be normalized? (default:
  FALSE)}

  \item{init}{not currently used}

  \item{sup_prior}{use the prior estimates based only on
  the labeled data, not the imputed labels (default:
  FALSE)}

  \item{factr}{change the precision requirement passed to
  the optim function (default: 1e7)}

  \item{...}{Additional Parameters, Not used}
}
\description{
Semi-supervised version of Linear Discriminant Analysis
using implicit constraints. This method finds the labeling
of the unlabeled objects, whose resulting LDA solution
gives the highest log-likelihood when evaluated on the
labeled objects only.
}

