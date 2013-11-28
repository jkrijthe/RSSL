\docType{methods}
\name{loss}
\alias{loss}
\alias{loss,LeastSquaresClassifier-method}
\alias{loss,LinearSVM-method}
\alias{loss,LogisticLossClassifier-method}
\alias{loss,LogisticRegression-method}
\alias{loss,NormalBasedClassifier-method}
\alias{loss,SGDSVM-method}
\alias{loss,SelfLearning-method}
\title{Loss of a classifier or regression function}
\arguments{
  \item{object}{object of class NormalBasedClassifier}

  \item{newdata}{a matrix of data.frame, depending on what
  was used to train the classifier, with new objects to be
  classified}

  \item{y}{if newdata is a matrix, y should contain the
  correct labels of the observations in newdata}

  \item{object}{Object of class LeastSquaresClassifier}

  \item{X}{Design matrix of the test data, intercept term
  is added within the function}

  \item{y}{Vector with true classes of the test data}

  \item{newdata}{data.frame object with test data}

  \item{object}{Object of class LogisticLossClassifier}

  \item{X}{Design matrix of the test data, intercept term
  is added within the function}

  \item{y}{Vector with true classes of the test data}

  \item{newdata}{data.frame object with test data}

  \item{object}{Object of class LeastSquaresClassifier}

  \item{X}{Design matrix of the test data, intercept term
  is added within the function}

  \item{y}{Vector with true classes of the test data}

  \item{newdata}{data.frame object with test data}

  \item{object}{Object of class LinearSVM}

  \item{X}{Design matrix of the test data, intercept term
  is added within the function}

  \item{y}{Vector with true classes of the test data}

  \item{newdata}{data.frame object with test data}

  \item{object}{Object of class SelfLearning}

  \item{X}{Design matrix of the test data, intercept term
  is added within the function}

  \item{y}{Vector with true classes of the test data}

  \item{newdata}{data.frame object with test data}
}
\value{
  The total minus log likelihood on the dataset

  numeric of the total loss on the test data

  numeric of the total loss on the test data

  numeric of the total loss on the test data

  numeric of the total loss on the test data

  numeric of the total loss on the test data
}
\description{
  Loss of a classifier or regression function

  Return the MINUS log likelihood on the given dataset

  Loss method for LogisticRegression MINUS Log likelihood
  on a new data set

  Loss on new objects of a trained least squares classifier

  Calculate the logistic loss of a classifier on a given
  dataset

  Hinge loss on new objects of a trained SGDSVM

  Hinge loss on new objects of a trained LinearSVM

  Loss method for SelfLearning Classifier The loss method
  delegates prediction to the specific model object
}

